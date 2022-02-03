{-  This file is part of emu.
    emu is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.
    emu is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    You should have received a copy of the GNU General Public License
    along with emu. If not, see <https://www.gnu.org/licenses/>.  -}
                                                                             
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}

module Semantics.Check
    (

     Environment (..),

     stmtReturns,
     check

    ) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Except

import qualified Data.ByteString as B
import Data.Either
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text, pack)
import Data.Word
import Debug.Trace

import Foreign (Ptr, poke, peek)
import Foreign.Ptr
import Foreign.Marshal.Alloc

import Parser.AST (Type (..),
                   Modifier (..),
                   FixedPointVal (..),
                   FloatingPointVal (..))
import qualified Parser.AST as A
    
import Semantics.Error
import Semantics.Marshal
import Semantics.SAST

foreign import capi "lib.h cxx_comptime_eval" c_comptime_eval :: Ptr SAST -> Ptr DecoratedType -> IO (Ptr ComptimeValue)
foreign import capi "lib.h destruct_comptime_value" c_destruct_comptime_value :: Ptr ComptimeValue -> IO ()

type Variables = M.Map (Text, VarKind) VarBinding
type Functions = M.Map Text Function
type Structures = M.Map Text Structure

data Environment = Environment { vars :: Variables,
                                 funcs :: Functions,
                                 structs :: Structures,
                                 curFuncSignature :: Maybe FunctionSignature } deriving Show

type Semantics = ExceptT SemanticsError (StateT Environment IO)

uniform :: Eq a => [a] -> Bool
uniform [] = True
uniform (x:xs) = all (== x) xs

implicitlyConvert :: Expression -> DecoratedType -> Either (Expression, DecoratedType) Expression
implicitlyConvert e t = if checkImplicitCast (typeOf e) t then if typeOf e == t then Right e else Right (Cast e t) else Left (e, t)

checkExplicitCast :: DecoratedType -> DecoratedType -> Bool
checkExplicitCast t1 t2 = checkExplicitCastHelper t1 t2 || checkImplicitCast t1 t2
    where checkExplicitCastHelper (PureType U8) (DerefType _) = True
          checkExplicitCastHelper (PureType U16) (DerefType _) = True
          checkExplicitCastHelper (PureType U32) (DerefType _) = True
          checkExplicitCastHelper (PureType U64) (DerefType _) = True
          checkExplicitCastHelper (PureType I8) (DerefType _) = True
          checkExplicitCastHelper (PureType I16) (DerefType _) = True
          checkExplicitCastHelper (PureType I32) (DerefType _) = True
          checkExplicitCastHelper (PureType I64) (DerefType _) = True
          checkExplicitCastHelper (DerefType _) (PureType U64) = True
          checkExplicitCastHelper (PureType U8) (PureType Void) = False
          checkExplicitCastHelper (PureType U16) (PureType Void) = False
          checkExplicitCastHelper (PureType U32) (PureType Void) = False
          checkExplicitCastHelper (PureType U64) (PureType Void) = False
          checkExplicitCastHelper (PureType I8) (PureType Void) = False
          checkExplicitCastHelper (PureType I16) (PureType Void) = False
          checkExplicitCastHelper (PureType I32) (PureType Void) = False
          checkExplicitCastHelper (PureType I64) (PureType Void) = False
          checkExplicitCastHelper (PureType F32) (PureType Void) = False
          checkExplicitCastHelper (PureType F64) (PureType Void) = False
          checkExplicitCastHelper (PureType U8) (PureType (StructType _)) = False
          checkExplicitCastHelper (PureType U16) (PureType (StructType _)) = False
          checkExplicitCastHelper (PureType U32) (PureType (StructType _)) = False
          checkExplicitCastHelper (PureType U64) (PureType (StructType _)) = False
          checkExplicitCastHelper (PureType I8) (PureType (StructType _)) = False
          checkExplicitCastHelper (PureType I16) (PureType (StructType _)) = False
          checkExplicitCastHelper (PureType I32) (PureType (StructType _)) = False
          checkExplicitCastHelper (PureType I64) (PureType (StructType _)) = False
          checkExplicitCastHelper (PureType F32) (PureType (StructType _)) = False
          checkExplicitCastHelper (PureType F64) (PureType (StructType _)) = False
          checkExplicitCastHelper (PureType U8) (PureType _) = True
          checkExplicitCastHelper (PureType U16) (PureType _) = True
          checkExplicitCastHelper (PureType U32) (PureType _) = True
          checkExplicitCastHelper (PureType U64) (PureType _) = True
          checkExplicitCastHelper (PureType I8) (PureType _) = True
          checkExplicitCastHelper (PureType I16) (PureType _) = True
          checkExplicitCastHelper (PureType I32) (PureType _) = True
          checkExplicitCastHelper (PureType I64) (PureType _) = True
          checkExplicitCastHelper (PureType F32) (PureType _) = True
          checkExplicitCastHelper (PureType F64) (PureType _) = True
          checkExplicitCastHelper (DerefType _) (DerefType _) = True
          checkExplicitCastHelper (ArrayType tt1 s1) (ArrayType tt2 s2) = s1 == s2 && checkExplicitCast tt1 tt2
          checkExplicitCastHelper _ _ = False

checkImplicitCast :: DecoratedType -> DecoratedType -> Bool
checkImplicitCast t1 t2 = (t1 == t2) || checkImplicitCastHelper t1 t2
    where checkImplicitCastHelper (PureType U8) (PureType U16) = True
          checkImplicitCastHelper (PureType U8) (PureType U32) = True
          checkImplicitCastHelper (PureType U8) (PureType U64) = True
          checkImplicitCastHelper (PureType U16) (PureType U32) = True
          checkImplicitCastHelper (PureType U16) (PureType U64) = True
          checkImplicitCastHelper (PureType U32) (PureType U64) = True
          checkImplicitCastHelper (PureType U8) (PureType I16) = True
          checkImplicitCastHelper (PureType U8) (PureType I32) = True
          checkImplicitCastHelper (PureType U8) (PureType I64) = True
          checkImplicitCastHelper (PureType U16) (PureType I32) = True
          checkImplicitCastHelper (PureType U16) (PureType I64) = True
          checkImplicitCastHelper (PureType U32) (PureType I64) = True
          checkImplicitCastHelper (PureType U8) (PureType F32) = True
          checkImplicitCastHelper (PureType U8) (PureType F64) = True
          checkImplicitCastHelper (PureType U16) (PureType F32) = True
          checkImplicitCastHelper (PureType U16) (PureType F64) = True
          checkImplicitCastHelper (PureType U32) (PureType F32) = True
          checkImplicitCastHelper (PureType U32) (PureType F64) = True
          checkImplicitCastHelper (PureType U64) (PureType F32) = True
          checkImplicitCastHelper (PureType U64) (PureType F64) = True
          checkImplicitCastHelper (PureType I8) (PureType F32) = True
          checkImplicitCastHelper (PureType I8) (PureType F64) = True
          checkImplicitCastHelper (PureType I16) (PureType F32) = True
          checkImplicitCastHelper (PureType I16) (PureType F64) = True
          checkImplicitCastHelper (PureType I32) (PureType F32) = True
          checkImplicitCastHelper (PureType I32) (PureType F64) = True
          checkImplicitCastHelper (PureType I64) (PureType F32) = True
          checkImplicitCastHelper (PureType I64) (PureType F64) = True
          checkImplicitCastHelper (PureType F32) (PureType F64) = True
          checkImplicitCastHelper (PureType I8) (PureType I16) = True
          checkImplicitCastHelper (PureType I8) (PureType I32) = True
          checkImplicitCastHelper (PureType I8) (PureType I64) = True
          checkImplicitCastHelper (PureType I16) (PureType I32) = True
          checkImplicitCastHelper (PureType I16) (PureType I64) = True
          checkImplicitCastHelper (PureType I32) (PureType I64) = True
          checkImplicitCastHelper _ _ = False

check :: A.AST -> Semantics SAST
check (A.AST decls) = SAST <$> mapM checkDecl decls

checkDecl :: A.Declaration -> Semantics Declaration
checkDecl ((l, sc, ec), d) = checked
    where checked = case d of
                      A.StructDecl mods name fields -> do
                             checkIfInFunctionAlready <- gets curFuncSignature
                             when (isJust checkIfInFunctionAlready) $ throwError $ SemanticsError l sc ec NestedDeclarationError
                             when (A.Pure `elem` mods) $ throwError $ SemanticsError l sc ec $ InvalidModifier A.Pure
                             when (A.Const `elem` mods) $ throwError $ SemanticsError l sc ec $ InvalidModifier A.Const
                             when (A.Inline `elem` mods) $ throwError $ SemanticsError l sc ec $ InvalidModifier A.Inline
                             when (A.Register `elem` mods) $ throwError $ SemanticsError l sc ec $ InvalidModifier A.Register
                             when (A.Restrict `elem` mods) $ throwError $ SemanticsError l sc ec $ InvalidModifier A.Restrict
                             boundVars <- gets vars
                             boundFuncs <- gets funcs
                             boundStructs <- gets structs
                             let varLookup = M.lookup (name, Local) boundVars <|> M.lookup (name, Formal) boundVars <|> M.lookup (name, Global) boundVars
                             let funcLookup = M.lookup name boundFuncs
                             let structLookup = M.lookup name boundStructs
                             when (isJust varLookup || isJust funcLookup || isJust structLookup) $ throwError $ SemanticsError l sc ec $ DuplicateDeclaration name
                             sfields <- checkDecoratedIdentifiers (l, sc, ec) fields
                             let struct = Structure mods name sfields
                             modify $ \env -> env { structs = M.insert name struct boundStructs }
                             return $ StructDecl $ struct
                      A.FuncDecl mods name args retType body -> do
                             checkIfInFunctionAlready <- gets curFuncSignature
                             when (isJust checkIfInFunctionAlready) $ throwError $ SemanticsError l sc ec NestedDeclarationError
                             when (A.Const `elem` mods) $ throwError $ SemanticsError l sc ec $ InvalidModifier A.Const
                             when (A.Register `elem` mods) $ throwError $ SemanticsError l sc ec $ InvalidModifier A.Register
                             when (A.Restrict `elem` mods) $ throwError $ SemanticsError l sc ec $ InvalidModifier A.Restrict
                             boundVars <- gets vars
                             boundFuncs <- gets funcs
                             boundStructs <- gets structs
                             let varLookup = M.lookup (name, Local) boundVars <|> M.lookup (name, Formal) boundVars <|> M.lookup (name, Global) boundVars
                             let funcLookup = M.lookup name boundFuncs
                             let structLookup = M.lookup name boundStructs
                             when (isJust varLookup || isJust funcLookup || isJust structLookup) $ throwError $ SemanticsError l sc ec $ DuplicateDeclaration name
                             prevEnv <- get
                             sargs <- checkDecoratedIdentifiersAndNames (l, sc, ec) args
                             mapM (\decIden@(DecoratedIdentifier _ varName varT) -> if isTypeVoid varT
                                                                                    then throwError $ SemanticsError l sc ec $ VoidVarDeclaration varName
                                                                                    else modify $ \env -> env { vars = M.insert (varName, Formal) (VarBinding decIden Undefined) (vars env) }) sargs
                             sretType <- checkDecoratedType retType
                             let sig = FunctionSignature mods name sargs sretType
                             modify $ \env -> env { curFuncSignature = Just sig }
                             sbody <- checkStmt body
                             termination <- stmtReturns (l, sc, ec) sbody
                             unless (termination == Just sretType || isTypeVoid sretType) $ throwError $ SemanticsError l sc ec FunctionNotReturning
                             let func = Function sig (if isTypeVoid sretType && termination == Nothing then Block [StatementDecl sbody, StatementDecl $ ReturnStatement Undefined] else sbody)
                             modify $ \_ -> prevEnv { funcs = M.insert name func boundFuncs }
                             return $ FuncDecl $ func
                      A.VarDecl (A.DecoratedIdentifier mods name t) init -> do
                             when (A.Pure `elem` mods) $ throwError $ SemanticsError l sc ec $ InvalidModifier A.Pure
                             when (A.Inline `elem` mods) $ throwError $ SemanticsError l sc ec $ InvalidModifier A.Inline
                             boundVars <- gets vars
                             boundFuncs <- gets funcs
                             boundStructs <- gets structs
                             let varLookup = M.lookup (name, Local) boundVars <|> M.lookup (name, Formal) boundVars <|> M.lookup (name, Global) boundVars
                             let funcLookup = M.lookup name boundFuncs
                             let structLookup = M.lookup name boundStructs
                             when (isJust varLookup || isJust funcLookup || isJust structLookup) $ throwError $ SemanticsError l sc ec $ DuplicateDeclaration name
                             prevEnv <- get
                             st <- checkDecoratedType t
                             when (isTypeVoid st) $ throwError $ SemanticsError l sc ec $ VoidVarDeclaration name
                             sinit <- checkExpr init
                             when (not ((typeOf sinit) == PureType Void) && (not $ checkImplicitCast (typeOf sinit) st)) $ throwError $ SemanticsError l sc ec $ ImplicitCastError (typeOf sinit) st
                             let varBind = VarBinding (DecoratedIdentifier mods name st) sinit
                             checkIfInFunction <- gets curFuncSignature
                             if isJust checkIfInFunction
                             then modify $ \_ -> prevEnv { vars = M.insert (name, Local) varBind boundVars }
                             else modify $ \_ -> prevEnv { vars = M.insert (name, Global) varBind boundVars }
                             return $ VarDecl $ varBind
                      A.StatementDecl stmt -> do
                             checkIfInFunctionAlready <- gets curFuncSignature
                             when (isNothing checkIfInFunctionAlready) $ throwError $ SemanticsError l sc ec StatementOutsideDeclarationError
                             StatementDecl <$> checkStmt stmt

checkDecoratedIdentifiersAndNames :: A.Location -> [A.DecoratedIdentifier] -> Semantics [DecoratedIdentifier]
checkDecoratedIdentifiersAndNames (l, sc, ec) idens = do
  sidens <- checkDecoratedIdentifiers (l, sc, ec) idens
  boundVars <- gets vars
  checkDecIdens sidens boundVars
  return sidens
    where checkDecIdens :: [DecoratedIdentifier] -> Variables -> Semantics ()
          checkDecIdens [] _ = return ()
          checkDecIdens ((DecoratedIdentifier _ name _):xs) bv = do
                                                        let varLookup = M.lookup (name, Local) bv <|> M.lookup (name, Formal) bv <|> M.lookup (name, Global) bv
                                                        when (isJust varLookup) $ throwError $ SemanticsError l sc ec $ DuplicateDeclaration name
                                                        checkDecIdens xs bv
                                             
checkDecoratedIdentifiers :: A.Location -> [A.DecoratedIdentifier] -> Semantics [DecoratedIdentifier]
checkDecoratedIdentifiers (l, sc, ec) idens = let sortedNames = map (\(A.DecoratedIdentifier _ n _) -> n) $
                                                                sortBy (\(A.DecoratedIdentifier _ n1 _) (A.DecoratedIdentifier _ n2 _) -> compare n1 n2) idens
                                                  checkDup [] = Nothing
                                                  checkDup [x] = Nothing
                                                  checkDup (x1:x2:xs)
                                                      | x1 == x2 = Just x1
                                                      | otherwise = checkDup (x2:xs)
                                                  maybeDup = checkDup sortedNames
                                              in case maybeDup of
                                                   Just dupName -> throwError $ SemanticsError l sc ec $ DuplicateField dupName
                                                   Nothing -> mapM convertIden idens
                                                       where convertIden (A.DecoratedIdentifier mods name decType) = do
                                                                 sDecType <- checkDecoratedType decType
                                                                 return $ DecoratedIdentifier mods name sDecType

checkStmt :: A.Statement -> Semantics Statement
checkStmt ((l, sc, ec), s) = checked
    where mkIfElse mkCond mkSb1 mkSb2 = IfElseStatement mkCond mkSb1 mkSb2 <$> (isJust <$> stmtReturns (l, sc, ec) mkSb1) <*> (isJust <$> stmtReturns (l, sc, ec) mkSb2)
          mkDoWhile mkCond mkSb = DoWhileStatement mkCond mkSb <$> (isJust <$> stmtReturns (l, sc, ec) mkSb)
          checked = case s of
                      A.ExpressionStatement e -> ExpressionStatement <$> checkExpr e
                      A.IfElseStatement cond b1 b2 -> do
                             scond <- checkExpr cond
                             boundVars <- gets vars
                             sb1 <- checkStmt b1
                             modify $ \env -> env { vars = boundVars }
                             boundVars <- gets vars
                             sb2 <- checkStmt b2
                             modify $ \env -> env { vars = boundVars }
                             case typeOf scond of
                               PureType Bool -> mkIfElse scond sb1 sb2
                               _ -> throwError $ SemanticsError l sc ec $ TypeError (PureType Bool) $ typeOf scond
                      A.WhileStatement cond b -> do
                             scond <- checkExpr cond
                             boundVars <- gets vars
                             sb <- checkStmt b
                             modify $ \env -> env { vars = boundVars }
                             case typeOf scond of
                               PureType Bool -> (\x -> mkIfElse scond x EmptyStatement) =<< (mkDoWhile scond sb)
                               _ -> throwError $ SemanticsError l sc ec $ TypeError (PureType Bool) $ typeOf scond
                      A.ForStatement decl cond iter b -> do
                             boundVars <- gets vars
                             sdecl <- checkDecl decl
                             scond <- checkExpr cond
                             siter <- checkExpr iter
                             sb <- checkStmt b
                             modify $ \env -> env { vars = boundVars }
                             case typeOf scond of
                               PureType Bool -> (\x -> Block [
                                                        sdecl,
                                                        StatementDecl x
                                                       ])
                                                <$> ((\x -> mkIfElse scond x EmptyStatement) =<< mkDoWhile scond (Block [StatementDecl sb, StatementDecl $ ExpressionStatement siter]))
                               _ -> throwError $ SemanticsError l sc ec $ TypeError (PureType Bool) $ typeOf scond
                      A.SwitchStatement _ _ -> undefined
                      A.CaseStatement _ _ -> undefined
                      A.ReturnStatement expr -> do
                             sexpr <- checkExpr expr
                             msig <- gets curFuncSignature
                             case msig of
                               Just (FunctionSignature _ _ _ retType) -> case implicitlyConvert sexpr retType of
                                                                            Right conv -> return $ ReturnStatement conv
                                                                            Left _ -> throwError $ SemanticsError l sc ec $ TypeError retType $ typeOf sexpr
                               Nothing -> throwError $ SemanticsError l sc ec ReturnNotInFunctionError
                      A.BreakStatement -> undefined
                      A.ContinueStatement -> undefined
                      A.Block decls -> do
                             boundVars <- gets vars
                             sdecls <- mapM checkDecl decls
                             modify $ \env -> env { vars = boundVars }
                             return $ Block sdecls
                      A.EmptyStatement -> return EmptyStatement

checkExpr :: A.Expression -> Semantics Expression
checkExpr ((l, sc, ec), e) = checked
    where checked = case e of
                      A.BooleanLiteral b -> return $ Literal $ ComptimeBool b
                      A.FixedPointLiteral (U8Val x) -> return $ Literal $ ComptimeU8 x
                      A.FixedPointLiteral (U16Val x) -> return $ Literal $ ComptimeU16 x
                      A.FixedPointLiteral (U32Val x) -> return $ Literal $ ComptimeU32 x
                      A.FixedPointLiteral (U64Val x) -> return $ Literal $ ComptimeU64 x
                      A.FixedPointLiteral (I8Val x) -> return $ Literal $ ComptimeI8 x
                      A.FixedPointLiteral (I16Val x) -> return $ Literal $ ComptimeI16 x
                      A.FixedPointLiteral (I32Val x) -> return $ Literal $ ComptimeI32 x
                      A.FixedPointLiteral (I64Val x) -> return $ Literal $ ComptimeI64 x
                      A.FloatingPointLiteral (F32Val x) -> return $ Literal $ ComptimeF32 x
                      A.FloatingPointLiteral (F64Val x) -> return $ Literal $ ComptimeF64 x
                      A.CharLiteral c -> return $ Literal $ ComptimeU8 c
                      A.StringLiteral s -> let unpacked = B.unpack s in return $ Literal $ ComptimeArr (ComptimeU8 <$> unpacked) $ PureType U8
                      A.Undefined -> return $ Undefined
                      A.ComptimeExpression ce -> Literal <$> (checkExpr ce >>= comptimeEvaluate (l, sc, ec))
                      A.ArrayLiteral x -> do
                             exprs <- mapM checkExpr x
                             if uniform $ map typeOf exprs then
                                 return $ Array exprs
                             else throwError $ SemanticsError l sc ec HeterogenousArray
                      A.PrimaryIdentifier t -> do
                             boundVars <- gets vars
                             let lookup = M.lookup (t, Local) boundVars <|> M.lookup (t, Formal) boundVars <|> M.lookup (t, Global) boundVars
                             case lookup of
                               Nothing -> throwError $ SemanticsError l sc ec $ UndefinedIdentifier t
                               Just dt -> return $ LValueExpression $ Identifier t $ (\(VarBinding (DecoratedIdentifier _ _ x) _) -> x) $ fromJust lookup
                      A.Call f args -> do
                             boundFuncs <- gets funcs
                             let lookup = M.lookup f boundFuncs
                             let checkCallFromSig sig = let (idens, retType) = (\(FunctionSignature _ _ idens retType) -> (idens, retType)) sig
                                                        in if length idens /= length args then throwError $ SemanticsError l sc ec $ CallError f (length idens) (length args)
                                                           else do
                                                             sargs <- mapM checkExpr args
                                                             let converts = zipWith implicitlyConvert sargs (map (\(DecoratedIdentifier _ _ t) -> t) idens)
                                                             if all isRight converts then return $ Call f (map (fromRight undefined) converts) retType
                                                             else let mismatch = fromLeft undefined $ head $ filter isLeft converts in throwError $ SemanticsError l sc ec $ TypeError (snd $ mismatch) (typeOf $ fst $ mismatch)
                             case lookup of
                               Nothing -> do
                                           curSig <- gets curFuncSignature
                                           case curSig of
                                             Nothing -> throwError $ SemanticsError l sc ec $ UndefinedIdentifier f
                                             Just sig@(FunctionSignature _ name _ _) -> if name == f then checkCallFromSig sig
                                                                                        else throwError $ SemanticsError l sc ec $ UndefinedIdentifier f
                               Just df -> checkCallFromSig $ (\(Function sig _) -> sig) df
                      A.Unary op expr -> do
                             sexpr <- checkExpr expr
                             case op of
                               A.PrePlusPlus -> if canIncDec $ typeOf sexpr
                                                then case sexpr of
                                                       LValueExpression lval -> return $ Crement PrePlusPlus lval (typeOf sexpr)
                                                       _ -> throwError $ SemanticsError l sc ec $ LValueCrementError
                                                else throwError $ SemanticsError l sc ec $ IncDecError $ typeOf sexpr
                               A.PreMinusMinus -> if canIncDec $ typeOf sexpr
                                                  then case sexpr of
                                                         LValueExpression lval -> return $ Crement PreMinusMinus lval (typeOf sexpr)
                                                         _ -> throwError $ SemanticsError l sc ec $ LValueCrementError
                                                  else throwError $ SemanticsError l sc ec $ IncDecError $ typeOf sexpr
                               A.PostPlusPlus -> if canIncDec $ typeOf sexpr
                                                 then case sexpr of
                                                        LValueExpression lval -> return $ Crement PostPlusPlus lval (typeOf sexpr)
                                                        _ -> throwError $ SemanticsError l sc ec $ LValueCrementError
                                                 else throwError $ SemanticsError l sc ec $ IncDecError $ typeOf sexpr
                               A.PostMinusMinus -> if canIncDec $ typeOf sexpr
                                                   then case sexpr of
                                                          LValueExpression lval -> return $ Crement PostMinusMinus lval (typeOf sexpr)
                                                          _ -> throwError $ SemanticsError l sc ec $ LValueCrementError
                                                   else throwError $ SemanticsError l sc ec $ IncDecError $ typeOf sexpr
                               A.Plus -> if numeric $ typeOf sexpr
                                         then return $ Unary Plus sexpr (typeOf sexpr)
                                         else throwError $ SemanticsError l sc ec $ NumericError $ typeOf sexpr
                               A.Minus -> if numeric $ typeOf sexpr
                                          then return $ Unary Minus sexpr (typeOf sexpr)
                                          else throwError $ SemanticsError l sc ec $ NumericError $ typeOf sexpr
                               A.Excla -> if boolean $ typeOf sexpr
                                          then return $ Unary Excla sexpr (typeOf sexpr)
                                          else throwError $ SemanticsError l sc ec $ TypeError (PureType Bool) $ typeOf sexpr
                               A.Tilda -> if singletonNonVoid $ typeOf sexpr
                                          then return $ Unary Tilda sexpr (typeOf sexpr)
                                          else throwError $ SemanticsError l sc ec PointerTypeError
                               A.Star -> case typeOf sexpr of
                                           DerefType t -> return $ LValueExpression $ Dereference sexpr $ t
                                           otherwise -> throwError $ SemanticsError l sc ec $ DerefNonPointerError $ typeOf sexpr
                               A.And -> case sexpr of
                                          LValueExpression lval -> return $ Address lval
                                          _ -> throwError $ SemanticsError l sc ec AddressError
                               A.Cast astDecType -> do
                                           t <- checkDecoratedType astDecType
                                           if checkExplicitCast (typeOf sexpr) t
                                           then return $ Cast sexpr t
                                           else throwError $ SemanticsError l sc ec $ CastError (typeOf sexpr) t
                               A.Index exprs -> case sexpr of
                                                  LValueExpression lval -> do
                                                             sexprs <- mapM checkExpr exprs
                                                             LValueExpression <$> indexArr lval sexprs
                                                  _ -> throwError $ SemanticsError l sc ec IndexNonLValue
                      A.Binary op expr1 expr2 -> do
                             sexpr1 <- checkExpr expr1
                             sexpr2 <- checkExpr expr2
                             case op of
                               A.Equals -> if not $ isLValue sexpr1 then throwError $ SemanticsError l sc ec AssignError
                                           else if not $ checkImplicitCast (typeOf sexpr2) (typeOf sexpr1) then throwError $ SemanticsError l sc ec $ ImplicitCastError (typeOf sexpr2) (typeOf sexpr1)
                                                else return $ Assign Equals ((\(LValueExpression lval) -> lval) sexpr1) sexpr2
                               A.PlusEquals -> arithEqualsOp sexpr1 sexpr2 PlusEquals True numeric numeric NumericError NumericError
                               A.MinusEquals -> arithEqualsOp sexpr1 sexpr2 MinusEquals True numeric numeric NumericError NumericError
                               A.StarEquals -> arithEqualsOp sexpr1 sexpr2 StarEquals False numeric numeric NumericError NumericError
                               A.SlashEquals -> arithEqualsOp sexpr1 sexpr2 SlashEquals False numeric numeric NumericError NumericError
                               A.PercentEquals -> arithEqualsOp sexpr1 sexpr2 PercentEquals False numeric isIntegralType NumericError NonIntegralError
                               A.LShiftEquals -> arithEqualsOp sexpr1 sexpr2 LShiftEquals False numeric isIntegralType NumericError NonIntegralError
                               A.HatEquals -> arithEqualsOp sexpr1 sexpr2 HatEquals False singletonNonVoid singletonNonVoid BadTypeError BadTypeError
                               A.BarEquals -> arithEqualsOp sexpr1 sexpr2 BarEquals False singletonNonVoid singletonNonVoid BadTypeError BadTypeError
                               A.AndEquals -> arithEqualsOp sexpr1 sexpr2 AndEquals False singletonNonVoid singletonNonVoid BadTypeError BadTypeError
                               A.LogicOr -> createCheckedOperand boolean boolean LogicOr (typeReconciliation sexpr1 sexpr2) (TypeError (PureType Bool)) (TypeError (PureType Bool))
                               A.LogicXor -> createCheckedOperand boolean boolean LogicXor (typeReconciliation sexpr1 sexpr2) (TypeError (PureType Bool)) (TypeError (PureType Bool))
                               A.LogicAnd -> createCheckedOperand boolean boolean LogicAnd (typeReconciliation sexpr1 sexpr2) (TypeError (PureType Bool)) (TypeError (PureType Bool))
                               A.BitwiseOr -> createCheckedOperand singletonNonVoid singletonNonVoid BitwiseOr (typeReconciliation sexpr1 sexpr2) BadTypeError BadTypeError
                               A.BitwiseXor -> createCheckedOperand singletonNonVoid singletonNonVoid BitwiseXor (typeReconciliation sexpr1 sexpr2) BadTypeError BadTypeError
                               A.BitwiseAnd -> createCheckedOperand singletonNonVoid singletonNonVoid BitwiseAnd (typeReconciliation sexpr1 sexpr2) BadTypeError BadTypeError
                               A.EqualsEquals -> createCheckedOperand singletonNonVoid singletonNonVoid EqualsEquals (overrideType (PureType Bool) $ typeReconciliation sexpr1 sexpr2) BadTypeError BadTypeError
                               A.ExclaEquals -> createCheckedOperand singletonNonVoid singletonNonVoid ExclaEquals (overrideType (PureType Bool) $ typeReconciliation sexpr1 sexpr2) BadTypeError BadTypeError
                               A.Greater -> createCheckedOperand numeric numeric Greater (overrideType (PureType Bool) $ typeReconciliation sexpr1 sexpr2) NumericError NumericError
                               A.Lesser -> createCheckedOperand numeric numeric Lesser (overrideType (PureType Bool) $ typeReconciliation sexpr1 sexpr2) NumericError NumericError
                               A.GreaterEquals -> createCheckedOperand numeric numeric GreaterEquals (overrideType (PureType Bool) $ typeReconciliation sexpr1 sexpr2) NumericError NumericError
                               A.LesserEquals -> createCheckedOperand numeric numeric LesserEquals (overrideType (PureType Bool) $ typeReconciliation sexpr1 sexpr2) NumericError NumericError
                               A.LShift -> createCheckedOperand numeric isIntegralType LShift (Right (sexpr1, sexpr2, typeOf sexpr1)) NumericError NonIntegralError
                               A.RShift -> createCheckedOperand numeric isIntegralType RShift (Right (sexpr1, sexpr2, typeOf sexpr1)) NumericError NonIntegralError
                               A.TermPlus -> case typeOf sexpr1 of
                                               DerefType _ -> createCheckedOperand (\_ -> True) isIntegralType TermPlus (Right (sexpr1, sexpr2, typeOf sexpr1)) BadTypeError NonIntegralError
                                               otherwise -> createCheckedOperand numeric numeric TermPlus (typeReconciliation sexpr1 sexpr2) NumericError NumericError
                               A.TermMinus -> case typeOf sexpr1 of
                                               DerefType _ -> createCheckedOperand (\_ -> True) isIntegralType TermMinus (Right (sexpr1, sexpr2, typeOf sexpr1)) BadTypeError NonIntegralError
                                               otherwise -> createCheckedOperand numeric numeric TermMinus (typeReconciliation sexpr1 sexpr2) NumericError NumericError
                               A.FactorStar -> createCheckedOperand numeric numeric FactorStar (typeReconciliation sexpr1 sexpr2) NumericError NumericError
                               A.FactorSlash -> createCheckedOperand numeric numeric FactorSlash (typeReconciliation sexpr1 sexpr2) NumericError NumericError
                               A.FactorPercent -> createCheckedOperand isIntegralType isIntegralType FactorPercent (typeReconciliation sexpr1 sexpr2) NonIntegralError NonIntegralError
                      A.Access expr1 expr2 -> do
                             sstruct <- checkExpr expr1
                             case sstruct of
                               LValueExpression lval ->
                                   case typeOf sstruct of
                                     PureType (StructType structName) -> do
                                                         boundStructs <- gets structs
                                                         let lookup = M.lookup structName boundStructs
                                                         case lookup of
                                                           Just struct ->
                                                               case expr2 of
                                                                 (_, A.PrimaryIdentifier fieldName) -> do
                                                                            (pos, t) <- getPosInStruct (l, sc, ec) fieldName struct 0
                                                                            return $ LValueExpression $ Access lval pos t
                                                                 _ -> throwError $ SemanticsError l sc ec NameAccessError
                                                           Nothing -> throwError $ SemanticsError (-1) (-1) (-1) $ BadTypeError $ PureType $ StructType structName
                                     _ -> throwError $ SemanticsError l sc ec NonStructFieldAccessError
                               _ -> throwError $ SemanticsError l sc ec LValueAccessError
          typeReconciliation sexpr1 sexpr2 = if typeOf sexpr1 == typeOf sexpr2 then Right (sexpr1, sexpr2, typeOf sexpr1)
                                             else if checkImplicitCast (typeOf sexpr1) (typeOf sexpr2) then Right (Cast sexpr1 $ typeOf sexpr2, sexpr2, typeOf sexpr2)
                                                  else if checkImplicitCast (typeOf sexpr2) (typeOf sexpr1) then Right (sexpr1, Cast sexpr2 $ typeOf sexpr1, typeOf sexpr1)
                                                       else Left $ SemanticsError l sc ec $ TypeReconcileError (typeOf sexpr1) (typeOf sexpr2)
          createCheckedOperand :: (DecoratedType -> Bool) -> (DecoratedType -> Bool) -> BinaryOp -> Either SemanticsError (Expression, Expression, DecoratedType) -> (DecoratedType -> SemanticsErrorType) -> (DecoratedType -> SemanticsErrorType) -> Semantics Expression
          createCheckedOperand _ _ _ (Left s) _ _ = throwError s
          createCheckedOperand f1 f2 o (Right (e1, e2, t)) auxEr1 auxEr2 = if (f1 $ typeOf e1) && (f2 $ typeOf e2) then return $ Binary o e1 e2 t
                                                                           else if f1 $ typeOf e1 then throwError $ SemanticsError l sc ec $ auxEr2 $ typeOf e2
                                                                                else throwError $ SemanticsError l sc ec $ auxEr1 $ typeOf e1
          overrideType t eith = (\(e1, e2, _) -> (e1, e2, t)) <$> eith
          singletonNonVoid (ArrayType _ _) = False
          singletonNonVoid (DerefType _) = False
          singletonNonVoid (PureType Void) = False
          singletonNonVoid _ = True
          canIncDec (ArrayType _ _) = False
          canIncDec (PureType Void) = False
          canIncDec (PureType Bool) = False
          canIncDec _ = True
          numeric (ArrayType _ _) = False
          numeric (DerefType _) = False
          numeric (PureType Void) = False
          numeric (PureType Bool) = False
          numeric _ = True
          boolean (PureType Bool) = True
          boolean _ = False
          isIntegralType (PureType U8) = True
          isIntegralType (PureType U16) = True
          isIntegralType (PureType U32) = True
          isIntegralType (PureType U64) = True
          isIntegralType (PureType I8) = True
          isIntegralType (PureType I16) = True
          isIntegralType (PureType I32) = True
          isIntegralType (PureType I64) = True
          isIntegralType _ = False
          isLValue (LValueExpression _) = True
          isLValue _ = False
          indexArr :: LValue -> [Expression] -> Semantics LValue
          indexArr lval [] = return lval
          indexArr lval (index:indices)
              | isIntegralType $ typeOf index = case typeOf $ LValueExpression lval of
                                                  (ArrayType t s) -> (\x -> return $ Index x index t s) =<< (indexArr lval indices)
                                                  _ -> throwError $ SemanticsError l sc ec IndexNonArrayError
              | otherwise = throwError $ SemanticsError l sc ec (NonIntegralError $ typeOf index)
          arithEqualsOp :: Expression -> Expression -> AssignOp -> Bool -> (DecoratedType -> Bool) -> (DecoratedType -> Bool) -> (DecoratedType -> SemanticsErrorType) -> (DecoratedType -> SemanticsErrorType) -> Semantics Expression
          arithEqualsOp sexpr1 sexpr2 op canPtrLeft f1 f2 err1 err2 = if not $ isLValue sexpr1 then throwError $ SemanticsError l sc ec AssignError
                                                                      else case typeOf sexpr1 of
                                                                             DerefType _ -> if (not canPtrLeft) || (not $ isIntegralType $ typeOf sexpr2) then throwError $ SemanticsError l sc ec $ NonIntegralError $ typeOf sexpr2
                                                                                            else return $ Assign op ((\(LValueExpression lval) -> lval) sexpr1) sexpr2
                                                                             _ -> if (f1 $ typeOf sexpr1) && (f2 $ typeOf sexpr2)
                                                                                  then let converted = implicitlyConvert sexpr2 (typeOf sexpr1)
                                                                                       in case converted of
                                                                                            Left (_, _) -> throwError $ SemanticsError l sc ec $ ImplicitCastError (typeOf sexpr2) (typeOf sexpr1)
                                                                                            Right casted -> return $ Assign op ((\(LValueExpression lval) -> lval) sexpr1) casted
                                                                                  else if f1 $ typeOf sexpr1 then throwError $ SemanticsError l sc ec $ err2 $ typeOf sexpr2
                                                                                       else throwError $ SemanticsError l sc ec $ err1 $ typeOf sexpr1

checkDecoratedType :: A.DecoratedType -> Semantics DecoratedType
checkDecoratedType ((l, sc, ec), A.PureType t) = return $ PureType t
checkDecoratedType ((l, sc, ec), A.DerefType t) = DerefType <$> checkDecoratedType t
checkDecoratedType ((l, sc, ec), A.ArrayType t e) = do
  sexpr <- checkExpr e
  st <- checkDecoratedType t
  let recurCase se = case se of
                       Literal cv -> case cv of
                                       ComptimeU8 x -> exWord x st
                                       ComptimeU16 x -> exWord x st
                                       ComptimeU32 x -> exWord x st
                                       ComptimeU64 x -> exWord x st
                                       ComptimeI8 x -> exWord x st
                                       ComptimeI16 x -> exWord x st
                                       ComptimeI32 x -> exWord x st
                                       ComptimeI64 x -> exWord x st
                                       _ -> throwError $ SemanticsError l sc ec $ InvalidArraySizeError
                       LValueExpression (Identifier name t) -> do
                                                        boundVars <- gets vars
                                                        let lookup = M.lookup (name, Local) boundVars <|> M.lookup (name, Formal) boundVars <|> M.lookup (name, Global) boundVars
                                                        case lookup of
                                                          Nothing -> throwError $ SemanticsError l sc ec $ UndefinedIdentifier name
                                                          Just (VarBinding (DecoratedIdentifier mods _ varT) ve) ->
                                                              if A.Const `elem` mods
                                                              then if varT `elem` [PureType U8, PureType U16, PureType U32, PureType U64, PureType I8, PureType I16, PureType I32, PureType I64]
                                                                   then recurCase ve
                                                                   else throwError $ SemanticsError l sc ec $ NonIntegralError varT
                                                              else throwError $ SemanticsError l sc ec $ NonConstArraySizeError
                       _ -> throwError $ SemanticsError l sc ec $ NonComptimeError
  recurCase sexpr
    where exWord :: Integral a => a -> DecoratedType -> Semantics DecoratedType
          exWord x st = case extractWord64 x of
                          Just ex64 -> return $ ArrayType st ex64
                          Nothing -> throwError $ SemanticsError l sc ec $ InvalidArraySizeError

extractWord64 :: Integral a => a -> Maybe Word64
extractWord64 x
    | x >= 0 = Just $ fromIntegral x
    | otherwise = Nothing

sizeOf :: DecoratedType -> Semantics Word64
sizeOf (PureType Void) = return 0
sizeOf (PureType Bool) = return 1
sizeOf (PureType U8) = return 1
sizeOf (PureType U16) = return 2
sizeOf (PureType U32) = return 4
sizeOf (PureType U64) = return 8
sizeOf (PureType I8) = return 1
sizeOf (PureType I16) = return 2
sizeOf (PureType I32) = return 4
sizeOf (PureType I64) = return 8
sizeOf (PureType F32) = return 4
sizeOf (PureType F64) = return 8
sizeOf (PureType (StructType structName)) = do
  boundStructs <- gets structs
  let lookup = M.lookup structName boundStructs
  case lookup of
    Just (Structure _ _ decIdens) -> (foldl (+) 0) <$> (mapM sizeOf $ map (\(DecoratedIdentifier _ _ t) -> t) decIdens)
    Nothing -> throwError $ SemanticsError (-1) (-1) (-1) $ BadTypeError $ PureType $ StructType structName
sizeOf (DerefType _) = return 8
sizeOf (ArrayType t s) = (* s) <$> sizeOf t

getPosInStruct :: (Int, Int, Int) -> Text -> Structure -> Word64 -> Semantics (Word64, DecoratedType)
getPosInStruct (l, sc, ec) search (Structure _ structName []) _ = throwError $ SemanticsError l sc ec $ FieldAccessError structName search
getPosInStruct errPos search (Structure mods structName ((DecoratedIdentifier _ fieldName t):dis)) pos
    | search == fieldName = return (pos, t)
    | otherwise = sizeOf t >>= (\x -> getPosInStruct errPos search (Structure mods structName dis) (pos + 1))

isTypeVoid :: DecoratedType -> Bool
isTypeVoid (PureType Void) = True
isTypeVoid (DerefType t) = isTypeVoid t
isTypeVoid (ArrayType t _) = isTypeVoid t
isTypeVoid _ = False

stmtReturns :: A.Location -> Statement -> Semantics (Maybe DecoratedType)
stmtReturns _ (ExpressionStatement _) = return Nothing
stmtReturns (l, sc, ec) (IfElseStatement _ s1 s2 _ _) = do
  curSig <- gets curFuncSignature
  rs1 <- stmtReturns (l, sc, ec) s1
  rs2 <- stmtReturns (l, sc, ec) s2
  case (rs1, rs2) of
    (Nothing, Nothing) -> return Nothing
    (Nothing, _) -> return Nothing
    (_, Nothing) -> return Nothing
    (Just ts1, Just ts2) ->
        case curSig of
          Nothing -> throwError $ SemanticsError l sc ec StatementOutsideDeclarationError
          Just (FunctionSignature _ _ _ retType) -> if ts1 /= retType then throwError $ SemanticsError l sc ec $ TypeError retType ts1
                                  else if ts2 /= retType then throwError $ SemanticsError l sc ec $ TypeError retType ts2
                                       else return $ Just retType
stmtReturns (l, sc, ec) (DoWhileStatement _ s _) = do
  curSig <- gets curFuncSignature
  rs <- stmtReturns (l, sc, ec) s
  case rs of
    Nothing -> return Nothing
    Just ts ->
        case curSig of
          Nothing -> throwError $ SemanticsError l sc ec StatementOutsideDeclarationError
          Just (FunctionSignature _ _ _ retType) -> if ts /= retType then throwError $ SemanticsError l sc ec $ TypeError retType ts
                                  else return $ Just retType
stmtReturns _ (ReturnStatement e) = return $ Just $ typeOf e
stmtReturns _ (Block []) = return Nothing
stmtReturns (l, sc, ec) (Block (x:xs)) = do
  first <- case x of
             StatementDecl sx -> stmtReturns (l, sc, ec) sx
             otherwise -> return Nothing
  case first of
    Just retType -> case xs of
                      [] -> return $ Just retType
                      otherwise -> throwError $ SemanticsError l sc ec DeadCode
    Nothing -> stmtReturns (l, sc, ec) $ Block xs
stmtReturns _ EmptyStatement = return Nothing

appendSet :: Eq a => a -> [a] -> [a]
appendSet x xs = union xs [x]

unions :: Eq a => [[a]] -> [a]
unions [] = []
unions (x:xs) = foldl' union x xs

class Depends d where
    depends :: A.Location -> d -> Semantics [Declaration]

instance Depends Declaration where
    depends loc s@(StructDecl (Structure _ _ idens)) = appendSet s <$> (unions <$> mapM (depends loc) idens)
    depends loc f@(FuncDecl (Function sig@(FunctionSignature _ n idens retType) s)) = do
                               msig <- gets curFuncSignature
                               let recursive = case msig of
                                                 Just (FunctionSignature _ in_n _ _) -> n == in_n
                                                 Nothing -> False
                               if recursive then return []
                               else do
                                 modify $ \env -> env { curFuncSignature = Just sig }
                                 appendSet f <$> (unions <$> sequence
                                                             [unions <$> mapM (depends loc) idens, (depends loc) retType, (depends loc) s])
    depends loc (VarDecl (VarBinding iden e)) = unions <$> sequence [(depends loc) iden, (depends loc) e]
    depends loc (StatementDecl s) = (depends loc) s

instance Depends Statement where
    depends loc (ExpressionStatement e) = (depends loc) e
    depends loc (IfElseStatement e s1 s2 _ _) = unions <$> sequence [(depends loc) e, (depends loc) s1, (depends loc) s2]
    depends loc (DoWhileStatement e s _) = unions <$> sequence [(depends loc) e, (depends loc) s]
    depends loc (ReturnStatement e) = (depends loc) e
    depends loc (Block ds) = unions <$> mapM (depends loc) ds
    depends _ (EmptyStatement) = return []

instance Depends Expression where
    depends loc (Binary op e1 e2 dt) = unions <$> sequence [(depends loc) e1, (depends loc) e2, (depends loc) dt]
    depends loc (Unary _ e dt) = unions <$> sequence [(depends loc) e, (depends loc) dt]
    depends loc (Literal cv) = (depends loc) cv
    depends loc (Array es) = unions <$> mapM (depends loc) es
    depends loc (Call fn es dt) = unions <$> sequence [(do
                                                         boundFuncs <- gets funcs
                                                         let lookup = M.lookup fn boundFuncs
                                                         case lookup of
                                                           Just f -> unions <$> sequence [(depends loc) $ FuncDecl f, return [FuncDecl f]]
                                                           Nothing -> throwError $ SemanticsError (-1) (-1) (-1) $ UndefinedIdentifier fn),
                                                       unions <$> mapM (depends loc) es,
                                                       (depends loc) dt]
    depends loc (Cast e dt) = unions <$> sequence [(depends loc) e, (depends loc) dt]
    depends loc (LValueExpression lv) = (depends loc) lv
    depends loc (Assign _ lv e) = unions <$> sequence [(depends loc) lv, (depends loc) e]
    depends loc (Address lv) = (depends loc) lv
    depends loc (Crement _ lv dt) = unions <$> sequence [(depends loc) lv, (depends loc) dt]
    depends _ Undefined = return []

instance Depends LValue where
    depends loc (Dereference e dt) = unions <$> sequence [(depends loc) e, (depends loc) dt]
    depends loc (Access lv _ dt) = unions <$> sequence [(depends loc) lv, (depends loc) dt]
    depends loc (Index lv e dt _) = unions <$> sequence [(depends loc) lv, (depends loc) e, (depends loc) dt]
    depends loc@(l, sc, ec) (Identifier n dt) = unions <$> sequence [(do
                                                                       boundVars <- gets vars
                                                                       let varLookup = M.lookup (n, Global) boundVars
                                                                       case varLookup of
                                                                         Just v@(VarBinding (DecoratedIdentifier mods _ _) _) -> do
                                                                                        if A.Const `elem` mods then unions <$> sequence [(depends loc) $ VarDecl v, return [VarDecl v]]
                                                                                        else throwError $ SemanticsError l sc ec $ CannotComptimeError
                                                                         Nothing -> return []), (depends loc) dt]

instance Depends ComptimeValue where
    depends loc (ComptimePointer _ dt) = (depends loc) dt
    depends loc (ComptimeStruct cvs n) = unions <$> sequence [(depends loc) $ PureType $ StructType n, unions <$> mapM (depends loc) cvs]
    depends loc (ComptimeArr cvs dt) = unions <$> sequence [unions <$> mapM (depends loc) cvs, (depends loc) dt]
    depends _ _ = return []

instance Depends DecoratedIdentifier where
    depends loc (DecoratedIdentifier _ _ dt) = (depends loc) dt

instance Depends DecoratedType where
    depends loc (PureType (StructType n)) = do
                                 boundStructs <- gets structs
                                 case M.lookup n boundStructs of
                                   Just struct -> return [StructDecl struct]
                                   Nothing -> throwError $ SemanticsError (-1) (-1) (-1) $ BadTypeError $ PureType $ StructType n
    depends loc (DerefType dt) = (depends loc) dt
    depends loc (ArrayType dt _) = (depends loc) dt
    depends _ _ = return []

comptimeEvaluate :: A.Location -> Expression -> Semantics ComptimeValue
comptimeEvaluate (l, sc, ec) e = do
  bound <- get
  dependencies <- depends (l, sc, ec) e
  modify $ \_ -> bound
  let sast = SAST (dependencies ++ [FuncDecl (Function (FunctionSignature [] (pack "@comptime_eval") [] (typeOf e)) (ReturnStatement e))])
  sastptr <- liftIO $ callocBytes sizeOfSAST
  liftIO $ poke sastptr sast
  dtptr <- liftIO $ callocBytes sizeOfDT
  liftIO $ poke dtptr $ typeOf e
  cvptr <- liftIO $ c_comptime_eval sastptr dtptr
  when (cvptr == nullPtr) $ throwError $ SemanticsError l sc ec CannotComptimeError
  cv <- liftIO $ peek cvptr
  liftIO $ c_destruct_comptime_value cvptr
  liftIO $ free cvptr
  return cv;
