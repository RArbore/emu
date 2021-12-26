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

module Semantics.Check
    (

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
import Data.Text (Text)
import Data.Word

import Parser.AST (Type (..),
                   Modifier (..),
                   FixedPointVal (..),
                   FloatingPointVal (..))
import qualified Parser.AST as A
    
import Semantics.Error
import Semantics.SAST

type Variables = M.Map (Text, VarKind) VarBinding
type Functions = M.Map Text Function
type Structures = M.Map Text Structure

data Environment = Environment { vars :: Variables,
                                 funcs :: Functions,
                                 structs :: Structures,
                                 curFuncRetType :: DecoratedType }

type Semantics = ExceptT SemanticsError (State Environment)

uniform :: Eq a => [a] -> Bool
uniform [] = True
uniform (x:xs) = all (== x) xs

implicitlyConvert :: Expression -> DecoratedType -> Either (Expression, DecoratedType) Expression
implicitlyConvert e t = if checkImplicitCast (typeOf e) t then if typeOf e == t then Right e else Right (Unary Cast e t) else Left (e, t)

checkExplicitCast :: DecoratedType -> DecoratedType -> Bool
checkExplicitCast t1 t2 = checkExplicitCastHelper t1 t2 || checkImplicitCast t1 t2
    where checkExplicitCastHelper (PureType U8) (DerefType _) = True
          checkExplicitCastHelper (PureType U16) (DerefType _) = True
          checkExplicitCastHelper (PureType U32) (DerefType _) = True
          checkExplicitCastHelper (PureType U64) (DerefType _) = True
          checkExplicitCastHelper (DerefType _) (PureType U64) = True
          checkExplicitCastHelper (PureType F32) (PureType U8) = True
          checkExplicitCastHelper (PureType F64) (PureType U8) = True
          checkExplicitCastHelper (PureType F32) (PureType U16) = True
          checkExplicitCastHelper (PureType F64) (PureType U16) = True
          checkExplicitCastHelper (PureType F32) (PureType U32) = True
          checkExplicitCastHelper (PureType F64) (PureType U32) = True
          checkExplicitCastHelper (PureType F32) (PureType U64) = True
          checkExplicitCastHelper (PureType F64) (PureType U64) = True
          checkExplicitCastHelper (PureType F32) (PureType I8) = True
          checkExplicitCastHelper (PureType F64) (PureType I8) = True
          checkExplicitCastHelper (PureType F32) (PureType I16) = True
          checkExplicitCastHelper (PureType F64) (PureType I16) = True
          checkExplicitCastHelper (PureType F32) (PureType I32) = True
          checkExplicitCastHelper (PureType F64) (PureType I32) = True
          checkExplicitCastHelper (PureType F32) (PureType I64) = True
          checkExplicitCastHelper (PureType F64) (PureType I64) = True
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
          checkImplicitCastHelper _ _ = False

check :: A.AST -> Semantics SAST
check = undefined

checkDecl :: A.Declaration -> Semantics Declaration
checkDecl ((l, sc, ec), d) = checked
    where checked = case d of
                      A.StructDecl mods name fields -> do
                             boundVars <- gets vars
                             boundFuncs <- gets funcs
                             boundStructs <- gets structs
                             let lookup = M.lookup (name, Local) boundVars <|> M.lookup (name, Formal) boundVars <|> M.lookup (name, Global) boundVars
                             let funcLookup = M.lookup name boundFuncs
                             let structLookup = M.lookup name boundStructs
                             when (isJust lookup || isJust funcLookup || isJust structLookup) $ throwError $ SemanticsError l sc ec $ DuplicateDeclaration name
                             sfields <- checkFields (l, sc, ec) fields
                             when (A.Pure `elem` mods) $ throwError $ SemanticsError l sc ec $ InvalidModifier A.Pure
                             when (A.Const `elem` mods) $ throwError $ SemanticsError l sc ec $ InvalidModifier A.Const
                             when (A.Inline `elem` mods) $ throwError $ SemanticsError l sc ec $ InvalidModifier A.Inline
                             when (A.Comptime `elem` mods) $ throwError $ SemanticsError l sc ec $ InvalidModifier A.Comptime
                             when (A.Register `elem` mods) $ throwError $ SemanticsError l sc ec $ InvalidModifier A.Register
                             when (A.Restrict `elem` mods) $ throwError $ SemanticsError l sc ec $ InvalidModifier A.Restrict
                             return $ StructDecl $ Structure mods name sfields

checkFields :: A.Location -> [A.DecoratedIdentifier] -> Semantics [DecoratedIdentifier]
checkFields (l, sc, ec) idens = let sortedNames = map (\(A.DecoratedIdentifier _ n _) -> n) $
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
    where checked = case s of
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
                               PureType Bool -> return $ IfElseStatement scond sb1 sb2
                               _ -> throwError $ SemanticsError l sc ec $ TypeError (PureType Bool) $ typeOf scond
                      A.WhileStatement cond b -> do
                             scond <- checkExpr cond
                             boundVars <- gets vars
                             sb <- checkStmt b
                             modify $ \env -> env { vars = boundVars }
                             case typeOf scond of
                               PureType Bool -> return $ IfElseStatement scond (DoWhileStatement scond sb) EmptyStatement
                               _ -> throwError $ SemanticsError l sc ec $ TypeError (PureType Bool) $ typeOf scond
                      A.ForStatement decl cond iter b -> do
                             boundVars <- gets vars
                             sdecl <- checkDecl decl
                             scond <- checkExpr cond
                             siter <- checkExpr iter
                             sb <- checkStmt b
                             modify $ \env -> env { vars = boundVars }
                             case typeOf scond of
                               PureType Bool -> return $ Block [
                                                          sdecl,
                                                          StatementDecl $ IfElseStatement scond
                                                                            (DoWhileStatement scond (Block [StatementDecl sb, StatementDecl $ ExpressionStatement siter]))
                                                                            EmptyStatement
                                                         ]
                               _ -> throwError $ SemanticsError l sc ec $ TypeError (PureType Bool) $ typeOf scond
                      A.SwitchStatement _ _ -> undefined
                      A.CaseStatement _ _ -> undefined
                      A.ReturnStatement expr -> do
                             sexpr <- checkExpr expr
                             retType <- gets curFuncRetType
                             if typeOf sexpr == retType
                             then return $ ReturnStatement sexpr
                             else throwError $ SemanticsError l sc ec $ TypeError retType $ typeOf sexpr
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
                      A.StringLiteral s -> let unpacked = B.unpack s in return $ Literal $ ComptimeU8Arr unpacked [fromIntegral $ length unpacked]
                      A.Undefined -> return $ Undefined
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
                               Just dt -> return $ LValueExpression $ Identifier t $ (\(VarBinding _ (DecoratedIdentifier _ _ x) _) -> x) $ fromJust lookup
                      A.Call f args -> do
                             boundFuncs <- gets funcs
                             let lookup = M.lookup f boundFuncs
                             case lookup of
                               Nothing -> throwError $ SemanticsError l sc ec $ UndefinedIdentifier f
                               Just df -> let (idens, retType) = (\(Function _ _ idens retType _) -> (idens, retType)) df
                                          in if length idens /= length args then throwError $ SemanticsError l sc ec $ CallError f (length idens) (length args)
                                          else do
                                            sargs <- mapM checkExpr args
                                            let converts = zipWith implicitlyConvert sargs (map (\(DecoratedIdentifier _ _ t) -> t) idens)
                                            if all isRight converts then return $ Call f (map (fromRight undefined) converts) retType
                                            else let mismatch = fromLeft undefined $ head $ filter isLeft converts in throwError $ SemanticsError l sc ec $ TypeError (snd $ mismatch) (typeOf $ fst $ mismatch)
                      A.Unary op expr -> do
                             sexpr <- checkExpr expr
                             case op of
                               A.PrePlusPlus -> if canIncDec $ typeOf sexpr then return $ Unary PrePlusPlus sexpr (typeOf sexpr) else throwError $ SemanticsError l sc ec $ IncDecError $ typeOf sexpr
                               A.PreMinusMinus -> if canIncDec $ typeOf sexpr then return $ Unary PreMinusMinus sexpr (typeOf sexpr) else throwError $ SemanticsError l sc ec $ IncDecError $ typeOf sexpr
                               A.PostPlusPlus -> if canIncDec $ typeOf sexpr then return $ Unary PostPlusPlus sexpr (typeOf sexpr) else throwError $ SemanticsError l sc ec $ IncDecError $ typeOf sexpr
                               A.PostMinusMinus -> if canIncDec $ typeOf sexpr then return $ Unary PostMinusMinus sexpr (typeOf sexpr) else throwError $ SemanticsError l sc ec $ IncDecError $ typeOf sexpr
                               A.Plus -> if numeric $ typeOf sexpr then return $ Unary Plus sexpr (typeOf sexpr) else throwError $ SemanticsError l sc ec $ NumericError $ typeOf sexpr
                               A.Minus -> if numeric $ typeOf sexpr then return $ Unary Minus sexpr (typeOf sexpr) else throwError $ SemanticsError l sc ec $ NumericError $ typeOf sexpr
                               A.Excla -> if boolean $ typeOf sexpr then return $ Unary Excla sexpr (typeOf sexpr) else throwError $ SemanticsError l sc ec $ BooleanError $ typeOf sexpr
                               A.Tilda -> if singletonNonVoid $ typeOf sexpr then return $ Unary Tilda sexpr (typeOf sexpr) else throwError $ SemanticsError l sc ec PointerTypeError
                               A.Star -> if canDeref $ typeOf sexpr then return $ LValueExpression $ Dereference sexpr else throwError $ SemanticsError l sc ec $ DerefNonPointerError $ typeOf sexpr
                               A.And -> case sexpr of
                                          LValueExpression lval -> return $ Address lval
                                          _ -> throwError $ SemanticsError l sc ec AddressError
                               A.Cast astDecType -> do
                                           t <- checkDecoratedType astDecType
                                           if checkExplicitCast (typeOf sexpr) t then return $ Unary Cast sexpr t else throwError $ SemanticsError l sc ec $ CastError (typeOf sexpr) t
                               A.Index exprs -> do
                                           sexprs <- mapM checkExpr exprs
                                           indexArr sexpr sexprs
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
                               A.LogicOr -> createCheckedOperand boolean boolean LogicOr (typeReconciliation sexpr1 sexpr2) BooleanError BooleanError
                               A.LogicXor -> createCheckedOperand boolean boolean LogicXor (typeReconciliation sexpr1 sexpr2) BooleanError BooleanError
                               A.LogicAnd -> createCheckedOperand boolean boolean LogicAnd (typeReconciliation sexpr1 sexpr2) BooleanError BooleanError
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
                                             else if checkImplicitCast (typeOf sexpr1) (typeOf sexpr2) then Right (Unary Cast sexpr1 $ typeOf sexpr2, sexpr2, typeOf sexpr2)
                                                  else if checkImplicitCast (typeOf sexpr2) (typeOf sexpr1) then Right (sexpr1, Unary Cast sexpr2 $ typeOf sexpr1, typeOf sexpr1)
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
          canDeref (DerefType _) = True
          canDeref _ = False
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
          indexArr :: Expression -> [Expression] -> Semantics Expression
          indexArr e [] = return e
          indexArr e (index:indices)
              | isIntegralType $ typeOf index = case typeOf e of
                                                  (ArrayType t _) -> (\x -> return $ Unary (Index index) x t) =<< (indexArr e indices)
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
  case sexpr of
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
    _ -> throwError $ SemanticsError l sc ec $ NonComptimeError
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
    | otherwise = sizeOf t >>= (\x -> getPosInStruct errPos search (Structure mods structName dis) (pos + x))
