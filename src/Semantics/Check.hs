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
                                 structs :: Structures }

type Semantics = ExceptT SemanticsError (State Environment)

uniform :: Eq a => [a] -> Bool
uniform [] = True
uniform (x:xs) = all (== x) xs

implicitlyConvert :: Expression -> DecoratedType -> Either (Expression, DecoratedType) Expression
implicitlyConvert e t = if checkImplicitCast (typeOf e) t then Right e else Left (e, t)

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

checkExpr :: A.Expression -> Semantics Expression
checkExpr ((l, sc, ec), e) = checked
    where checked = case e of
                      A.BooleanLiteral b -> return $ Literal $ BooleanLiteral b
                      A.FixedPointLiteral i -> case i of
                                                 A.U8Val _ -> return $ Literal $ FixedPointLiteral i
                                                 A.U16Val _ -> return $ Literal $ FixedPointLiteral i
                                                 A.U32Val _ -> return $ Literal $ FixedPointLiteral i
                                                 A.U64Val _ -> return $ Literal $ FixedPointLiteral i
                                                 A.I8Val _ -> return $ Literal $ FixedPointLiteral i
                                                 A.I16Val _ -> return $ Literal $ FixedPointLiteral i
                                                 A.I32Val _ -> return $ Literal $ FixedPointLiteral i
                                                 A.I64Val _ -> return $ Literal $ FixedPointLiteral i
                      A.FloatingPointLiteral d -> case d of
                                                    A.F32Val _ -> return $ Literal $ FloatingPointLiteral d
                                                    A.F64Val _ -> return $ Literal $ FloatingPointLiteral d
                      A.CharLiteral c -> return $ Literal $ FixedPointLiteral $ U8Val c
                      A.StringLiteral s -> return $ ArrayLiteral $ map (Literal . FixedPointLiteral . U8Val) $ B.unpack s
                      A.Undefined -> return $ Undefined
                      A.ArrayLiteral x -> do
                             exprs <- sequence $ map checkExpr x
                             if uniform $ map typeOf exprs then
                                 return $ ArrayLiteral exprs
                             else throwError $ SemanticsError l sc ec HeterogenousArray
                      A.PrimaryIdentifier t -> do
                             boundVars <- lift $ gets vars
                             let lookup = M.lookup (t, Local) boundVars <|> M.lookup (t, Formal) boundVars <|> M.lookup (t, Global) boundVars
                             case lookup of
                               Nothing -> throwError $ SemanticsError l sc ec $ UndefinedIdentifier t
                               Just dt -> return $ LValueExpression $ Identifier t $ (\(VarBinding _ (DecoratedIdentifier _ _ x) _) -> x) $ fromJust lookup
                      A.Call f args -> do
                             boundFuncs <- lift $ gets funcs
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
                               A.Tilda -> if notPointer $ typeOf sexpr then return $ Unary Tilda sexpr (typeOf sexpr) else throwError $ SemanticsError l sc ec PointerTypeError
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
                               A.LogicOr -> createCheckedOperand boolean LogicOr typeReconciliation BooleanError
                                   where typeReconciliation = if typeOf sexpr1 == typeOf sexpr2 then Right (sexpr1, sexpr2, typeOf sexpr1)
                                                              else if checkImplicitCast (typeOf sexpr1) (typeOf sexpr2) then Right (Unary Cast sexpr1 $ typeOf sexpr2, sexpr2, typeOf sexpr2)
                                                                   else if checkImplicitCast (typeOf sexpr2) (typeOf sexpr1) then Right (sexpr1, Unary Cast sexpr2 $ typeOf sexpr1, typeOf sexpr1)
                                                                        else Left $ SemanticsError l sc ec $ TypeReconcileError (typeOf sexpr1) (typeOf sexpr2)
                                         createCheckedOperand :: (DecoratedType -> Bool) -> BinaryOp -> Either SemanticsError (Expression, Expression, DecoratedType) -> (DecoratedType -> SemanticsErrorType) -> Semantics Expression
                                         createCheckedOperand _ _ (Left s) _ = throwError s
                                         createCheckedOperand f o (Right (e1, e2, t)) auxEr = if (f $ typeOf e1) && (f $ typeOf e2)
                                                                                                      then return $ Binary o e1 e2 t
                                                                                                      else if f $ typeOf e1 then throwError $ SemanticsError l sc ec $ auxEr $ typeOf e2
                                                                                                           else throwError $ SemanticsError l sc ec $ auxEr $ typeOf e1
          canIncDec (ArrayType t _) = canIncDec t
          canIncDec (PureType Void) = False
          canIncDec (PureType Bool) = False
          canIncDec _ = True
          numeric (ArrayType t _) = numeric t
          numeric (DerefType _) = False
          numeric (PureType Void) = False
          numeric (PureType Bool) = False
          numeric _ = True
          boolean (ArrayType t _) = boolean t
          boolean (PureType Bool) = True
          boolean _ = False
          notPointer (ArrayType t _) = notPointer t
          notPointer (DerefType _) = False
          notPointer _ = True
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
          indexArr :: Expression -> [Expression] -> Semantics Expression
          indexArr e [] = return e
          indexArr e (index:indices)
              | isIntegralType $ typeOf index = case typeOf e of
                                                  (ArrayType t _) -> (\x -> return $ Unary (Index index) x t) =<< (indexArr e indices)
                                                  _ -> throwError $ SemanticsError l sc ec IndexNonArrayError
              | otherwise = throwError $ SemanticsError l sc ec NonIntegralIndexError

checkDecoratedType :: A.DecoratedType -> Semantics DecoratedType
checkDecoratedType ((l, sc, ec), A.PureType t) = return $ PureType t
checkDecoratedType ((l, sc, ec), A.DerefType t) = DerefType <$> checkDecoratedType t
checkDecoratedType ((l, sc, ec), A.ArrayType t e) = do
  sexpr <- checkExpr e
  case sexpr of
    Literal c -> case c of
                   FixedPointLiteral v -> case extractWord64 v of
                                            Just w64 -> (\x -> ArrayType x w64) <$> checkDecoratedType t
                                            _ -> throwError $ SemanticsError l sc ec $ InvalidArraySizeError
                   _ -> throwError $ SemanticsError l sc ec $ InvalidArraySizeError
    _ -> throwError $ SemanticsError l sc ec $ NonComptimeError

extractWord64 :: FixedPointVal -> Maybe Word64
extractWord64 (U8Val x) = Just $ fromIntegral x
extractWord64 (U16Val x) = Just $ fromIntegral x
extractWord64 (U32Val x) = Just $ fromIntegral x
extractWord64 (U64Val x) = Just $ fromIntegral x
extractWord64 (I8Val x)
    | x >= 0 = Just $ fromIntegral x
    | otherwise = Nothing
extractWord64 (I16Val x)
    | x >= 0 = Just $ fromIntegral x
    | otherwise = Nothing
extractWord64 (I32Val x)
    | x >= 0 = Just $ fromIntegral x
    | otherwise = Nothing
extractWord64 (I64Val x)
    | x >= 0 = Just $ fromIntegral x
    | otherwise = Nothing
