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

{-# LANGUAGE OverloadedStrings #-}

module Semantics.Error
    (

     SemanticsError (..),
     SemanticsErrorType (..),
     VarKind (..),

     showSError

    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Word

import Parser.AST (Type (..), Modifier (..))

import Semantics.SAST
     
data SemanticsError = SemanticsError { line :: Int,
                                       startCol :: Int,
                                       endCol :: Int,
                                       errorType :: SemanticsErrorType }

data SemanticsErrorType = DuplicateDeclaration Text 
                        | DuplicateField Text 
                        | VoidVarDeclaration Text
                        | UndefinedIdentifier Text
                        | TypeError DecoratedType DecoratedType
                        | BadTypeError DecoratedType
                        | CastError DecoratedType DecoratedType
                        | ImplicitCastError DecoratedType DecoratedType
                        | IncDecError DecoratedType
                        | NumericError DecoratedType
                        | PointerTypeError
                        | DerefNonPointerError DecoratedType
                        | InvalidArraySizeError
                        | IndexNonArrayError
                        | IndexNonLValue
                        | NonIntegralError DecoratedType
                        | NonComptimeError
                        | NonConstArraySizeError 
                        | TypeReconcileError DecoratedType DecoratedType
                        | CallError Text Int Int
                        | AddressError
                        | AssignError
                        | FieldAccessError Text Text
                        | LValueAccessError
                        | LValueCrementError
                        | NonStructFieldAccessError
                        | NameAccessError
                        | HeterogenousArray
                        | ReturnNotInFunctionError
                        | NestedDeclarationError
                        | InvalidModifier Modifier
                        | StatementOutsideDeclarationError 
                        | FunctionNotReturning
                        | CannotComptimeError
                        | CannotInlineError
                        | AssignConstError
                        | NonPureError
                        | CannotInlineInGlobalsError
                        | DeadCode

data VarKind = Global | Local | Formal deriving (Show, Eq, Ord)

showSError :: SemanticsError -> Text -> Text -> String
showSError (SemanticsError l sc ec e) f o
    | l == -1 && sc == -1 && ec == -1 = "This error should be impossible to encounter. If you've found this organically, please file a bug report! The reported error is: " ++ show e
    | otherwise = T.unpack f ++ ":"
                  ++ (show l) ++ ":"
                  ++ (show sc) ++ ":\n"
                  ++ take offset (repeat ' ') ++ "|\n"
                  ++ (show l) ++ " | " ++ ((lines $ T.unpack o) !! (l - 1)) ++ "\n"
                  ++ take offset (repeat ' ') ++ "| "
                  ++ take (sc - 1) (repeat ' ')
                  ++ take repeatAmount (repeat '^') ++ "\n"
                  ++ show e
    where offset = 1 + (length $ show l)
          repeatAmount
              | ec == -1 = T.length ((T.lines o) !! (l - 1)) - sc
              | otherwise = ec - sc

instance Show SemanticsErrorType where
    show (DuplicateDeclaration iden) = "cannot redeclare already declared identifier " ++ T.unpack iden
    show (DuplicateField iden) = "cannot redeclare already declared field " ++ T.unpack iden
    show (VoidVarDeclaration iden) = "cannot declare variable " ++ T.unpack iden ++ " as type void"
    show (UndefinedIdentifier iden) = "cannot reference the undefined identifier " ++ T.unpack iden
    show (TypeError t1 t2) = "expected type " ++ show' t1 ++ ", got type " ++ show' t2
    show (BadTypeError t) = show' t ++ " isn't a legal type here"
    show (CastError t1 t2) = "couldn't cast type " ++ show' t1 ++ " to type " ++ show' t2
    show (ImplicitCastError t1 t2) = "couldn't implicitly cast type " ++ show' t1 ++ " to type " ++ show' t2
    show (IncDecError t) = "can't increment or decrement value of type " ++ show' t
    show (NumericError t) = show' t ++ " isn't a numeric type"
    show PointerTypeError = "can't perform operation on pointer type"
    show (DerefNonPointerError t) = "can't derefence non-pointer type " ++ show' t
    show InvalidArraySizeError = "invalid size for array"
    show IndexNonArrayError = "can't index non-array (possibly indexing an array with too many indices?)"
    show IndexNonLValue = "can't index non-lvalue"
    show (NonIntegralError t) = show' t ++ " is a non-integral type"
    show NonComptimeError = "value of expression isn't known at compile-time"
    show NonConstArraySizeError = "variables used as array sizes must be const"
    show (TypeReconcileError t1 t2) = "can't reconcile types " ++ show' t1 ++ " and " ++ show' t2 ++ " (tried to use these types as operands in binary operation, neither can be implicitly casted to the other)"
    show (CallError iden n1 n2) = "function " ++ T.unpack iden ++ " expected " ++ show n1 ++ " arguments, got " ++ show n2
    show AddressError = "can't take address of non-lvalue"
    show AssignError = "can't assign to a non-lvalue"
    show (FieldAccessError iden1 iden2) = "struct type " ++ T.unpack iden1 ++ " doesn't contain a field named " ++ T.unpack iden2
    show LValueAccessError = "cannot access a field of a non-lvalue"
    show LValueCrementError = "cannot increment / decrement a non-lvalue"
    show NonStructFieldAccessError = "cannot access a field of a non-struct"
    show NameAccessError = "cannot access a field with an expression"
    show HeterogenousArray = "expressions in array literal don't have the same type"
    show ReturnNotInFunctionError = "can't return while outside a function declaration"
    show NestedDeclarationError = "invalid nested declaration"
    show (InvalidModifier Pure) = "pure is an invalid modifier here"
    show (InvalidModifier Const) = "const is an invalid modifier here"
    show (InvalidModifier Inline) = "inline is an invalid modifier here"
    show (InvalidModifier Restrict) = "restrict is an invalid modifier here"
    show StatementOutsideDeclarationError = "statements are illegal outside of declarations"
    show FunctionNotReturning = "function doesn't return the correct type in all control flow paths"
    show CannotComptimeError = "this expression cannot be evaluated at compile-time"
    show CannotInlineError = "this function cannot be inlined (depends on a call to itself)"
    show AssignConstError = "cannot assign to const variable"
    show NonPureError = "this function cannot be marked pure"
    show CannotInlineInGlobalsError = "cannot inline functions inside global variable initialization"
    show DeadCode = "code is never reached"

show'' :: Type -> String
show'' Void = "void"
show'' Bool = "bool"
show'' U8 = "u8"
show'' U16 = "u16"
show'' U32 = "u32"
show'' U64 = "u64"
show'' I8 = "i8"
show'' I16 = "i16"
show'' I32 = "i32"
show'' I64 = "i64"
show'' F32 = "f32"
show'' F64 = "f64"
show'' (StructType t) = T.unpack t

show' :: DecoratedType -> String
show' (PureType t) = show'' t
show' (DerefType t) = '*':(show' t)
show' (ArrayType t w) = (show' t) ++ "[" ++ show w ++ "]"
