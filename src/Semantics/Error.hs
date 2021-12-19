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

import Parser.AST
     
data SemanticsError = SemanticsError { line :: Int,
                                       startCol :: Int,
                                       endCol :: Int,
                                       errorType :: SemanticsErrorType }

data SemanticsErrorType = DuplicateDeclaration Text VarKind
                        | VoidVarDeclaration Text
                        | UndefinedIdentifier Text
                        | TypeError Type Type
                        | CastError Type Type
                        | CallError Text Int Int
                        | AddressError
                        | AssignError
                        | ImproperIdentifier Text
                        | FieldAccessError Text Text
                        | LValueAccessError
                        | NameAccessError
                        | HeterogenousArray
                        | DeadCode

data VarKind = Global | Local | Formal | StructField Text

showSError :: SemanticsError -> Text -> Text -> String
showSError (SemanticsError l sc ec e) f o
    | l == -1 && sc == -1 && ec == -1 = "This error should be impossible to encounter. If you've found this organically, please file a bug report!"
    | otherwise = T.unpack f ++ ":"
                  ++ (show l) ++ ":"
                  ++ (show sc) ++ ":\n"
                  ++ take offset (repeat ' ') ++ "|\n"
                  ++ (show l) ++ " | " ++ ((lines $ T.unpack o) !! l)
                  ++ take offset (repeat ' ') ++ "| "
                  ++ take sc (repeat ' ')
                  ++ take repeatAmount (repeat '^') ++ "\n"
                  ++ show e
    where offset = 1 + (length $ show l)
          repeatAmount
              | ec == -1 = T.length o - sc
              | otherwise = ec - sc

instance Show SemanticsErrorType where
    show (DuplicateDeclaration iden Global) = "cannot redeclare already declared global identifier " ++ T.unpack iden
    show (DuplicateDeclaration iden Local) = "cannot redeclare already declared local identifier " ++ T.unpack iden
    show (DuplicateDeclaration iden Formal) = "cannot redeclare already declared formal identifier " ++ T.unpack iden
    show (DuplicateDeclaration iden (StructField name)) = "cannot redeclare already declared structure field identifier " ++ T.unpack iden ++ " for structure " ++ T.unpack name
    show (VoidVarDeclaration iden) = "cannot declare variable " ++ T.unpack iden ++ " as type void"
    show (UndefinedIdentifier iden) = "cannot reference the undefined identifier " ++ T.unpack iden
    show (TypeError t1 t2) = "expected type " ++ show' t1 ++ ", got type " ++ show' t2
    show (CastError t1 t2) = "couldn't case type " ++ show' t1 ++ " to type " ++ show' t2
    show (CallError iden n1 n2) = "function " ++ T.unpack iden ++ " expected " ++ show n1 ++ " arguments, got " ++ show n2
    show AddressError = "can't take address of non-lvalue"
    show AssignError = "can't assign to a non-lvalue"
    show (ImproperIdentifier iden) = "improperly used identifier " ++ T.unpack iden
    show (FieldAccessError iden1 iden2) = "struct type " ++ T.unpack iden1 ++ " doesn't contain a field named " ++ T.unpack iden2
    show LValueAccessError = "cannot access a field of a non-lvalue"
    show NameAccessError = "cannot access a field with an expression"
    show HeterogenousArray = "expressions in array literal don't have the same type"
    show DeadCode = "code is never reached"

show' :: Type -> String
show' Void = "void"
show' Bool = "bool"
show' U8 = "u8"
show' U16 = "u16"
show' U32 = "u32"
show' U64 = "u64"
show' I8 = "i8"
show' I16 = "i16"
show' I32 = "i32"
show' I64 = "i64"
show' F32 = "f32"
show' F64 = "f64"
show' (StructType t) = T.unpack t
