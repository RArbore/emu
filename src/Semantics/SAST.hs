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

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Semantics.SAST
    (

     SAST (..),
     Structure  (..),
     Function  (..),
     VarBinding  (..),
     Declaration  (..),
     Statement  (..),
     Expression  (..),
     LValue  (..),
     ComptimeValue  (..),
     DecoratedIdentifier (..),
     DecoratedType (..),
     BinaryOp  (..),
     UnaryOp  (..),

     typeOf
     
    ) where

import Control.DeepSeq
    
import qualified Data.ByteString as B
import Data.Text (Text)
import Data.Word

import GHC.Generics (Generic)

import Parser.AST (Type (..), Modifier (..), FixedPointVal (..), FloatingPointVal (..))

newtype SAST = SAST [Declaration] deriving (Show, Generic, NFData)

data Structure = Structure [Modifier] Text [DecoratedIdentifier] deriving (Show, Generic, NFData)

data Function = Function [Modifier] Text [DecoratedIdentifier] DecoratedType Statement deriving (Show, Generic, NFData)

data VarBinding = VarBinding [Modifier] DecoratedIdentifier Expression deriving (Show, Generic, NFData)

data Declaration = StructDecl Structure
                 | FuncDecl Function
                 | VarDecl VarBinding
                 | StatementDecl Statement deriving (Show, Generic, NFData)
    
data Statement = ExpressionStatement Expression
               | IfElseStatement Expression Statement Statement
               | WhileStatement Expression Statement
               | SwitchStatement Expression Statement
               | CaseStatement Expression Statement
               | ReturnStatement Expression
               | BreakStatement
               | ContinueStatement
               | Block [Declaration]
               | EmptyStatement deriving (Show, Generic, NFData)

data Expression = Binary BinaryOp Expression Expression DecoratedType
                | Unary UnaryOp Expression DecoratedType
                | Literal ComptimeValue
                | ArrayLiteral [Expression]
                | Call Text [Expression] DecoratedType
                | LValueExpression LValue
                | Assign AssignOp LValue Expression
                | Address LValue
                | Undefined deriving (Show, Generic, NFData, Eq)

data LValue = Dereference Expression
            | Access LValue Int DecoratedType
            | Identifier Text DecoratedType deriving (Show, Generic, NFData, Eq)

data ComptimeValue = BooleanLiteral Bool
                   | FixedPointLiteral FixedPointVal
                   | FloatingPointLiteral FloatingPointVal deriving (Show, Generic, NFData, Eq)

typeOf :: Expression -> DecoratedType
typeOf (Binary _ _ _ t) = t
typeOf (Unary _ _ t) = t
typeOf (Literal (BooleanLiteral _)) = DecoratedType 0 Bool []
typeOf (Literal (FixedPointLiteral f)) = case f of
                                           U8Val _ -> DecoratedType 0 U8 []
                                           U16Val _ -> DecoratedType 0 U16 []
                                           U32Val _ -> DecoratedType 0 U32 []
                                           U64Val _ -> DecoratedType 0 U64 []
                                           I8Val _ -> DecoratedType 0 I8 []
                                           I16Val _ -> DecoratedType 0 I16 []
                                           I32Val _ -> DecoratedType 0 I32 []
                                           I64Val _ -> DecoratedType 0 I64 []
typeOf (Literal (FloatingPointLiteral f)) = case f of
                                           F32Val _ -> DecoratedType 0 F32 []
                                           F64Val _ -> DecoratedType 0 F64 []
typeOf (ArrayLiteral x) = typeOf $ head x
typeOf (Call _ _ t) = t
typeOf (LValueExpression (Dereference e)) = let (DecoratedType d t a) = typeOf e in DecoratedType (d + 1) t a
typeOf (LValueExpression (Access _ _ t)) = t
typeOf (LValueExpression (Identifier _ t)) = t
typeOf (Assign _ lval _) = typeOf $ LValueExpression lval
typeOf Undefined = DecoratedType 0 Void []

data DecoratedIdentifier = DecoratedIdentifier [Modifier] Text DecoratedType deriving (Show, Generic, NFData)
data DecoratedType = DecoratedType Int Type [ComptimeValue] deriving (Show, Generic, NFData, Eq)
 
data AssignOp = Equals
              | PlusEquals
              | MinusEquals
              | StarEquals
              | SlashEquals
              | PercentEquals
              | LShiftEquals
              | RShiftEquals
              | HatEquals
              | BarEquals
              | AndEquals deriving (Show, Generic, NFData, Eq)
            
data BinaryOp = LogicOr
              | LogicXor
              | LogicAnd
              | BitwiseOr
              | BitwiseXor
              | BitwiseAnd
              | EqualsEquals
              | ExclaEquals
              | Greater
              | Lesser
              | GreaterEquals
              | LesserEquals
              | LShift
              | RShift
              | TermPlus
              | TermMinus
              | FactorStar
              | FactorSlash
              | FactorPercent
              | Dot 
              | Arrow deriving (Show, Generic, NFData, Eq)
                
data UnaryOp = PrePlusPlus
             | PreMinusMinus
             | Plus
             | Minus
             | Excla
             | Tilda
             | Cast DecoratedType
             | PostPlusPlus
             | PostMinusMinus
             | Index [Expression] deriving (Show, Generic, NFData, Eq)
