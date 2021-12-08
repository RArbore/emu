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

module Parser.AST
    (

     AST (..),
     Statement  (..),
     Expression  (..),
     DecoratedIdentifier  (..),
     DecoratedType  (..),
     Modifier  (..),
     Type  (..),
     BinaryOp  (..),
     UnaryOp  (..)
     
    ) where

import Control.DeepSeq
    
import qualified Data.Text as T

import GHC.Generics (Generic)

newtype AST = AST [Statement] deriving (Show, Generic, NFData)
    
data Statement = StructDecl [Modifier] T.Text [DecoratedIdentifier]
               | FuncDecl [Modifier] T.Text [DecoratedIdentifier] DecoratedType Statement
               | VarDecl [Modifier] DecoratedIdentifier Expression
               | ExpressionStatement Expression
               | IfElseStatement Expression Statement Statement
               | WhileStatement Expression Statement
               | ForStatement Statement Expression Expression Statement
               | SwitchStatement Expression Statement
               | CaseStatement Expression Statement
               | ReturnStatement Expression
               | BreakStatement
               | ContinueStatement
               | Block [Statement]
               | EmptyStatement deriving (Show, Generic, NFData)

data Expression = Binary Expression BinaryOp Expression
                | Unary Expression UnaryOp
                | BooleanLiteral Bool
                | FixedPointLiteral Integer
                | FloatingPointLiteral Double
                | CharLiteral Char
                | StringLiteral T.Text
                | PrimaryIdentifier T.Text
                | ArrayLiteral [Expression]
                | Undefined deriving (Show, Generic, NFData)
 
data DecoratedIdentifier = DecoratedIdentifier [Modifier] T.Text DecoratedType deriving (Show, Generic, NFData)
data DecoratedType = DecoratedType Int Type [Expression] deriving (Show, Generic, NFData)
--newtype Identifier = Identifier T.Text deriving (Show, Generic, NFData)
--newtype Parameters = Parameters [DecoratedIdentifier] deriving (Show, Generic, NFData)
--newtype Arguments = Arguments [Expression] deriving (Show, Generic, NFData)
    
data Modifier = Pure
              | Const
              | Inline
              | Comptime
              | Register
              | Restrict deriving (Show, Generic, NFData)
                
data Type = U8
          | U16
          | U32
          | U64
          | I8
          | I16
          | I32
          | I64
          | F16
          | F32
          | F64
          | StructType T.Text deriving (Show, Generic, NFData)
            
data BinaryOp = Equals
              | PlusEquals
              | MinusEquals
              | StarEquals
              | SlashEquals
              | PercentEquals
              | LShiftEquals
              | RShiftEquals
              | HatEquals
              | BarEquals
              | AndEquals
              | LogicOr
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
              | FactorPercent deriving (Show, Generic, NFData)
                
data UnaryOp = PrePlusPlus
             | PreMinusMinus
             | Plus
             | Minus
             | Excla
             | Tilda
             | Star
             | And
             | Cast DecoratedType
             | PostPlusPlus
             | PostMinusMinus
             | Call [Expression]
             | Index [Expression]
             | Dot T.Text
             | Arrow T.Text deriving (Show, Generic, NFData)
