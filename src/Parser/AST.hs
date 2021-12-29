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

     Location,
     AST (..),
     Declaration (..),
     Statement (..),
     Expression (..),
     Declaration' (..),
     Statement' (..),
     Expression' (..),
     FixedPointVal (..),
     FloatingPointVal (..),
     DecoratedIdentifier (..),
     DecoratedType (..),
     DecoratedType' (..),
     Modifier (..),
     Type (..),
     BinaryOp (..),
     UnaryOp (..)
     
    ) where

import Control.DeepSeq
    
import qualified Data.ByteString as B
import Data.Int
import Data.Text (Text)
import Data.Word

import GHC.Generics (Generic)

type Location = (Int, Int, Int)
newtype AST = AST [Declaration] deriving (Show, Generic, NFData)

type Declaration = (Location, Declaration')
data Declaration' = StructDecl [Modifier] Text [DecoratedIdentifier]
                  | FuncDecl [Modifier] Text [DecoratedIdentifier] DecoratedType Statement
                  | VarDecl DecoratedIdentifier Expression
                  | StatementDecl Statement deriving (Show, Generic, NFData)
    
type Statement = (Location, Statement')
data Statement' = ExpressionStatement Expression
                | IfElseStatement Expression Statement Statement
                | WhileStatement Expression Statement
                | ForStatement Declaration Expression Expression Statement
                | SwitchStatement Expression Statement
                | CaseStatement Expression Statement
                | ReturnStatement Expression
                | BreakStatement
                | ContinueStatement
                | Block [Declaration]
                | EmptyStatement deriving (Show, Generic, NFData)

type Expression = (Location, Expression')
data Expression' = Binary BinaryOp Expression Expression
                 | Unary UnaryOp Expression 
                 | BooleanLiteral Bool
                 | FixedPointLiteral FixedPointVal
                 | FloatingPointLiteral FloatingPointVal
                 | CharLiteral Word8
                 | StringLiteral B.ByteString
                 | PrimaryIdentifier Text
                 | Call Text [Expression]
                 | ArrayLiteral [Expression]
                 | Access Expression Expression
                 | ComptimeExpression Expression
                 | Undefined deriving (Show, Generic, NFData, Eq)

data FixedPointVal = U8Val Word8
                   | U16Val Word16
                   | U32Val Word32
                   | U64Val Word64
                   | I8Val Int8
                   | I16Val Int16
                   | I32Val Int32
                   | I64Val Int64 deriving (Show, Generic, NFData, Eq)

data FloatingPointVal = F32Val Float
                      | F64Val Double deriving (Show, Generic, NFData, Eq)
 
data DecoratedIdentifier = DecoratedIdentifier [Modifier] Text DecoratedType deriving (Show, Generic, NFData)
type DecoratedType = (Location, DecoratedType')
data DecoratedType' = PureType Type
                    | DerefType DecoratedType
                    | ArrayType DecoratedType Expression deriving (Show, Generic, NFData, Eq)

data Modifier = Pure
              | Const
              | Inline
              | Register
              | Restrict deriving (Show, Generic, NFData, Eq)
                
data Type = Void
          | Bool
          | U8
          | U16
          | U32
          | U64
          | I8
          | I16
          | I32
          | I64
          | F32
          | F64
          | StructType Text deriving (Show, Generic, NFData, Eq)
            
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
              | FactorPercent deriving (Show, Generic, NFData, Eq)
                
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
             | Index [Expression] deriving (Show, Generic, NFData, Eq)
