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
     Decl  (..),
     Statement  (..),
     Expression  (..),
     Assignment  (..),
     LogicOr  (..),
     LogicXor  (..),
     LogicAnd  (..),
     BitwiseOr  (..),
     BitwiseXor  (..),
     BitwiseAnd  (..),
     Equality  (..),
     Comparison  (..),
     Shift  (..),
     Term  (..),
     Factor  (..),
     Prefix  (..),
     Postfix  (..),
     Primary  (..),
     DecoratedIdentifier  (..),
     DecoratedType  (..),
     Identifier  (..),
     Parameters  (..),
     Arguments  (..),
     Modifier  (..),
     Type  (..),
     AssignOp  (..),
     EqualityOp  (..),
     CompareOp  (..),
     ShiftOp  (..),
     TermOp  (..),
     FactorOp  (..),
     PrefixOp  (..),
     PostfixOp  (..)
     
    ) where

import Control.DeepSeq
    
import qualified Data.Text as T

import GHC.Generics (Generic)

newtype AST = AST [Decl] deriving (Show, Generic, NFData)
    
data Decl = StructDecl [Modifier] Identifier Parameters
          | FuncDecl [Modifier] Identifier Parameters DecoratedType Statement
          | VarDecl [Modifier] DecoratedIdentifier Expression
          | StatementDecl Statement deriving (Show, Generic, NFData)

data Statement = ExpressionStatement Expression
               | IfElseStatement Expression Statement Statement
               | WhileStatement Expression Statement
               | ForStatement Decl Expression Expression Statement
               | SwitchStatement Expression Statement
               | CaseStatement Expression Statement
               | ReturnStatement Expression
               | BreakStatement
               | ContinueStatement
               | Block [Decl]
               | EmptyStatement deriving (Show, Generic, NFData)

newtype Expression = Expression Assignment deriving (Show, Generic, NFData)
data Assignment = Assignment LogicOr [(AssignOp, LogicOr)] deriving (Show, Generic, NFData)
data LogicOr = LogicOr [LogicXor] LogicXor deriving (Show, Generic, NFData)
data LogicXor = LogicXor [LogicAnd] LogicAnd deriving (Show, Generic, NFData)
data LogicAnd = LogicAnd [BitwiseOr] BitwiseOr deriving (Show, Generic, NFData)
data BitwiseOr = BitwiseOr [BitwiseXor] BitwiseXor deriving (Show, Generic, NFData)
data BitwiseXor = BitwiseXor [BitwiseAnd] BitwiseAnd deriving (Show, Generic, NFData)
data BitwiseAnd = BitwiseAnd [Equality] Equality deriving (Show, Generic, NFData)
data Equality = Equality [(Comparison, EqualityOp)] Comparison deriving (Show, Generic, NFData)
data Comparison = Comparison [(Shift, CompareOp)] Shift deriving (Show, Generic, NFData)
data Shift = Shift [(Term, ShiftOp)] Term deriving (Show, Generic, NFData)
data Term = Term [(Factor, TermOp)] Factor deriving (Show, Generic, NFData)
data Factor = Factor [(Prefix, FactorOp)] Prefix deriving (Show, Generic, NFData)
data Prefix = Prefix [PrefixOp] Postfix deriving (Show, Generic, NFData)
data Postfix = Postfix Primary [PostfixOp] deriving (Show, Generic, NFData)
             
data Primary = BooleanLiteral Bool
             | FixedPointLiteral Integer
             | FloatingPointLiteral Double
             | CharLiteral Char
             | StringLiteral T.Text
             | PrimaryIdentifier Identifier
             | Grouping Expression
             | ArrayLiteral [Expression]
             | Undefined deriving (Show, Generic, NFData)
 
data DecoratedIdentifier = DecoratedIdentifier [Modifier] Identifier DecoratedType deriving (Show, Generic, NFData)
data DecoratedType = DecoratedType Int Type [Expression] deriving (Show, Generic, NFData)
newtype Identifier = Identifier T.Text deriving (Show, Generic, NFData)
newtype Parameters = Parameters [DecoratedIdentifier] deriving (Show, Generic, NFData)
newtype Arguments = Arguments [Expression] deriving (Show, Generic, NFData)
    
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
          | StructType Identifier deriving (Show, Generic, NFData)
            
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
              | AndEquals deriving (Show, Generic, NFData)
                
data EqualityOp = EqualsEquals
                | ExclaEquals deriving (Show, Generic, NFData)
                  
data CompareOp = Greater
               | Lesser
               | GreaterEquals
               | LesserEquals deriving (Show, Generic, NFData)
                 
data ShiftOp = LShift
             | RShift deriving (Show, Generic, NFData)
               
data TermOp = TermPlus
            | TermMinus deriving (Show, Generic, NFData)
              
data FactorOp = FactorStar
              | FactorSlash
              | FactorPercent deriving (Show, Generic, NFData)
                
data PrefixOp = PrePlusPlus
              | PreMinusMinus
              | Plus
              | Minus
              | Excla
              | Tilda
              | Star
              | And
              | Cast DecoratedType deriving (Show, Generic, NFData)
                
data PostfixOp = PostPlusPlus
               | PostMinusMinus
               | Call Arguments
               | Index Arguments
               | Dot Identifier
               | Arrow Identifier deriving (Show, Generic, NFData)
