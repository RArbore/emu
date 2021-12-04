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

import qualified Data.Text as T

newtype AST = AST [Decl] deriving (Show)
    
data Decl = StructDecl [Modifier] Identifier Parameters
          | FuncDecl [Modifier] Identifier Parameters DecoratedType Statement
          | VarDecl DecoratedIdentifier Expression
          | StatementDecl Statement deriving (Show)

data Statement = ExpressionStatement Expression
               | IfElseStatement Expression Statement Statement
               | WhileStatement Expression Statement
               | ForStatement Statement Expression Expression Statement
               | SwitchStatement Expression Statement
               | CaseStatement Expression Statement
               | ReturnStatement Expression
               | Block [Statement] deriving (Show)

newtype Expression = Expression Assignment deriving (Show)
data Assignment = Assignment LogicOr AssignOp LogicOr deriving (Show)
data LogicOr = LogicOr [LogicXor] LogicXor deriving (Show)
data LogicXor = LogicXor [LogicAnd] LogicAnd deriving (Show)
data LogicAnd = LogicAnd [BitwiseOr] BitwiseOr deriving (Show)
data BitwiseOr = BitwiseOr [BitwiseXor] BitwiseXor deriving (Show)
data BitwiseXor = BitwiseXor [BitwiseAnd] BitwiseAnd deriving (Show)
data BitwiseAnd = BitwiseAnd [Equality] Equality deriving (Show)
data Equality = Equality [(Comparison, EqualityOp)] Comparison deriving (Show)
data Comparison = Comparison [(Shift, CompareOp)] Shift deriving (Show)
data Shift = Shift [(Term, ShiftOp)] Term deriving (Show)
data Term = Term [(Factor, TermOp)] Factor deriving (Show)
data Factor = Factor [(Prefix, FactorOp)] Prefix deriving (Show)
data Prefix = Prefix [PrefixOp] Postfix deriving (Show)
data Postfix = Postfix Primary [PostfixOp] deriving (Show)
             
data Primary = BooleanLiteral Bool
             | FixedPointLiteral Integer
             | FloatingPointLiteral Double
             | CharLiteral Char
             | StringLiteral T.Text
             | PrimaryIdentifier Identifier
             | Grouping Expression
             | ArrayLiteral [Expression] deriving (Show)
 
data DecoratedIdentifier = DecoratedIdentifier Modifier Identifier DecoratedType deriving (Show)
data DecoratedType = DecoratedType Int Type [Int] deriving (Show)
newtype Identifier = Identifier T.Text deriving (Show)
newtype Parameters = Parameters [DecoratedIdentifier] deriving (Show)
newtype Arguments = Arguments [Expression] deriving (Show)
    
data Modifier = Pure
              | Const
              | Inline
              | Comptime
              | Register
              | Restrict deriving (Show)
                
data Type =  U8
          | U16
          | U32
          | U64
          | I8
          | I16
          | I32
          | I64
          | F16
          | F32
          | F64 deriving (Show)
            
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
              | AndEquals deriving (Show)
                
data EqualityOp = EqualsEquals
                | ExclaEquals deriving (Show)
                  
data CompareOp = Greater
               | Lesser
               | GreaterEquals
               | LesserEquals deriving (Show)
                 
data ShiftOp = LShift
             | RShift deriving (Show)
               
data TermOp = TermPlus
            | TermMinus deriving (Show)
              
data FactorOp = FactorStar
              | FactorSlash
              | FactorPercent deriving (Show)
                
data PrefixOp = PrePlusPlus
              | PreMinusMinus
              | Plus
              | Minus
              | Excla
              | Tilda
              | Star
              | And
              | Cast DecoratedType deriving (Show)
                
data PostfixOp = PostPlusPlus
               | PostMinusMinus
               | Call Arguments
               | Index Arguments
               | Dot Identifier
               | Arrow Identifier deriving (Show)
