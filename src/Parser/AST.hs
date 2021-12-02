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

    AST (..)

  ) where

import qualified Data.Text as T

newtype AST = AST [Decl]
data Decl = StructDecl [Modifier] Identifier Parameters
          | FuncDecl [Modifier] Identifier Parameters DecoratedType Statement
          | VarDecl DecoratedIdentifier Expression
          | StatementDecl Statement

data Statement = ExpressionStatement Expression
               | IfElseStatement Expression Statement Statement
               | WhileStatement Expression Statement
               | ForStatement Statement Expression Expression Statement
               | SwitchStatement Expression Statement
               | CaseStatement Expression Statement
               | ReturnStatement Expression
               | Block [Statement]

newtype Expression = Expression Assignment
data Assignment = Assignment LogicOr AssignOp LogicOr
data LogicOr = LogicOr [LogicXor] LogicXor
data LogicXor = LogicXor [LogicAnd] LogicAnd
data LogicAnd = LogicAnd [BitwiseOr] BitwiseOr
data BitwiseOr = BitwiseOr [BitwiseXor] BitwiseXor
data BitwiseXor = BitwiseXor [BitwiseAnd] BitwiseAnd
data BitwiseAnd = BitwiseAnd [Equality] Equality
data Equality = Equality [(Comparison, EqualityOp)] Comparison
data Comparison = Comparison [(Shift, CompareOp)] Shift
data Shift = Shift [(Term, ShiftOp)] Term
data Term = Term [(Factor, TermOp)] Factor
data Factor = Factor [(Prefix, FactorOp)] Prefix
data Prefix = Prefix [PrefixOp] Postfix
data Postfix = Postfix Primary [PostfixOp]
data Primary = BooleanLiteral Bool
             | FixedPointLiteral Integer
             | FloatingPointLiteral Double
             | CharLiteral Char
             | StringLiteral T.Text
             | PrimaryIdentifier Identifier
             | Grouping Expression
             | ArrayLiteral [Expression]
newtype Identifier = Identifier T.Text

data DecoratedIdentifier = DecoratedIdentifier Modifier Identifier DecoratedType
data DecoratedType = DecoratedType Int Type [Int]
newtype Parameters = Parameters [DecoratedIdentifier]
newtype Arguments = Arguments [Expression]
data Modifier = Pure | Const | Inline | Comptime | Register | Restrict
data Type =  U8 | U16 | U32 | U64 | I8 | I16 | I32 | I64 | F16 | F32 | F64

data AssignOp = Equals | PlusEquals | MinusEquals | StarEquals | SlashEquals | PercentEquals | LShiftEquals | RShiftEquals | HatEquals | BarEquals | AndEquals
data EqualityOp = EqualsEquals | ExclaEquals
data CompareOp = Greater | Lesser | GreaterEquals | LesserEquals
data ShiftOp = LShift | RShift
data TermOp = TermPlus | TermMinus
data FactorOp = FactorStar | FactorSlash | FactorPercent
data PrefixOp = PrePlusPlus | PreMinusMinus | Plus | Minus | Excla | Tilda | Star | And | Cast DecoratedType
data PostfixOp = PostPlusPlus | PostMinusMinus | Call Arguments | Index Arguments | Dot Identifier | Arrow Identifier
