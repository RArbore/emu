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

import qualified Lexer.Token as LT

type AST = [Decl]
data Decl = StructDecl [Modifier] LT.Token Identifier Parameters
          | FuncDecl [Modifier] LT.Token Identifier Parameters DecoratedType Block
          | VarDecl DecoratedIdentifier Expression
          | StatementDecl Statement
data Statement = ExpressionStatement Expression
               | IfElseStatement LT.Token Expression Statement Statement
               | WhileStatement LT.Token Expression Statement
               | ForStatement LT.Token Statement Expression Expression Statement
               | SwitchStatement LT.Token Expression Statement
               | CaseStatement LT.Token Expression Statement
               | ReturnStatement LT.Token Expression
type Expression = Assignment
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

data DecoratedIdentifier = DecoratedIdentifier Modifier Identifier DecoratedType
data DecoratedType = DecoratedType Int Type [Int]
data Parameters = Parameters [DecoratedIdentifier]
data Arguments = Arguments [Expression]
data Modifier = Pure | Const | Inline | Comptime | Register | Restrict
