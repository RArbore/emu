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

module Lexer.Token
  (

    Token (..),
    TokenType (..)

  ) where

import qualified Data.Text as T

data Token = Token { tokenType :: TokenType,
                     column :: Int,
                     line :: Int } deriving (Show)

data TokenType = Plus
               | Minus
               | Star
               | Slash
               | Percent
               | Excla
               | Hat
               | Bar
               | And
               | HatHat
               | BarBar
               | AndAnd
               | Greater
               | Lesser
               | EqualsEquals
               | GreaterEquals
               | LesserEquals
               | ExclaEquals
               | Comma
               | Semi
               | Colon
               | Dot
               | Tilda
               | Arrow
               | LShift
               | RShift
               | Equals
               | PlusPlus
               | MinusMinus
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
               | LeftParen
               | RightParen
               | LeftBracket
               | RightBracket
               | LeftBrace
               | RightBrace
               | Func
               | Struct
               | Undefined
               | If
               | Else
               | While
               | For
               | Switch
               | Case
               | Return
               | Pure
               | Const
               | Inline
               | Comptime
               | Register
               | Restrict
               | Boolean
               | U8
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
               | BooleanLiteral Bool
               | FixedPointLiteral Integer
               | FloatingPointLiteral Double
               | CharLiteral Char
               | StringLiteral T.Text
               | Identifier T.Text
               | BadToken T.Text
               deriving (Show)
