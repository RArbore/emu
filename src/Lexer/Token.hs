{-  This file is part of rwm.
    rwm is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.
    rwm is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    You should have received a copy of the GNU General Public License
    along with rwm. If not, see <https://www.gnu.org/licenses/>.  -}

module Lexer.Token
  (

    Token (..),
    TokenType (..)

  ) where

data Token = Token { tokenType :: TokenType,
                     line :: Int,
                     column :: Int } deriving (Show)
data TokenType = Plus
               | Minus
               | Star
               | Slash
               | Hat
               | Bar
               | And
               | DoubleHat
               | DoubleBar
               | DoubleAnd
               | Equals
               | Greater
               | Lesser
               | DoubleEquals
               | GreaterEquals
               | LesserEquals
               deriving (Show, Enum)
