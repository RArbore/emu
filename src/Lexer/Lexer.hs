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

module Lexer.Lexer
  (

    lexer

  ) where

import Data.Char
import qualified Data.Text as T

import Lexer.Token

lexer :: Int -> Int -> T.Text -> [Token]
lexer c l text -- (t:tx)
  | T.null text = []
  | p "\n" = lexer 0 (l + 1) $ T.tail text
  | isSpace $ T.head text = lexHelper 1
  | p "++" = (Token PlusPlus c l):(lexHelper 2)
  | p "+=" = (Token PlusEquals c l):(lexHelper 2)
  | p "+" = (Token Plus c l):(lexHelper 1)
  | p "--" = (Token MinusMinus c l):(lexHelper 2)
  | p "-=" = (Token MinusEquals c l):(lexHelper 2)
  | p "-" = (Token Minus c l):(lexHelper 1)
  | p "*=" = (Token StarEquals c l):(lexHelper 2)
  | p "*" = (Token Star c l):(lexHelper 1)
  | p "/=" = (Token SlashEquals c l):(lexHelper 2)
  | p "/" = (Token Slash c l):(lexHelper 1)
  | p "%=" = (Token PercentEquals c l):(lexHelper 2)
  | p "%" = (Token Percent c l):(lexHelper 1)
  | p "!=" = (Token ExclaEquals c l):(lexHelper 2)
  | p "!" = (Token Excla c l):(lexHelper 1)
  | p "^^" = (Token HatHat c l):(lexHelper 2)
  | p "^=" = (Token HatEquals c l):(lexHelper 2)
  | p "^" = (Token Hat c l):(lexHelper 1)
  | p "||" = (Token BarBar c l):(lexHelper 2)
  | p "|=" = (Token BarEquals c l):(lexHelper 2)
  | p "|" = (Token Bar c l):(lexHelper 1)
  | p "&&" = (Token AndAnd c l):(lexHelper 2)
  | p "&=" = (Token AndEquals c l):(lexHelper 2)
  | p "&" = (Token And c l):(lexHelper 1)
  | p ">>=" = (Token RShiftEquals c l):(lexHelper 3)
  | p ">>" = (Token RShift c l):(lexHelper 2)
  | p ">=" = (Token GreaterEquals c l):(lexHelper 2)
  | p ">" = (Token Greater c l):(lexHelper 1)
  | p "<<=" = (Token LShiftEquals c l):(lexHelper 3)
  | p "<<" = (Token LShift c l):(lexHelper 2)
  | p "<=" = (Token LesserEquals c l):(lexHelper 2)
  | p "<" = (Token Lesser c l):(lexHelper 1)
  | p "==" = (Token EqualsEquals c l):(lexHelper 2)
  | p "=" = (Token Equals c l):(lexHelper 1)
    where p s = T.isPrefixOf (T.pack s) text
          lexHelper n = lexer (c + n) l $ T.drop n text
