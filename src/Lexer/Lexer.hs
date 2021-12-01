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
  | isSpace $ T.head text = lexer (c + 1) l $ T.tail text
  | p "++" = th PlusPlus 2
  | p "+=" = th PlusEquals 2
  | p "+" = th Plus 1
  | p "--" = th MinusMinus 2
  | p "-=" = th MinusEquals 2
  | p "-" = th Minus 1
  | p "*=" = th StarEquals 2
  | p "*" = th Star 1
  | p "/=" = th SlashEquals 2
  | p "/" = th Slash 1
  | p "%=" = th PercentEquals 2
  | p "%" = th Percent 1
  | p "!=" = th ExclaEquals 2
  | p "!" = th Excla 1
  | p "^^" = th HatHat 2
  | p "^=" = th HatEquals 2
  | p "^" = th Hat 1
  | p "||" = th BarBar 2
  | p "|=" = th BarEquals 2
  | p "|" = th Bar 1
  | p "&&" = th AndAnd 2
  | p "&=" = th AndEquals 2
  | p "&" = th And 1
  | p ">>=" = th RShiftEquals 3
  | p ">>" = th RShift 2
  | p ">=" = th GreaterEquals 2
  | p ">" = th Greater 1
  | p "<<=" = th LShiftEquals 3
  | p "<<" = th LShift 2
  | p "<=" = th LesserEquals 2
  | p "<" = th Lesser 1
  | p "==" = th EqualsEquals 2
  | p "=" = th Equals 1
    where p s = T.isPrefixOf (T.pack s) text
          th t n = (Token t c l):(lexer (c + n) l $ T.drop n text)
