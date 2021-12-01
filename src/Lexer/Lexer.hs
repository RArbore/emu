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
  | t == '\n' = lexer 0 (l + 1) tx
  | isSpace t = lexer (c + 1) l tx
  | t == '+' && nci '+' = (Token PlusPlus c l):(lexer (c + 2) l ttx)
  | t == '+' && nci '=' = (Token PlusEquals c l):(lexer (c + 2) l ttx)
  | t == '+' = (Token Plus c l):(lexer (c + 1) l ttx)
  | t == '-' && nci '-' = (Token MinusMinus c l):(lexer (c + 2) l ttx)
  | t == '-' && nci '=' = (Token MinusEquals c l):(lexer (c + 2) l ttx)
  | t == '-' = (Token Minus c l):(lexer (c + 1) l ttx)
    where t = T.head text
          tx = T.tail text
          ttx = T.tail tx
          tttx = T.tail ttx
          nci ch = not (T.null tx) && T.head tx == ch -- next character is
