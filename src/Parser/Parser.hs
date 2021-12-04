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

module Parser.Parser
  (

    parser

  ) where

import Interface.Error

import Lexer.Token

import Parser.AST

type Parser t = [Token] -> Either Error (t, [Token])

parser :: Parser AST
parser [] = Right (AST [], [])
parser x = do
  (outDecl, tokens) <- decl x
  (AST nextDecls, _) <- parser tokens
  return (AST (outDecl:nextDecls), [])
  
decl :: Parser Decl
decl = undefined
