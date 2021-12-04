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

    ParserState (..),

    parser

  ) where

import qualified Data.Text as T

import qualified Interface.Error as E

import qualified Lexer.Token as LT

import Parser.AST

type Parser t = ParserState -> [LT.Token] -> Either E.Error (t, [LT.Token], ParserState)
data ParserState = ParserState { filename :: T.Text,
                                 line :: Int,
                                 column :: Int } deriving (Show)

parser :: Parser AST
parser s [] = Right (AST [], [], s)
parser s x = do
  (outDecl, tokens, ns) <- decl s x
  (AST nextDecls, leftovers, nns) <- parser ns tokens
  return (AST (outDecl:nextDecls), leftovers, nns)
  
decl :: Parser Decl
decl = undefined

tokenParser :: LT.TokenType -> Parser LT.Token
tokenParser tt s [] = Left $ E.Error
                      (T.pack $ "Couldn't find token of type " ++ (show tt))
                      (filename s)
                      (line s)
                      (column s)
                      (column s + 1)
tokenParser tt (ParserState f l c) (x:xs)
    | tt == (LT.tokenType x) = Right (x, xs, (ParserState nf nl nc))
    | otherwise = undefined
    where (nf, nl, nc)
              | null xs = (f, l + LT.length x, c)
              | otherwise = (f, LT.line $ head xs, LT.column $ head xs)

