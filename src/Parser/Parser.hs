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
  (decls, leftovers, ns) <- sequenceParser decl s x
  return (AST decls, leftovers, ns)
  
decl :: Parser Decl
decl s x = undefined

modifier :: Parser LT.Token
modifier s x = tokenParser LT.Pure s x
               <> tokenParser LT.Const s x
               <> tokenParser LT.Inline s x
               <> tokenParser LT.Comptime s x
               <> tokenParser LT.Register s x
               <> tokenParser LT.Restrict s x
               <> (Left $ E.Error
                      (T.pack $ "Couldn't find modifier token")
                      (filename s)
                      (line s)
                      (column s)
                      (column s + l))
    where l
             | null x = 1
             | otherwise = LT.length $ head x

sequenceParser :: Parser a -> Parser [a]
sequenceParser _ s [] = Right ([], [], s)
sequenceParser p s x = do
  (parsed, tokens, ns) <- p s x
  (nextParsed, leftovers, nns) <- sequenceParser p ns tokens
  return (parsed:nextParsed, leftovers, nns)
  
tokenParser :: LT.TokenType -> Parser LT.Token
tokenParser tt s [] = Left $ E.Error
                      (T.pack $ "Couldn't find token of type " ++ (show tt))
                      (filename s)
                      (line s)
                      (column s)
                      (column s + 1)
tokenParser tt (ParserState f l c) (x:xs)
    | tt == (LT.tokenType x) = Right (x, xs, (ParserState nf nl nc))
    | otherwise = Left $ E.Error
                  (T.pack $ "Couldn't find token of type " ++ (show tt))
                  f l c (c + LT.length x)
    where (nf, nl, nc)
              | null xs = (f, l + LT.length x, c)
              | otherwise = (f, LT.line $ head xs, LT.column $ head xs)

