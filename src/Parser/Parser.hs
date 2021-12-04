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

import Data.Either
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
  (outDecl, nx, ns) <- decl s x
  (AST nextDecls, nnx, nns) <- parser ns nx
  return (AST (outDecl:nextDecls), nnx, nns)

decl :: Parser Decl
decl s x = structDecl <> funcDecl <> varDecl <> statementDecl
    where structDecl = do
            (modifiers, nx, ns) <- sequenceParser modifier s x
            (_, nnx, nns) <- rTokenParser LT.Struct () ns nx
            (identifier, nnnx, nnns) <- identifierParser nns nnx
            (_, nnnnx, nnnns) <- rTokenParser LT.LeftBrace () nnns nnnx
            (params, nnnnnx, nnnnns) <- parameters nnnns nnnnx
            (_, nnnnnnx, nnnnnns) <- rTokenParser LT.RightBrace () nnnnns nnnnnx
            (_, nnnnnnnx, nnnnnnns) <- rTokenParser LT.Semi () nnnnnns nnnnnnx
            return (StructDecl modifiers identifier params, nnnnnnnx, nnnnnnns)
          funcDecl = do
            (modifiers, nx, ns) <- sequenceParser modifier s x
            (_, nnx, nns) <- rTokenParser LT.Func () ns nx
            (identifier, nnnx, nnns) <- identifierParser nns nnx
            (_, nnnnx, nnnns) <- rTokenParser LT.LeftParen () nnns nnnx
            (params, nnnnnx, nnnnns) <- parameters nnnns nnnnx
            (_, nnnnnnx, nnnnnns) <- rTokenParser LT.RightParen () nnnnns nnnnnx
            (_, nnnnnnnx, nnnnnnns) <- rTokenParser LT.Colon () nnnnnns nnnnnnx
            (decType, nnnnnnnnx, nnnnnnnns) <- decoratedType nnnnnnns nnnnnnnx
            (stmt, nnnnnnnnnx, nnnnnnnnns) <- statement nnnnnnnns nnnnnnnnx
            return (FuncDecl modifiers identifier params decType stmt, nnnnnnnnnx, nnnnnnnnns)
          varDecl = do
            (decIden, nx, ns) <- decoratedIdentifier s x
            (_, nnx, nns) <- rTokenParser LT.Equals () ns nx
            (expr, nnnx, nnns) <- expression nns nnx
            (_, nnnnx, nnnns) <- rTokenParser LT.Semi () nnns nnnx
            return (VarDecl decIden expr, nnnnx, nnnns)
          statementDecl = do
            (stmt, nx, ns) <- statement s x
            return (StatementDecl stmt, nx, ns)

statement :: Parser Statement
statement = undefined

expression :: Parser Expression
expression = undefined

parameters :: Parser Parameters
parameters = undefined

decoratedType :: Parser DecoratedType
decoratedType = undefined

decoratedIdentifier :: Parser DecoratedIdentifier
decoratedIdentifier = undefined

modifier :: Parser Modifier
modifier s x = rTokenParser LT.Pure Pure s x
               <> rTokenParser LT.Const Const s x
               <> rTokenParser LT.Inline Inline s x
               <> rTokenParser LT.Comptime Comptime s x
               <> rTokenParser LT.Register Register s x
               <> rTokenParser LT.Restrict Restrict s x
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
sequenceParser p s x
    | isLeft $ p s x = Right ([], x, s)
    | otherwise = Right (found:nextFound, nnx, nns)
    where (found, nx, ns) = fromRight undefined $ p s x
          (nextFound, nnx, nns) = fromRight undefined $ (sequenceParser p) ns nx

identifierParser :: Parser Identifier
identifierParser s [] = Left $ E.Error
                        (T.pack $ "Couldn't find identifier")
                        (filename s)
                        (line s)
                        (column s)
                        (column s + 1)
identifierParser (ParserState f l c) ((LT.Token (LT.Identifier t) _ _ len):xs) = Right (Identifier t, xs, (ParserState nf nl nc))
    where (nf, nl, nc)
              | null xs = (f, l + len, c)
              | otherwise = (f, LT.line $ head xs, LT.column $ head xs)
identifierParser (ParserState f l c) (x:_) = Left $ E.Error
                        (T.pack $ "Couldn't find identifier")
                        f l c (c + LT.length x)

rTokenParser :: LT.TokenType -> a -> Parser a
rTokenParser tt val s x = do
  (_, xs, ns) <- tokenParser tt s x
  return (val, xs, ns)
  
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
