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

{-# LANGUAGE OverloadedStrings #-}

module Parser.Parser
    (

    ) where

import Control.Monad (void)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Parser.AST
    
type Parser = Parsec Void Text

rWords :: [Text]
rWords = ["func",
          "struct",
          "undefined",
          "if",
          "else",
          "while",
          "for",
          "switch",
          "case",
          "return",
          "break",
          "continue",
          "pure",
          "const",
          "inline",
          "comptime",
          "register",
          "restrict",
          "bool",
          "u8",
          "u16",
          "u32",
          "u64",
          "i8",
          "i16",
          "i32",
          "i64",
          "f16",
          "f32",
          "f64",
          "true",
          "false"]

pWhite :: Parser ()
pWhite = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

pLexeme :: Parser a -> Parser a
pLexeme = L.lexeme pWhite

pSymbol :: Text -> Parser Text
pSymbol = L.symbol pWhite

pParens :: Parser a -> Parser a
pParens = between (pSymbol "(") (pSymbol ")")

pBraces :: Parser a -> Parser a
pBraces = between (pSymbol "{") (pSymbol "}")

pBrackets :: Parser a -> Parser a
pBrackets = between (pSymbol "[") (pSymbol "]")

pExpect :: Text -> Parser ()
pExpect t = void $ pSymbol t

pRWord :: Text -> Parser ()
pRWord w = (pLexeme . try) (string w *> notFollowedBy alphaNumChar)

pCharLit :: Parser Char
pCharLit = between (pSymbol "'") (pSymbol "'") L.charLiteral

pStringLit :: Parser Text
pStringLit = do
  found <- char '"' *> manyTill L.charLiteral (char '"')
  return $ T.pack found

pIdentifier :: Parser Text
pIdentifier = (pLexeme . try) (p >>= check)
  where p = fmap T.pack $ (:) <$> (letterChar <|> single '_') <*> many (alphaNumChar <|> single '_')
        check x = if x `elem` rWords
                  then fail $ "Keyword " ++ show x ++ " can't be an identifier"
                  else return x

pDecoratedIdentifier :: Parser DecoratedIdentifier
pDecoratedIdentifier = do
  mods <- many pModifier
  iden <- pIdentifier
  pExpect ":"
  typeP <- pDecoratedType
  return $ DecoratedIdentifier mods iden typeP

pInt :: Parser Integer
pInt = pLexeme L.decimal

pFloat :: Parser Double
pFloat = pLexeme L.float

pExpr :: Parser Expression
pExpr = undefined

pType :: Parser Type
pType = Bool <$ pRWord "bool"
            <|> U8 <$ pRWord "u8"
            <|> U16 <$ pRWord "u16"
            <|> U32 <$ pRWord "u32"
            <|> U64 <$ pRWord "u64"
            <|> I8 <$ pRWord "i8"
            <|> I16 <$ pRWord "i16"
            <|> I32 <$ pRWord "i32"
            <|> I64 <$ pRWord "i64"
            <|> F16 <$ pRWord "f8"
            <|> F32 <$ pRWord "f16"
            <|> F64 <$ pRWord "f32"
            <|> StructType <$> pIdentifier

pDecoratedType :: Parser DecoratedType
pDecoratedType = do
  stars <- many $ pExpect "*"
  typeP <- pType
  exprs <- many $ pExpect "[" *> pExpr <* pExpect "]"
  return $ DecoratedType (length stars) typeP exprs

pModifier :: Parser Modifier
pModifier = Pure <$ pRWord "pure"
            <|> Const <$ pRWord "const"
            <|> Inline <$ pRWord "inline"
            <|> Comptime <$ pRWord "comptime"
            <|> Register <$ pRWord "register"
            <|> Restrict <$ pRWord "restrict"
