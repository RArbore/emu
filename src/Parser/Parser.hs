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

pType :: Parser Type
pType = do
  parsed <- Bool <$ string "bool"
            <|> U8 <$ string "u8"
            <|> U16 <$ string "u16"
            <|> U32 <$ string "u32"
            <|> U64 <$ string "u64"
            <|> I8 <$ string "i8"
            <|> I16 <$ string "i16"
            <|> I32 <$ string "i32"
            <|> I64 <$ string "i64"
            <|> F16 <$ string "f8"
            <|> F32 <$ string "f16"
            <|> F64 <$ string "f32"
            <|> StructType <$> pIdentifier
  return parsed
