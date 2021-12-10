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

import qualified Data.Text as T
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char

import Parser.AST
    
type Parser = Parsec Void T.Text

pIdentifier :: Parser T.Text
pIdentifier = undefined

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
  
