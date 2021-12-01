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
import Data.Either
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Read as TR

import Lexer.Token

lexer :: Int -> Int -> T.Text -> Either [T.Text] [Token]
lexer c l text -- (t:tx)
  | T.null text = Right []
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
  | p "//" = lexer (c + numToNextLine) l $ T.drop numToNextLine text
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
  | p "," = th Comma 1
  | p ";" = th Semi 1
  | p ":" = th Colon 1
  | p "." = th Dot 1
  | p "~" = th Tilda 1
  | p "->" = th Arrow 2
  | p "(" = th LeftParen 1
  | p ")" = th RightParen 1
  | p "[" = th LeftBracket 1
  | p "]" = th RightBracket 1
  | p "{" = th LeftBrace 1
  | p "}" = th RightBrace 1
  | p "func" = th Func 4
  | p "struct" = th Struct 6
  | p "undefined" = th Undefined 9
  | p "if" = th If 2
  | p "else" = th Else 4
  | p "while" = th While 5
  | p "for" = th While 3
  | p "switch" = th Switch 6
  | p "case" = th Case 4
  | p "return" = th Return 6
  | p "pure" = th Pure 4
  | p "const" = th Const 5
  | p "inline" = th Inline 6
  | p "comptime" = th Comptime 8
  | p "register" = th Register 8
  | p "restrict" = th Restrict 8
  | p "bool" = th Boolean 7
  | p "u8" = th U8 2
  | p "u16" = th U16 3
  | p "u32" = th U32 3
  | p "u64" = th U64 3
  | p "i8" = th I8 2
  | p "i16" = th I16 3
  | p "i32" = th I32 3
  | p "i64" = th I64 3
  | p "f16" = th F16 3
  | p "f32" = th F32 3
  | p "f64" = th F64 3
  | p "true" = th (BooleanLiteral True) 4
  | p "false" = th (BooleanLiteral True) 5
  | isDigit $ T.head text = if T.head (T.dropWhile isDigit text) == '.' then th (FloatingPointLiteral $ fst $ fromRight undefined $ TR.double $ takeDoub text) $ T.length $ takeDoub text else th (FixedPointLiteral $ fst $ fromRight undefined $ TR.decimal $ takeDeci text) $ T.length $ takeDeci text
  | p "'" = th (toCharLit $ T.unpack $ insideSingleQuotes) $ 2 + (T.length insideSingleQuotes)
  | p "\"" = th (StringLiteral insideDoubleQuotes) $ 2 + (T.length insideDoubleQuotes)
  | isAlpha $ T.head text = th (Identifier $ T.takeWhile (\x -> isAlphaNum x) text) $ T.length $ T.takeWhile (\x -> isAlphaNum x) text
  | otherwise = th (BadToken $ T.pack $ "Unrecognized character " ++ [T.head text] ++ " (occurred at line " ++ (show l) ++ " and column " ++ (show c) ++ ").") 1
    where p s = T.isPrefixOf (T.pack s) text
          th :: TokenType -> Int -> Either [T.Text] [Token]
          th t n = (Token t c l) `comp` (lexer (c + n) l $ T.drop n text)
          comp (Token t _ _) (Left errs)
            | isBadTokenType t = Left ((textFromBadTokenType t):errs)
            | otherwise = Left errs
          comp token@(Token t _ _) (Right tokens)
            | isBadTokenType t = Left [textFromBadTokenType t]
            | otherwise = Right (token:tokens)
          takeDeci t = T.takeWhile isDigit t
          takeDoub t = takeDeci t `T.append` T.singleton '.' `T.append` takeDeci t
          isBadTokenType (BadToken _) = True
          isBadTokenType _ = False
          textFromBadTokenType (BadToken x) = x
          textFromBadTokenType _ = undefined
          toCharLit "\\'" = CharLiteral '\''
          toCharLit "\\\"" = CharLiteral '"'
          toCharLit "\\\\" = CharLiteral '\\'
          toCharLit "\\n" = CharLiteral '\n'
          toCharLit "\\r" = CharLiteral '\r'
          toCharLit "\\t" = CharLiteral '\t'
          toCharLit "\\b" = CharLiteral '\b'
          toCharLit "\\f" = CharLiteral '\f'
          toCharLit "\\v" = CharLiteral '\v'
          toCharLit "\\0" = CharLiteral '\0'
          toCharLit [x] = CharLiteral x
          toCharLit x = BadToken $ T.pack $ x ++ " is not a valid character or character code (occured at line " ++ (show l) ++ " and column " ++ (show c) ++ ")."
          insideSingleQuotes = T.takeWhile (\x -> x /= '\'') $ T.tail text
          insideDoubleQuotes = replaceEscapes $ T.takeWhile (\x -> x /= '"') $ T.tail text
          replaceEscapes = rep "\\'" '\'' .
                           rep "\\\"" '"' .
                           rep "\\\\" '\\' .
                           rep "\\n" '\n' .
                           rep "\\r" '\r' .
                           rep "\\t" '\t' .
                           rep "\\b" '\b' .
                           rep "\\f" '\f' .
                           rep "\\v" '\v' .
                           rep "\\0" '\0'
          rep s ch = T.replace (T.pack s) (T.singleton ch)
          numToNextLine = if isJust numToNextLineMaybe then fromJust numToNextLineMaybe + 1else T.length text
          numToNextLineMaybe = T.findIndex (\x -> x == '\n') text