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

     pProgram

    ) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void

import Debug.Trace

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
          "void",
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
  pSymbol ":"
  typeP <- pDecoratedType
  return $ DecoratedIdentifier mods iden typeP

pInt :: Parser Integer
pInt = pLexeme L.decimal

pFloat :: Parser Double
pFloat = pLexeme L.float

pProgram :: Parser AST
pProgram = AST <$> (pWhite *> many pDeclaration <* eof)

pDeclaration :: Parser Declaration 
pDeclaration = (try pStmtDecl)
               <|> (try pStructDecl)
               <|> (try pFuncDecl)
               <|> pVarDecl
    where pStructDecl = do
            mods <- many pModifier
            pRWord "struct"
            iden <- pIdentifier
            pSymbol "{"
            first <- pDecoratedIdentifier
            rest <- many $ pSymbol "," *> pDecoratedIdentifier
            pSymbol "}"
            pSymbol ";"
            return $ StructDecl mods iden (first:rest)
          pFuncDecl = do
            mods <- many pModifier
            pRWord "func"
            iden <- pIdentifier
            pSymbol "("
            parameters <- option [] (do first <- pDecoratedIdentifier
                                        rest <- many $ pSymbol "," *> pDecoratedIdentifier
                                        return (first:rest))
            pSymbol ")"
            typeP <- option (DecoratedType 0 Void []) (pSymbol ":" *> pDecoratedType)
            stmt <- pStatement
            return $ FuncDecl mods iden parameters typeP stmt
          pVarDecl = do
            mods <- many pModifier
            iden <- pDecoratedIdentifier
            pSymbol "="
            expr <- pExpression
            pSymbol ";"
            return $ VarDecl mods iden expr
          pStmtDecl = StatementDecl <$> pStatement

pStatement :: Parser Statement
pStatement =  pIfElse
             <|> pWhile
             <|> pFor
             <|> pSwitch
             <|> pCase
             <|> pReturn
             <|> pBreak
             <|> pContinue
             <|> pBlock
             <|> pExprStmt
             <|> pEmpty
    where pExprStmt = ExpressionStatement <$> (pExpression <* pSymbol ";")
          pIfElse = do
            pRWord "if"
            pSymbol "("
            expr <- pExpression
            pSymbol ")"
            stmt <- pStatement
            elseBranch <- option (EmptyStatement) (pRWord "else" *> pStatement)
            return $ IfElseStatement expr stmt elseBranch
          pWhile = do
            pRWord "while"
            pSymbol "("
            expr <- pExpression
            pSymbol ")"
            stmt <- pStatement
            return $ WhileStatement expr stmt
          pFor = do
            pRWord "for"
            pSymbol "("
            decl <- pDeclaration
            expr1 <- option (BooleanLiteral True) pExpression
            pSymbol ";"
            expr2 <- option (Undefined) pExpression
            pSymbol ")"
            stmt <- pStatement
            return $ ForStatement decl expr1 expr2 stmt
          pSwitch = do
            pRWord "switch"
            pSymbol "("
            expr <- pExpression
            pSymbol ")"
            stmt <- pStatement
            return $ SwitchStatement expr stmt
          pCase = do
            pRWord "case"
            expr <- pExpression
            pSymbol ":"
            stmt <- pStatement
            return $ CaseStatement expr stmt
          pReturn = ReturnStatement <$> (pRWord "return" *> pExpression <* pSymbol ";")
          pBreak = BreakStatement <$ (do
                                       pRWord "break"
                                       pSymbol ";")
          pContinue = ContinueStatement <$ (do
                                             pRWord "continue"
                                             pSymbol ";")
          pBlock = Block <$> (pSymbol "{" *> (many pDeclaration) <* pSymbol "}")
          pEmpty = EmptyStatement <$ pSymbol ";"
         
pExpression :: Parser Expression
pExpression = makeExprParser pPrimary opTable

opTable :: [[Operator Parser Expression]]
opTable =
    [
     [
      Postfix $ foldr1 (.) . reverse <$> some postfix,
      InfixL $ Binary Dot <$ pSymbol ".",
      InfixL $ Binary Arrow <$ tryPSymbol "->"
     ],
     [Prefix $ foldr1 (.) <$> some prefix],
     [
      InfixL $ Binary FactorStar <$ tryPSymbol "*",
      InfixL $ Binary FactorSlash <$ tryPSymbol "/",
      InfixL $ Binary FactorPercent <$ tryPSymbol "%"
     ],
     [
      InfixL $ Binary TermPlus <$ tryPSymbol "+",
      InfixL $ Binary TermMinus <$ tryPSymbol "-"
     ],
     [
      InfixL $ Binary LShift <$ tryPSymbol "<<",
      InfixL $ Binary RShift <$ tryPSymbol ">>"
     ],
     [
      InfixL $ Binary Greater <$ tryPSymbol ">",
      InfixL $ Binary Lesser <$ tryPSymbol "<",
      InfixL $ Binary GreaterEquals <$ tryPSymbol ">=",
      InfixL $ Binary LesserEquals <$ tryPSymbol "<="
     ],
     [
      InfixL $ Binary EqualsEquals <$ tryPSymbol "==",
      InfixL $ Binary ExclaEquals <$ pSymbol "!="
     ],
     [InfixL $ Binary BitwiseAnd <$ tryPSymbol "&"],
     [InfixL $ Binary BitwiseXor <$ tryPSymbol "^"],
     [InfixL $ Binary BitwiseOr <$ tryPSymbol "|"],
     [InfixL $ Binary LogicAnd <$ tryPSymbol "&&"],
     [InfixL $ Binary LogicXor <$ tryPSymbol "^^"],
     [InfixL $ Binary LogicOr <$ tryPSymbol "||"],
     [
      InfixR $ Binary Equals <$ tryPSymbol "=",
      InfixR $ Binary PlusEquals <$ tryPSymbol "+=",
      InfixR $ Binary MinusEquals <$ tryPSymbol "-=",
      InfixR $ Binary StarEquals <$ tryPSymbol "*=",
      InfixR $ Binary SlashEquals <$ tryPSymbol "/=",
      InfixR $ Binary PercentEquals <$ tryPSymbol "%=",
      InfixR $ Binary LShiftEquals <$ tryPSymbol "<<=",
      InfixR $ Binary RShiftEquals <$ tryPSymbol ">>=",
      InfixR $ Binary HatEquals <$ tryPSymbol "^=",
      InfixR $ Binary BarEquals <$ tryPSymbol "|=",
      InfixR $ Binary AndEquals <$ tryPSymbol "&="
     ]
    ]
    where wTryPSymbol = pLexeme . try . pSymbol
          tryPSymbol sym = pLexeme $ try (pSymbol sym <* notFollowedBy opChar)
          opChar = oneOf ("!#$%&*+./<=>?@\\^|-~" :: String)
          cast = do
            pSymbol "("
            typeP <- pDecoratedType
            pSymbol ")"
            return $ Unary $ Cast typeP
          call = do
            pSymbol "("
            first <- pExpression
            rest <- many $ pSymbol "," *> pExpression
            pSymbol ")"
            return $ Unary $ Call (first:rest)
          index = do
            pSymbol "["
            first <- pExpression
            rest <- many $ pSymbol "," *> pExpression
            pSymbol "]"
            return $ Unary $ Index (first:rest)
          prefix = (Unary PrePlusPlus <$ wTryPSymbol "++")
                   <|> (Unary PreMinusMinus <$ wTryPSymbol "--")
                   <|> (Unary Plus <$ wTryPSymbol "+")
                   <|> (Unary Minus <$ wTryPSymbol "-")
                   <|> (Unary Excla <$ wTryPSymbol "!")
                   <|> (Unary Tilda <$ pSymbol "~")
                   <|> (Unary Star <$ wTryPSymbol "*")
                   <|> (Unary And <$ wTryPSymbol "&")
                   <|> cast
          postfix = (Unary PostPlusPlus <$ wTryPSymbol "++")
                    <|> (Unary PostMinusMinus <$ wTryPSymbol "--")
                    <|> call
                    <|> index
         
pPrimary :: Parser Expression
pPrimary = (pSymbol "(" *> pExpression <* pSymbol ")")
           <|> BooleanLiteral <$> (False <$ pRWord "false" <|> True <$ pRWord "true")
           <|> try (FloatingPointLiteral <$> pFloat)
           <|> FixedPointLiteral <$> pInt
           <|> CharLiteral <$> pCharLit
           <|> StringLiteral <$> pStringLit
           <|> PrimaryIdentifier <$> pIdentifier
           <|> pArrayLiteral
           <|> Undefined <$ pRWord "undefined"
    where pArrayLiteral = do
            pSymbol "{"
            first <- pExpression
            rest <- many $ pSymbol "," *> pExpression 
            pSymbol "}"
            return $ ArrayLiteral (first:rest)

pType :: Parser Type
pType = Void <$ pRWord "void"
            <|> Bool <$ pRWord "bool"
            <|> U8 <$ pRWord "u8"
            <|> U16 <$ pRWord "u16"
            <|> U32 <$ pRWord "u32"
            <|> U64 <$ pRWord "u64"
            <|> I8 <$ pRWord "i8"
            <|> I16 <$ pRWord "i16"
            <|> I32 <$ pRWord "i32"
            <|> I64 <$ pRWord "i64"
            <|> F16 <$ pRWord "f16"
            <|> F32 <$ pRWord "f32"
            <|> F64 <$ pRWord "f64"
            <|> StructType <$> pIdentifier

pDecoratedType :: Parser DecoratedType
pDecoratedType = do
  stars <- many $ pSymbol "*"
  typeP <- pType
  exprs <- many $ pSymbol "[" *> pExpression <* pSymbol "]"
  return $ DecoratedType (length stars) typeP exprs

pModifier :: Parser Modifier
pModifier = Pure <$ pRWord "pure"
            <|> Const <$ pRWord "const"
            <|> Inline <$ pRWord "inline"
            <|> Comptime <$ pRWord "comptime"
            <|> Register <$ pRWord "register"
            <|> Restrict <$ pRWord "restrict"

                
