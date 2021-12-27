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

import Data.Char (ord)
import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
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

posEx :: Parser (Int, Int)
posEx = do
  a <- unPos <$> sourceLine <$> getSourcePos
  b <- unPos <$> sourceColumn <$> getSourcePos
  return (a, b)

form :: Int -> Int -> Int -> Int -> Location
form sl sc el ec
    | sl == el = (sl, sc, ec)
    | otherwise = (sl, sc, -1)

locWrap :: Parser a -> Parser (Location, a)
locWrap p = do
  (sl, sc) <- posEx
  x <- p
  (el, ec) <- posEx
  return (form sl sc el ec, x)

impossible :: a -> (Location, a)
impossible x = ((-1, -1, -1), x)

pDeclaration :: Parser Declaration 
pDeclaration = (try pStmtDecl)
               <|> locWrap (try pStructDecl)
               <|> locWrap (try pFuncDecl)
               <|> locWrap pVarDecl
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
            (sl, sc) <- posEx
            typeP <- option ((sl, sc, sc), PureType Void) (pSymbol ":" *> pDecoratedType)
            stmt <- pStatement
            return $ FuncDecl mods iden parameters typeP stmt
          pVarDecl = do
            (sl, sc) <- posEx
            mods <- many pModifier
            iden <- pDecoratedIdentifier
            pSymbol "="
            expr <- pExpression
            pSymbol ";"
            (el, ec) <- posEx
            return $ VarDecl mods iden expr
          pStmtDecl = do
            stmt@(loc, _) <- pStatement
            return (loc, StatementDecl stmt)

pStatement :: Parser Statement
pStatement = locWrap $ pIfElse
             <|> pWhile
             <|> pFor
             <|> pSwitch
             <|> pCase
             <|> pReturn
             <|> pBreak
             <|> pContinue
             <|> (try pBlock)
             <|> pExprStmt
             <|> pEmpty
    where pExprStmt = ExpressionStatement <$> (pExpression <* pSymbol ";")
          pIfElse = do
            pRWord "if"
            pSymbol "("
            expr <- pExpression
            pSymbol ")"
            stmt <- pStatement
            elseBranch <- option (impossible EmptyStatement) (pRWord "else" *> pStatement)
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
            expr1 <- option (impossible $ BooleanLiteral True) pExpression
            pSymbol ";"
            expr2 <- option (impossible Undefined) pExpression
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
      InfixL $ (locWrapBin $ Access) <$ pSymbol ".",
      InfixL $ (locWrapBin $ (\e1 e2 -> Access (locWrapUn (Unary Star) e1) e2)) <$ tryPSymbol "->"
     ],
     [Prefix $ foldr1 (.) <$> some prefix],
     [
      InfixL $ (locWrapBin $ Binary FactorStar) <$ tryPSymbol "*",
      InfixL $ (locWrapBin $ Binary FactorSlash) <$ tryPSymbol "/",
      InfixL $ (locWrapBin $ Binary FactorPercent) <$ tryPSymbol "%"
     ],
     [
      InfixL $ (locWrapBin $ Binary TermPlus) <$ tryPSymbol "+",
      InfixL $ (locWrapBin $ Binary TermMinus) <$ tryPSymbol "-"
     ],
     [
      InfixL $ (locWrapBin $ Binary LShift) <$ tryPSymbol "<<",
      InfixL $ (locWrapBin $ Binary RShift) <$ tryPSymbol ">>"
     ],
     [
      InfixL $ (locWrapBin $ Binary Greater) <$ tryPSymbol ">",
      InfixL $ (locWrapBin $ Binary Lesser) <$ tryPSymbol "<",
      InfixL $ (locWrapBin $ Binary GreaterEquals) <$ tryPSymbol ">=",
      InfixL $ (locWrapBin $ Binary LesserEquals) <$ tryPSymbol "<="
     ],
     [
      InfixL $ (locWrapBin $ Binary EqualsEquals) <$ tryPSymbol "==",
      InfixL $ (locWrapBin $ Binary ExclaEquals) <$ pSymbol "!="
     ],
     [InfixL $ (locWrapBin $ Binary BitwiseAnd) <$ tryPSymbol "&"],
     [InfixL $ (locWrapBin $ Binary BitwiseXor) <$ tryPSymbol "^"],
     [InfixL $ (locWrapBin $ Binary BitwiseOr) <$ tryPSymbol "|"],
     [InfixL $ (locWrapBin $ Binary LogicAnd) <$ tryPSymbol "&&"],
     [InfixL $ (locWrapBin $ Binary LogicXor) <$ tryPSymbol "^^"],
     [InfixL $ (locWrapBin $ Binary LogicOr) <$ tryPSymbol "||"],
     [
      InfixR $ (locWrapBin $ Binary Equals) <$ tryPSymbol "=",
      InfixR $ (locWrapBin $ Binary PlusEquals) <$ tryPSymbol "+=",
      InfixR $ (locWrapBin $ Binary MinusEquals) <$ tryPSymbol "-=",
      InfixR $ (locWrapBin $ Binary StarEquals) <$ tryPSymbol "*=",
      InfixR $ (locWrapBin $ Binary SlashEquals) <$ tryPSymbol "/=",
      InfixR $ (locWrapBin $ Binary PercentEquals) <$ tryPSymbol "%=",
      InfixR $ (locWrapBin $ Binary LShiftEquals) <$ tryPSymbol "<<=",
      InfixR $ (locWrapBin $ Binary RShiftEquals) <$ tryPSymbol ">>=",
      InfixR $ (locWrapBin $ Binary HatEquals) <$ tryPSymbol "^=",
      InfixR $ (locWrapBin $ Binary BarEquals) <$ tryPSymbol "|=",
      InfixR $ (locWrapBin $ Binary AndEquals) <$ tryPSymbol "&="
     ]
    ]
    where locWrapBin b x@((sl, sc, _), _) y@((el, _, ec), _) = (ln, b x y)
              where ln
                        | sl == el = (sl, sc, ec)
                        | otherwise = (sl, sc, -1)
          locWrapUn u x@((l, sc, ec), _) = ((l, sc, ec), u x)
          wTryPSymbol = pLexeme . try . pSymbol
          tryPSymbol sym = pLexeme $ try (pSymbol sym <* notFollowedBy opChar)
          opChar = oneOf ("!#$%&*+./<=>?@\\^|-~" :: String)
          cast = do
            pSymbol "("
            typeP <- pDecoratedType
            pSymbol ")"
            return $ locWrapUn $ Unary $ Cast typeP
          index = do
            pSymbol "["
            first <- pExpression
            rest <- many $ pSymbol "," *> pExpression
            pSymbol "]"
            return $ locWrapUn $ Unary $ Index (first:rest)
          prefix = ((locWrapUn $ Unary PrePlusPlus) <$ wTryPSymbol "++")
                   <|> ((locWrapUn $ Unary PreMinusMinus) <$ wTryPSymbol "--")
                   <|> ((locWrapUn $ Unary Plus) <$ wTryPSymbol "+")
                   <|> ((locWrapUn $ Unary Minus) <$ wTryPSymbol "-")
                   <|> ((locWrapUn $ Unary Excla) <$ wTryPSymbol "!")
                   <|> ((locWrapUn $ Unary Tilda) <$ pSymbol "~")
                   <|> ((locWrapUn $ Unary Star) <$ wTryPSymbol "*")
                   <|> ((locWrapUn $ Unary And) <$ wTryPSymbol "&")
                   <|> cast
          postfix = ((locWrapUn $ Unary PostPlusPlus) <$ wTryPSymbol "++")
                    <|> ((locWrapUn $ Unary PostMinusMinus) <$ wTryPSymbol "--")
                    <|> index
         
pPrimary :: Parser Expression
pPrimary = locWrap $ grouping
           <|> BooleanLiteral <$> (False <$ pRWord "false" <|> True <$ pRWord "true")
           <|> try (FloatingPointLiteral . F64Val <$> pFloat)
           <|> fixedPoint
           <|> char
           <|> StringLiteral . encodeUtf8 <$> pStringLit
           <|> try call
           <|> PrimaryIdentifier <$> pIdentifier
           <|> pArrayLiteral
           <|> Undefined <$ pRWord "undefined"
    where grouping = do
            pSymbol "("
            (_, expr) <- pExpression
            pSymbol ")"
            return expr
          fixedPoint = do
            int <- pInt
            if int <= 0 || int == fromIntegral ((fromIntegral int) :: Int64) then return $ FixedPointLiteral $ I64Val $ fromIntegral int else return $ FixedPointLiteral $ U64Val $ fromIntegral int
          char = do
            c <- pCharLit
            let w = ord c
            if w >= 0 && w < 256 then return $ CharLiteral $ fromIntegral w else fail (c:" isn't an ASCII character, and thus can't be fit into a U8")
          pArrayLiteral = do
            pSymbol "{"
            first <- pExpression
            rest <- many $ pSymbol "," *> pExpression 
            pSymbol "}"
            return $ ArrayLiteral (first:rest)
          call = do
            iden <- pIdentifier
            pSymbol "("
            args <- option [] (do first <- pExpression
                                  rest <- many $ pSymbol "," *> pExpression
                                  return (first:rest))
            pSymbol ")"
            return $ Call iden args

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
            <|> F32 <$ pRWord "f32"
            <|> F64 <$ pRWord "f64"
            <|> StructType <$> pIdentifier

pDecoratedType :: Parser DecoratedType
pDecoratedType = try arrayType <|> grouping <|> pureType <|> derefType 
    where grouping = pSymbol "(" *> pDecoratedType <* pSymbol ")"
          pureType :: Parser DecoratedType
          pureType = locWrap (PureType <$> pType)
          derefType :: Parser DecoratedType
          derefType = locWrap (DerefType <$> (pSymbol "*" *> pDecoratedType))
          arrayType :: Parser DecoratedType
          arrayType = do
            t <- grouping <|> pureType <|> derefType 
            exprs <- many $ pSymbol "[" *> pExpression <* pSymbol "]"
            return $ formArrayType t $ reverse exprs
                where formArrayType it [] = it
                      formArrayType it@((l, sc, ec), t) xs = ((l, sc, endCol (last xs)), ArrayType (formArrayType it $ init xs) (last xs))
                      endCol ((_, _, ec), _) = ec
            
pModifier :: Parser Modifier
pModifier = Pure <$ pRWord "pure"
            <|> Const <$ pRWord "const"
            <|> Inline <$ pRWord "inline"
            <|> Register <$ pRWord "register"
            <|> Restrict <$ pRWord "restrict"
