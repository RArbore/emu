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

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Parser.Parser
    (

     ParserState (..),

     parser

    ) where

import Control.DeepSeq

import Data.Either
import Data.List
import Data.Maybe
import qualified Data.Text as T

import Debug.Trace

import GHC.Generics (Generic)

import qualified Interface.Error as E

import qualified Lexer.Token as LT

import Parser.AST

parseErrorMessage :: T.Text
parseErrorMessage = T.pack $ "Parse error"

type Parser t = ParserState -> [LT.Token] -> Either E.Error (t, [LT.Token], ParserState)
data ParserState = ParserState { filename :: T.Text,
                                 line :: Int,
                                 column :: Int } deriving (Show, Generic, NFData)

parser :: Parser AST
parser s [] = Right (AST [], [], s)
parser s x = do
  (outDecl, nx, ns) <- decl s x
  (AST nextDecls, nnx, nns) <- parser ns nx
  return (AST (outDecl:nextDecls), nnx, nns)

(<->) :: Parser a -> Parser b -> Parser (b, a)
(<->) pa pb s x = do
  (aa, nx, ns) <- pa s x
  (bb, nnx, nns) <- pb ns nx 
  return ((bb, aa), nnx, nns)
infixr <->

(<!>) :: Either a b -> Either a b -> Either a b
(<!>) (Right x) _ = Right x
(<!>) _ x = x

(<?>) :: Parser a -> Parser b -> Parser (b, a)
(<?>) pa pb s x = do
  (aa, nx, ns) <- pa s x
  (bb, nnx, nns) <- pb ns nx 
  return $ traceShow (map LT.tokenType x, map LT.tokenType nx, map LT.tokenType nnx) $ ((bb, aa), nnx, nns)
infixr <?>

decl :: Parser Statement
decl s x = statementDecl <!> structDecl <!> funcDecl <!> varDecl
    where structDecl = do
            (((((((_, _), params), _), identifier), _), modifiers), nx, ns)
              <- (sequenceParser modifier
                 <-> rTokenParser LT.Struct ()
                 <-> identifierParser
                 <-> rTokenParser LT.LeftBrace ()
                 <-> parameters
                 <-> rTokenParser LT.RightBrace ()
                 <-> rTokenParser LT.Semi ()) s x
            return (StructDecl modifiers identifier params, nx, ns)
          funcDecl = do
            (((((((((stmt, decType), _), _), params), _), identifier), _), modifiers), nx, ns)
              <- (sequenceParser modifier
                 <-> rTokenParser LT.Func ()
                 <-> identifierParser
                 <-> rTokenParser LT.LeftParen ()
                 <-> (tolerate [] parameters)
                 <-> rTokenParser LT.RightParen ()
                 <-> rTokenParser LT.Colon ()
                 <-> decoratedType
                 <-> statement) s x
            return (FuncDecl modifiers identifier params decType stmt, nx, ns)
          varDecl = do
            (((((_, expr), _), decIden), modifiers), nx, ns)
              <- (sequenceParser modifier
                 <-> decoratedIdentifier
                 <-> rTokenParser LT.Equals ()
                 <-> expression
                 <-> rTokenParser LT.Semi ()) s x
            return (VarDecl modifiers decIden expr, nx, ns)
          statementDecl = statement s x

statement :: Parser Statement
statement s x = exprStmt <!> ifElseStmt <!> whileStmt <!> forStmt <!> switchStmt <!> caseStmt <!> retStmt <!> breakStmt <!> contStmt <!> blockStmt <!> rTokenParser LT.Semi EmptyStatement s x
    where exprStmt = do
            ((_, expr), nx, ns)
              <- (expression
                 <-> rTokenParser LT.Semi ()) s x
            return (ExpressionStatement expr, nx, ns)
          ifElseStmt = do
            (((((stmt, _), expr), _), _), nx, ns)
              <- (rTokenParser LT.If ()
                 <-> rTokenParser LT.LeftParen ()
                 <-> expression
                 <-> rTokenParser LT.RightParen ()
                 <-> statement) s x
            (success, nnx, nns) <- (tolerate False $ rTokenParser LT.Else True) ns nx
            if success then do (stmtElse, nnnx, nnns) <- statement nns nnx
                               return (IfElseStatement expr stmt stmtElse, nnnx, nnns)
            else return (IfElseStatement expr stmt (Block []), nnx, nns)
          whileStmt = do
            (((((stmt, _), expr), _), _), nx, ns)
              <- (rTokenParser LT.While ()
                 <-> rTokenParser LT.LeftParen ()
                 <-> expression
                 <-> rTokenParser LT.RightParen ()
                 <-> statement) s x
            return (WhileStatement expr stmt, nx, ns)
          forStmt = do
            (((((((((stmtBody), _), exprInc), _), exprCond), declInit), _), _), nx, ns)
              <- (rTokenParser LT.For ()
                 <-> rTokenParser LT.LeftParen ()
                 <-> decl
                 <-> expression
                 <-> rTokenParser LT.Semi()
                 <-> expression
                 <-> rTokenParser LT.RightParen ()
                 <-> statement) s x
            return (ForStatement declInit exprCond exprInc stmtBody, nx, ns)
          switchStmt = do
            ((((((stmt), _), expr), _), _), nx, ns)
              <- (rTokenParser LT.Switch ()
                 <-> rTokenParser LT.LeftParen ()
                 <-> expression
                 <-> rTokenParser LT.RightParen ()
                 <-> statement) s x
            return (SwitchStatement expr stmt, nx, ns)
          caseStmt = do
            (((((stmt), _), expr), _), nx, ns)
              <- (rTokenParser LT.Case ()
                 <-> expression
                 <-> rTokenParser LT.Colon ()
                 <-> statement) s x
            return (CaseStatement expr stmt, nx, ns)
          retStmt = do
            (((_, expr), _), nx, ns)
              <- (rTokenParser LT.Return ()
                 <-> expression
                 <-> rTokenParser LT.Semi ()) s x
            return (ReturnStatement expr, nx ,ns)
          breakStmt = do
            ((_, _), nx, ns)
              <- (rTokenParser LT.Break ()
                 <-> rTokenParser LT.Semi ()) s x
            return (BreakStatement, nx ,ns)
          contStmt = do
            ((_, _), nx, ns)
              <- (rTokenParser LT.Continue ()
                 <-> rTokenParser LT.Semi ()) s x
            return (ContinueStatement, nx ,ns)
          blockStmt = do
            (((_, decls), _), nx, ns)
              <- (rTokenParser LT.LeftBrace ()
                 <-> sequenceParser decl
                 <-> rTokenParser LT.RightBrace()) s x
            return (Block decls, nx, ns)

expression :: Parser Expression
expression = assignment

assignment :: Parser Expression
assignment s x = do
  ((t, h), nx, ns)
    <- (logicOr
       <-> (sequenceParser $ assignOp <-> logicOr)) s x
  return (reverseBinary $ constructTree (reverse t) h, nx, ns)
    where reverseBinary (Binary expr2 op expr1) = case op of
                                                    Equals -> Binary expr1 Equals (reverseBinary expr2)
                                                    PlusEquals -> Binary expr1 PlusEquals (reverseBinary expr2)
                                                    MinusEquals -> Binary expr1 MinusEquals (reverseBinary expr2)
                                                    StarEquals -> Binary expr1 StarEquals (reverseBinary expr2)
                                                    SlashEquals -> Binary expr1 SlashEquals (reverseBinary expr2)
                                                    PercentEquals -> Binary expr1 PercentEquals (reverseBinary expr2)
                                                    LShiftEquals -> Binary expr1 LShiftEquals (reverseBinary expr2)
                                                    RShiftEquals -> Binary expr1 RShiftEquals (reverseBinary expr2)
                                                    HatEquals -> Binary expr1 HatEquals (reverseBinary expr2)
                                                    BarEquals -> Binary expr1 BarEquals (reverseBinary expr2)
                                                    AndEquals -> Binary expr1 AndEquals (reverseBinary expr2)
                                                    _ -> Binary (reverseBinary expr2) op expr1
          reverseBinary a = a

logicOr :: Parser Expression
logicOr = ltrSingleOpParser logicXor LogicOr LT.BarBar

logicXor :: Parser Expression
logicXor = ltrSingleOpParser logicAnd LogicXor LT.HatHat

logicAnd :: Parser Expression
logicAnd = ltrSingleOpParser bitwiseOr LogicAnd LT.AndAnd

bitwiseOr :: Parser Expression
bitwiseOr = ltrSingleOpParser bitwiseXor BitwiseOr LT.Bar

bitwiseXor :: Parser Expression
bitwiseXor = ltrSingleOpParser bitwiseAnd BitwiseXor LT.Hat

bitwiseAnd :: Parser Expression
bitwiseAnd = ltrSingleOpParser equality BitwiseAnd LT.And

equality :: Parser Expression
equality = ltrOpParser comparison equalityOp

comparison :: Parser Expression
comparison = ltrOpParser shift comparisonOp

shift :: Parser Expression
shift = ltrOpParser term shiftOp

term :: Parser Expression
term = ltrOpParser factor termOp

factor :: Parser Expression
factor = ltrOpParser prefix factorOp

prefix :: Parser Expression
prefix s x = (do
  ((post, ops), nx, ns)
    <- ((sequenceParser prefixOp)
       <-> postfix) s x
  return (foldr (\a b -> Unary b a) post ops, nx, ns)) <!> postfix s x

postfix :: Parser Expression
postfix s x = do
  ((ops, prim), nx, ns)
    <- (primary
       <-> (sequenceParser postfixOp)) s x
  return (foldl' Unary prim ops, nx, ns)

primary :: Parser Expression
primary sp xp = primaryIdentifier <!> grouping <!> arrayLiteral <!> booleanLiteral <!> fixedPointLiteral <!> floatingPointLiteral <!> charLiteral <!> stringLiteral <!>  undefinedParser
    where booleanLiteral = literalExtract (\x -> case x of
                                                   LT.BooleanLiteral b -> Just b
                                                   _ -> Nothing) BooleanLiteral sp xp
          fixedPointLiteral = literalExtract (\x -> case x of
                                                      LT.FixedPointLiteral n -> Just n
                                                      _ -> Nothing) FixedPointLiteral sp xp
          floatingPointLiteral = literalExtract (\x -> case x of
                                                         LT.FloatingPointLiteral n -> Just n
                                                         _ -> Nothing) FloatingPointLiteral sp xp
          charLiteral = literalExtract (\x -> case x of
                                                LT.CharLiteral c -> Just c
                                                _ -> Nothing) CharLiteral sp xp
          stringLiteral = literalExtract (\x -> case x of
                                                  LT.StringLiteral c -> Just c
                                                  _ -> Nothing) StringLiteral sp xp
          primaryIdentifier = do
            (iden, nx, ns) <- identifierParser sp xp
            return (PrimaryIdentifier iden, nx, ns)
          grouping = do
            (((_, expr), _), nx, ns)
              <- (rTokenParser LT.LeftParen ()
                 <-> expression
                 <-> rTokenParser LT.RightParen ()) sp xp
            return (expr, nx, ns)
          arrayLiteral = do
            (((_, exprs), _), nx, ns)
              <- (rTokenParser LT.LeftBrace ()
                 <-> arguments
                 <-> rTokenParser LT.RightBrace ()) sp xp
            return (ArrayLiteral exprs, nx, ns)
          undefinedParser = rTokenParser LT.Undefined Undefined sp xp
          literalExtract :: (NFData b) => (LT.TokenType -> Maybe b) -> (b -> c) -> Parser c
          literalExtract _ _ s [] = Left $ E.Error
                                    (parseErrorMessage)
                                    (filename s)
                                    (line s)
                                    (column s)
                                    (column s + 1)
          literalExtract extractVal construct (ParserState f l c) (x:xs)
              | isJust $ extractVal t = Right (construct $ fromJust $ extractVal t, xs, (ParserState nf nl nc))
              | otherwise = Left $ E.Error
                            (parseErrorMessage)
                            f l c (c + LT.length x)
              where len = LT.length x
                    t = LT.tokenType x
                    (nf, nl, nc)
                        | null xs = (f, l + len, c)
                        | otherwise = (f, LT.line $ head xs, LT.column $ head xs)

ltrSingleOpParser :: Parser Expression -> BinaryOp -> LT.TokenType -> Parser Expression
ltrSingleOpParser recur binop op s x = do
  ((l, i), nx, ns)
    <- ((sequenceParser $ recur <-> rTokenParser op binop)
       <-> recur) s x
  return (constructTree (map invert i) l, nx, ns)

ltrOpParser :: Parser Expression -> Parser BinaryOp -> Parser Expression
ltrOpParser recur binop s x = do
  ((l, i), nx, ns)
    <- ((sequenceParser $ recur <-> binop)
       <-> recur) s x
  return (constructTree (map invert i) l, nx, ns)

constructTree :: [(Expression, BinaryOp)] -> Expression -> Expression
constructTree [] lastT = lastT
constructTree [(initTE, initTO)] lastT = Binary initTE initTO lastT 
constructTree ((initTE1, initTO1):(initTE2, initTO2):initTS) lastT = constructTree (((Binary initTE1 initTO1 initTE2), initTO2):initTS) lastT
       
assignOp :: Parser BinaryOp
assignOp = anyTokenParser 
           [
            (LT.Equals, Equals),
            (LT.PlusEquals, PlusEquals),
            (LT.MinusEquals, MinusEquals),
            (LT.StarEquals, StarEquals),
            (LT.SlashEquals, SlashEquals),
            (LT.PercentEquals, PercentEquals),
            (LT.LShiftEquals, LShiftEquals),
            (LT.RShiftEquals, RShiftEquals),
            (LT.AndEquals, AndEquals),
            (LT.HatEquals, HatEquals),
            (LT.BarEquals, BarEquals)
           ]

equalityOp :: Parser BinaryOp
equalityOp = anyTokenParser 
                 [
                  (LT.EqualsEquals, EqualsEquals),
                  (LT.ExclaEquals, ExclaEquals)
                 ]

comparisonOp :: Parser BinaryOp
comparisonOp = anyTokenParser 
                 [
                  (LT.Greater, Greater),
                  (LT.Lesser, Lesser),
                  (LT.GreaterEquals, GreaterEquals),
                  (LT.LesserEquals, LesserEquals)
                 ]

shiftOp :: Parser BinaryOp
shiftOp = anyTokenParser 
                 [
                  (LT.LShift, LShift),
                  (LT.RShift, RShift)
                 ]

termOp :: Parser BinaryOp
termOp = anyTokenParser 
                 [
                  (LT.Plus, TermPlus),
                  (LT.Minus, TermMinus)
                 ]

factorOp :: Parser BinaryOp
factorOp = anyTokenParser 
                 [
                  (LT.Star, FactorStar),
                  (LT.Slash, FactorSlash),
                  (LT.Percent, FactorPercent)
                 ]

prefixOp :: Parser UnaryOp
prefixOp s x = castParser <!> anyTokenParser 
                 [
                  (LT.PlusPlus, PrePlusPlus),
                  (LT.MinusMinus, PreMinusMinus),
                  (LT.Plus, Plus),
                  (LT.Minus, Minus),
                  (LT.Excla, Excla),
                  (LT.Tilda, Tilda),
                  (LT.Star, Star),
                  (LT.And, And)
                 ] s x
    where castParser = do
            (((_, decType), _), nx, ns)
              <- (rTokenParser LT.LeftParen ()
                 <-> decoratedType
                 <-> rTokenParser LT.RightParen()) s x
            return (Cast decType, nx, ns)

postfixOp :: Parser UnaryOp
postfixOp s x = callParser <!> indexParser <!> dotParser <!> arrowParser
                <!> anyTokenParser 
                       [
                        (LT.PlusPlus, PostPlusPlus),
                        (LT.MinusMinus, PostMinusMinus)
                       ] s x
    where callParser = do
            (((_, args), _), nx, ns)
              <- (rTokenParser LT.LeftParen ()
                 <-> arguments
                 <-> rTokenParser LT.RightParen ()) s x
            return (Call args, nx, ns)
          indexParser = do
            (((_, args), _), nx, ns)
              <- (rTokenParser LT.LeftBracket ()
                 <-> arguments
                 <-> rTokenParser LT.RightBracket ()) s x
            return (Index args, nx, ns)
          dotParser = do
            ((iden, _), nx, ns)
              <- (rTokenParser LT.Dot ()
                 <-> identifierParser) s x
            return (Dot iden, nx, ns)
          arrowParser = do
            ((iden, _), nx, ns)
              <- (rTokenParser LT.Arrow ()
                 <-> identifierParser) s x
            return (Arrow iden, nx, ns)

anyTokenParser :: [(LT.TokenType, a)] -> Parser a
anyTokenParser [] s x = genError s x
anyTokenParser _ s [] = genError s []
anyTokenParser ((t, c):cs) s fx@(x:_)
    | LT.tokenType x == t = rTokenParser t c s fx <!> anyTokenParser cs s fx
    | otherwise = anyTokenParser cs s fx

parameters :: Parser [DecoratedIdentifier]
parameters s x = do
  ((nextParams, firstParam), nx, ns)
    <- (decoratedIdentifier
       <-> (sequenceParser $ rTokenParser LT.Comma () <-> decoratedIdentifier)) s x
  return (firstParam:(map fst nextParams), nx, ns)

arguments :: Parser [Expression]
arguments = tolerate [] argsNoTolerate
    where argsNoTolerate s x = do
            ((nextArgs, firstArg), nx, ns)
              <- (expression
                 <-> (sequenceParser $ rTokenParser LT.Comma () <-> expression)) s x
            return (firstArg:(map fst nextArgs), nx, ns)

decoratedType :: Parser DecoratedType
decoratedType s x = do
  (((post, t), stars), nx, ns)
    <- ((sequenceParser $ rTokenParser LT.Star ())
       <-> typeParser
       <-> (sequenceParser $ rTokenParser LT.LeftBracket () <-> expression <-> rTokenParser LT.RightBracket ())) s x
  return (DecoratedType (length stars) t (map snd $ map fst post), nx, ns)

decoratedIdentifier :: Parser DecoratedIdentifier
decoratedIdentifier s x = do
  ((((t, _), iden), m), nx, ns)
    <- (sequenceParser modifier
       <-> identifierParser
       <-> rTokenParser LT.Colon()
       <-> decoratedType) s x
  return (DecoratedIdentifier m iden t, nx, ns)

modifier :: Parser Modifier
modifier s x = rTokenParser LT.Pure Pure s x
               <!> rTokenParser LT.Const Const s x
               <!> rTokenParser LT.Inline Inline s x
               <!> rTokenParser LT.Comptime Comptime s x
               <!> rTokenParser LT.Register Register s x
               <!> rTokenParser LT.Restrict Restrict s x
               <!> (genError s x)

typeParser :: Parser Type
typeParser s x = rTokenParser LT.U8 U8 s x
               <!> rTokenParser LT.U16 U16 s x
               <!> rTokenParser LT.U32 U32 s x
               <!> rTokenParser LT.U64 U64 s x
               <!> rTokenParser LT.I8 I8 s x
               <!> rTokenParser LT.I16 I16 s x
               <!> rTokenParser LT.I32 I32 s x
               <!> rTokenParser LT.I64 I64 s x
               <!> rTokenParser LT.F16 F16 s x
               <!> rTokenParser LT.F32 F32 s x
               <!> rTokenParser LT.F64 F64 s x
               <!> (identifierParser s x >>= (\(xl, yl, zl) -> Right (StructType xl, yl, zl)))
               <!> (genError s x)

sequenceParser :: (NFData a) => Parser a -> Parser [a]
sequenceParser _ s [] = Right ([], [], s)
sequenceParser p s x
    | isLeft $ p s x = Right ([], x, s)
    | otherwise = Right (found:nextFound, nnx, nns)
    where (found, nx, ns) = fromRight undefined $ p s x
          (nextFound, nnx, nns) = fromRight undefined $ (sequenceParser p) ns nx

identifierParser :: Parser T.Text
identifierParser s [] = Left $ E.Error
                        (parseErrorMessage)
                        (filename s)
                        (line s)
                        (column s)
                        (column s + 1)
identifierParser (ParserState f l c) ((LT.Token (LT.Identifier t) _ _ len):xs) = Right (t, xs, (ParserState nf nl nc))
    where (nf, nl, nc)
              | null xs = (f, l + len, c)
              | otherwise = (f, LT.line $ head xs, LT.column $ head xs)
identifierParser (ParserState f l c) (x:_) = Left $ E.Error
                                             (parseErrorMessage)
                                             f l c (c + LT.length x)

rTokenParser :: LT.TokenType -> a -> Parser a
rTokenParser tt val s x = do
  (_, nx, ns) <- tokenParser tt s x
  return (val, nx, ns)
  
tokenParser :: LT.TokenType -> Parser LT.Token
tokenParser _ s [] = Left $ E.Error
                        (parseErrorMessage)
                        (filename s)
                        (line s)
                        (column s)
                        (column s + 1)
tokenParser tt (ParserState f l c) (x:xs)
    | tt == (LT.tokenType x) = Right (x, xs, (ParserState nf nl nc))
    | otherwise = Left $ E.Error
                  (parseErrorMessage)
                  f l c (c + LT.length x)
    where (nf, nl, nc)
              | null xs = (f, l + LT.length x, c)
              | otherwise = (f, LT.line $ head xs, LT.column $ head xs)

tolerate :: (NFData a) => a -> Parser a -> Parser a
tolerate e p s x
    | isRight $ p s x = p s x
    | otherwise = Right (e, x, s)

invert :: (a, b) -> (b, a)
invert (a, b) = (b, a)

genError :: Parser a
genError s x = Left $ E.Error
             (parseErrorMessage)
             (filename s)
             (line s)
             (column s)
             (column s + l)
    where l
             | null x = 1
             | otherwise = LT.length $ head x
