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
import Data.List
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

(<->) :: Parser a -> Parser b -> Parser (b, a)
(<->) pa pb s x = do
  (aa, nx, ns) <- pa s x
  (bb, nnx, nns) <- pb ns nx 
  return ((bb, aa), nnx, nns)
infixr <->

decl :: Parser Decl
decl s x = structDecl <> funcDecl <> varDecl <> statementDecl
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
                 <-> (tolerate (Parameters []) parameters)
                 <-> rTokenParser LT.RightParen ()
                 <-> rTokenParser LT.Colon ()
                 <-> decoratedType
                 <-> statement) s x
            return (FuncDecl modifiers identifier params decType stmt, nx, ns)
          varDecl = do
            ((((_, expr), _), decIden), nx, ns)
              <- (decoratedIdentifier
                 <-> rTokenParser LT.Equals ()
                 <-> expression
                 <-> rTokenParser LT.Semi ()) s x
            return (VarDecl decIden expr, nx, ns)
          statementDecl = do
            (stmt, nx, ns) <- statement s x
            return (StatementDecl stmt, nx, ns)

statement :: Parser Statement
statement s x = exprStmt <> ifElseStmt <> whileStmt <> forStmt <> switchStmt <> caseStmt <> retStmt <> breakStmt <> contStmt <> blockStmt
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
            (((((((((stmtBody), _), exprInc), _), exprCond), stmtInit), _), _), nx, ns)
              <- (rTokenParser LT.For ()
                 <-> rTokenParser LT.LeftParen ()
                 <-> statement
                 <-> expression
                 <-> rTokenParser LT.Semi()
                 <-> expression
                 <-> rTokenParser LT.RightParen ()
                 <-> statement) s x
            return (ForStatement stmtInit exprCond exprInc stmtBody, nx, ns)
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
            (((_, stmts), _), nx, ns)
              <- (rTokenParser LT.LeftBrace ()
                 <-> sequenceParser statement
                 <-> rTokenParser LT.RightBrace()) s x
            return (Block stmts, nx, ns)

expression :: Parser Expression
expression s x = (assignment s x) >>= (\(xl, yl, zl) -> Right (Expression xl, yl, zl))

assignment :: Parser Assignment
assignment s x = do
  ((t, h), nx, ns)
    <- (logicOr
       <-> (sequenceParser $ logicOr <-> assignOp)) s x
  return (Assignment h t, nx, ns)

logicOr :: Parser LogicOr
logicOr = ltrSingleOpParser logicXor LogicOr LT.BarBar

logicXor :: Parser LogicXor
logicXor = ltrSingleOpParser logicAnd LogicXor LT.HatHat
       
logicAnd :: Parser LogicAnd
logicAnd = ltrSingleOpParser bitwiseOr LogicAnd LT.AndAnd

bitwiseOr :: Parser BitwiseOr
bitwiseOr = ltrSingleOpParser bitwiseXor BitwiseOr LT.Bar

bitwiseXor :: Parser BitwiseXor
bitwiseXor = ltrSingleOpParser bitwiseAnd BitwiseXor LT.Hat

bitwiseAnd :: Parser BitwiseAnd
bitwiseAnd = ltrSingleOpParser equality BitwiseAnd LT.And

equality :: Parser Equality
equality = ltrOpParser comparison Equality equalityOp

comparison :: Parser Comparison
comparison = ltrOpParser shift Comparison comparisonOp

shift :: Parser Shift
shift = ltrOpParser term Shift shiftOp

term :: Parser Term
term = ltrOpParser factor Term termOp

factor :: Parser Factor
factor = ltrOpParser prefix Factor factorOp

prefix :: Parser Prefix
prefix s x = do
  ((post, ops), nx, ns)
    <- ((sequenceParser prefixOp)
       <-> postfix) s x
  return (Prefix ops post, nx, ns)

postfix :: Parser Postfix
postfix s x = do
  ((ops, prim), nx, ns)
    <- (primary
       <-> (sequenceParser postfixOp)) s x
  return (Postfix prim ops, nx, ns)

primary :: Parser Primary
primary = undefined

ltrSingleOpParser :: Parser a -> ([a] -> a -> b) -> LT.TokenType -> Parser b
ltrSingleOpParser recur construct op s x = do
  ((l, i), nx, ns)
    <- ((sequenceParser $ rTokenParser op () <-> recur)
       <-> recur) s x
  return (construct (map fst i) l, nx, ns)

ltrOpParser :: Parser a -> ([(a, b)] -> a -> c) -> Parser b -> Parser c
ltrOpParser recur construct op s x = do
  ((l, i), nx, ns)
    <- ((sequenceParser $ op <-> recur)
       <-> recur) s x
  return (construct i l, nx, ns)
       
assignOp :: Parser AssignOp
assignOp = anyTokenParser (T.pack "Couldn't find assign operation token")
           (LT.Equals, Equals)
           [
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

equalityOp :: Parser EqualityOp
equalityOp = anyTokenParser (T.pack "Couldn't find equality operation token")
                 (LT.EqualsEquals, EqualsEquals)
                 [(LT.ExclaEquals, ExclaEquals)]

comparisonOp :: Parser CompareOp
comparisonOp = anyTokenParser (T.pack "Couldn't find comparison operation token")
                 (LT.Greater, Greater)
                 [
                  (LT.Lesser, Lesser),
                  (LT.GreaterEquals, GreaterEquals),
                  (LT.LesserEquals, LesserEquals)
                 ]

shiftOp :: Parser ShiftOp
shiftOp = anyTokenParser (T.pack "Couldn't find shift operation token")
                 (LT.LShift, LShift)
                 [(LT.RShift, RShift)]

termOp :: Parser TermOp
termOp = anyTokenParser (T.pack "Couldn't find term operation token")
                 (LT.Plus, TermPlus)
                 [(LT.Minus, TermMinus)]

factorOp :: Parser FactorOp
factorOp = anyTokenParser (T.pack "Couldn't find factor operation token")
                 (LT.Star, FactorStar)
                 [
                  (LT.Slash, FactorSlash),
                  (LT.Percent, FactorPercent)
                 ]

prefixOp :: Parser PrefixOp
prefixOp s x = castParser <> anyTokenParser (T.pack "Couldn't find prefix operation token")
                 (LT.PlusPlus, PrePlusPlus)
                 [
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

postfixOp :: Parser PostfixOp
postfixOp s x = callParser <> indexParser <> dotParser <> arrowParser
                <> anyTokenParser (T.pack "Couldn't find postifx operation token")
                       (LT.PlusPlus, PostPlusPlus)
                       [(LT.MinusMinus, PostMinusMinus)] s x
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

anyTokenParser :: T.Text -> (LT.TokenType, a) -> [(LT.TokenType, a)] -> Parser a
anyTokenParser e h t s x = foldl' (<>) (applyToken h s x) ([p s x | p <- (map applyToken t)])
                               <> (Left $ E.Error
                                   (e)
                                   (filename s)
                                   (line s)
                                   (column s)
                                   (column s + l))
    where l
             | null x = 1
             | otherwise = LT.length $ head x
          applyToken (ta, cons) = rTokenParser ta cons

parameters :: Parser Parameters
parameters s x = do
  ((nextParams, firstParam), nx, ns)
    <- (decoratedIdentifier
       <-> (sequenceParser $ rTokenParser LT.Comma () <-> decoratedIdentifier)) s x
  return (Parameters $ firstParam:(map fst nextParams), nx, ns)

arguments :: Parser Arguments
arguments = tolerate (Arguments []) argsNoTolerate
    where argsNoTolerate s x = do
            ((nextArgs, firstArg), nx, ns)
              <- (expression
                 <-> (sequenceParser $ rTokenParser LT.Comma () <-> expression)) s x
            return (Arguments $ firstArg:(map fst nextArgs), nx, ns)

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

typeParser :: Parser Type
typeParser s x = rTokenParser LT.U8 U8 s x
               <> rTokenParser LT.U16 U16 s x
               <> rTokenParser LT.U32 U32 s x
               <> rTokenParser LT.U64 U64 s x
               <> rTokenParser LT.I8 I8 s x
               <> rTokenParser LT.I16 I16 s x
               <> rTokenParser LT.I32 I32 s x
               <> rTokenParser LT.I64 I64 s x
               <> rTokenParser LT.F16 F16 s x
               <> rTokenParser LT.F32 F32 s x
               <> rTokenParser LT.F64 F64 s x
               <> (identifierParser s x >>= (\(xl, yl, zl) -> Right (StructType xl, yl, zl)))
               <> (Left $ E.Error
                      (T.pack $ "Couldn't find type token")
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

tolerate :: a -> Parser a -> Parser a
tolerate e p s x
    | isRight $ p s x = p s x
    | otherwise = Right (e, x, s)
