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

module Passes.Pure
    (

     purePass

    ) where

import Control.Monad.State

import Data.List
import Data.Maybe
import qualified Data.Text as T

import Parser.AST (Modifier (..))
    
import Semantics.SAST

data PureCall = PureCall T.Text [Expression] DecoratedType T.Text deriving (Eq)

type Purity = State ([PureCall], [T.Text], [T.Text]) -- Pure calls already made, defined pure functions, defined const vars

purePass :: SAST -> SAST
purePass (SAST decls) = SAST $ pureHelperD [] decls
    where
      pureFuncs = getPureFunctions decls
      pureHelperD _ [] = []
      pureHelperD cgs (d:ds) = case d of
                                 FuncDecl func -> (FuncDecl $ pureHelperF cgs func):(pureHelperD cgs ds)
                                 VarDecl (VarBinding (DecoratedIdentifier mods n _) _) -> if Const `elem` mods then d:(pureHelperD (n:cgs) ds) else d:(pureHelperD cgs ds)
                                 otherwise -> d:(pureHelperD cgs ds)
      pureHelperF cgs (Function sig s) = Function sig $ Block (evalState (purifyInsideFunc $ ensureInBlock s) ([], pureFuncs, cgs))

getPureFunctions :: [Declaration] -> [T.Text]
getPureFunctions = mapMaybe (\x -> case x of
                                     FuncDecl (Function (FunctionSignature _ n _ _) _) -> Just n
                                     otherwise -> Nothing)

ensureInBlock :: Statement -> [Declaration]
ensureInBlock (Block ds) = ds
ensureInBlock s = [StatementDecl s]

purifyInsideFunc :: [Declaration] -> Purity ([Declaration], [Declaration])
purifyInsideFunc (d:ds) =
    case d of
      StatementDecl s ->
          do
            case s of
              ExpressionStatement e ->
                  do
                    (newE, header) <- purifyExpr e
                    after <- purifyInsideFunc ds
                    return (header, ensureInBlock (ExpressionStatement newE) ++ after)
              IfElseStatement e s1 s2 b1 b2 ->
                  do
                    (newE, header) <- purifyExpr e
                    prevState <- get
                    pos <- purifyInsideFunc $ ensureInBlock s1
                    posState <- get
                    put prevState
                    neg <- purifyInsideFunc $ ensureInBlock s2
                    negState <- get
                    put (tup1 posState `intersect` tup1 negState, tup2 prevState, tup2 prevState)
                    after <- purifyInsideFunc ds
                    return (header, ensureInBlock (IfElseStatement newE (Block pos) (Block neg) b1 b2) ++ after)
              DoWhileStatement e s b ->
                  do
                    (newE, header) <- purifyExpr e
                    (hoisted, body) <- (purifyInsideFunc $ ensureInBlock s) >>= purifyInsideFunc
                    after <- purifyInsideFunc ds
                    return (header ++ hoisted, ensureInBlock (DoWhileStatement newE (Block body) b) ++ after)

purifyExpr :: Expression -> Purity (Expression, [Declaration])
purifyExpr = undefined

tup1 :: (a, b, c) -> a
tup1 (a, _, _) = a

tup2 :: (a, b, c) -> b
tup2 (_, b, _) = b

tup3 :: (a, b, c) -> c
tup3 (_, _, c) = c
