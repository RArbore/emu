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

module Semantics.Check
    (

     check

    ) where

import Control.Monad.State
import Control.Monad.Except

import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Text (Text)

import Parser.AST (Type (..),
                   Modifier (..),
                   FixedPointVal (..),
                   FloatingPointVal (..))
import qualified Parser.AST as A
    
import Semantics.Error
import Semantics.SAST

type Variables = M.Map (Text, VarKind) VarBinding
type Functions = M.Map Text Function
type Structures = M.Map Text Structure

data Environment = Environment { vars :: Variables,
                                 funcs :: Functions,
                                 structs :: Structures }

type Semantics = ExceptT SemanticsError (State Environment)

check :: A.AST -> Semantics SAST
check = undefined

uniform :: Eq a => [a] -> Bool
uniform [] = True
uniform (x:xs) = all (== x) xs

checkExpr :: A.Expression -> Semantics Expression
checkExpr ((l, sc, ec), e) = case e of
                               A.BooleanLiteral b -> return $ Literal $ BooleanLiteral b
                               A.FixedPointLiteral i -> case i of
                                                          A.U8Val _ -> return $ Literal $ FixedPointLiteral i
                                                          A.U16Val _ -> return $ Literal $ FixedPointLiteral i
                                                          A.U32Val _ -> return $ Literal $ FixedPointLiteral i
                                                          A.U64Val _ -> return $ Literal $ FixedPointLiteral i
                                                          A.I8Val _ -> return $ Literal $ FixedPointLiteral i
                                                          A.I16Val _ -> return $ Literal $ FixedPointLiteral i
                                                          A.I32Val _ -> return $ Literal $ FixedPointLiteral i
                                                          A.I64Val _ -> return $ Literal $ FixedPointLiteral i
                               A.FloatingPointLiteral d -> case d of
                                                             A.F32Val _ -> return $ Literal $ FloatingPointLiteral d
                                                             A.F64Val _ -> return $ Literal $ FloatingPointLiteral d
                               A.CharLiteral c -> return $ Literal $ FixedPointLiteral $ U8Val c
                               A.StringLiteral s -> return $ ArrayLiteral $ map (Literal . FixedPointLiteral . U8Val) $ B.unpack s
                               A.Undefined -> return $ Undefined
                               A.ArrayLiteral x -> do
                                      exprs <- sequence $ map checkExpr x
                                      if uniform $ map typeOf exprs then
                                          return $ ArrayLiteral exprs
                                      else throwError $ SemanticsError l sc ec HeterogenousArray
