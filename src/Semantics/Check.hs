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
                               A.BooleanLiteral b -> return (DecoratedType 0 Bool [], Literal $ BooleanLiteral b)
                               A.FixedPointLiteral i -> case i of
                                                          A.U8Val _ -> return (DecoratedType 0 U8 [], Literal $ FixedPointLiteral i)
                                                          A.U16Val _ -> return (DecoratedType 0 U16 [], Literal $ FixedPointLiteral i)
                                                          A.U32Val _ -> return (DecoratedType 0 U32 [], Literal $ FixedPointLiteral i)
                                                          A.U64Val _ -> return (DecoratedType 0 U64 [], Literal $ FixedPointLiteral i)
                                                          A.I8Val _ -> return (DecoratedType 0 I8 [], Literal $ FixedPointLiteral i)
                                                          A.I16Val _ -> return (DecoratedType 0 I16 [], Literal $ FixedPointLiteral i)
                                                          A.I32Val _ -> return (DecoratedType 0 I32 [], Literal $ FixedPointLiteral i)
                                                          A.I64Val _ -> return (DecoratedType 0 I64 [], Literal $ FixedPointLiteral i)
                               A.FloatingPointLiteral d -> case d of
                                                             A.F32Val _ -> return (DecoratedType 0 F32 [], Literal $ FloatingPointLiteral d)
                                                             A.F64Val _ -> return (DecoratedType 0 F64 [], Literal $ FloatingPointLiteral d)
                               A.CharLiteral c -> return (DecoratedType 0 U8 [], Literal $ FixedPointLiteral $ U8Val c)
                               A.StringLiteral s -> return
                                                    (DecoratedType 0 U8 [(FixedPointLiteral $ U64Val $ fromIntegral $ B.length s)],
                                                     ArrayLiteral $ map (\x -> (DecoratedType 0 U8 [], Literal $ FixedPointLiteral $ U8Val x)) $ B.unpack s)
                               A.Undefined -> return (DecoratedType 0 Void [], Undefined)
                               A.ArrayLiteral x -> do
                                      exprs <- sequence $ map checkExpr x
                                      if uniform exprs then
                                          let (DecoratedType p t b, _) = head exprs
                                          in return $
                                                 (DecoratedType p t ((FixedPointLiteral $ U64Val $ fromIntegral $ length exprs):b),
                                                  ArrayLiteral exprs)
                                      else undefined
