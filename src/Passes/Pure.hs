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

import Data.Maybe
import qualified Data.Text as T

import Semantics.SAST

data PureCall = PureCall T.Text [Expression] T.Text

type Purity = State [PureCall]

purePass :: SAST -> SAST
purePass = undefined

getPureFunctions :: [Declaration] -> [T.Text]
getPureFunctions = mapMaybe (\x -> case x of
                                     FuncDecl (Function (FunctionSignature _ n _ _) _) -> Just n
                                     otherwise -> Nothing)

aliasesPureCall :: Expression -> PureCall -> Bool
aliasesPureCall (Call fn1 es1 _) (PureCall fn2 es2 _) = fn1 == fn2 && es1 == es2
aliasesPureCall _ _ = False
