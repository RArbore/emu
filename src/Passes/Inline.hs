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

module Passes.Inline
    (

     inlinePass

    ) where

import Data.Text (Text)

import Parser.AST (Modifier (Inline))
    
import Semantics.SAST

inlinePass :: SAST -> SAST
inlinePass (SAST decls) = let inlineFuncs = map (\(FuncDecl f) -> f) $ 
                                            filter (\d -> case d of
                                                            FuncDecl (Function (FunctionSignature mods _ _ _) _) -> Inline `elem` mods
                                                            otherwise -> False)
                                            decls
                          in undefined

class InlineDepends d where
    inlineDepends :: [Function] -> d -> [Function]

instance InlineDepends Declaration where
    inlineDepends fs (FuncDecl (Function _ s)) = inlineDepends fs s
    inlineDepends fs (VarDecl (VarBinding _ e)) = inlineDepends fs e
    inlineDepends fs (StatementDecl s) = inlineDepends fs s
    inlineDepends _ _ = []

fName :: Function -> Text
fName (Function (FunctionSignature _ n _ _) _) = n
