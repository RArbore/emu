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

module Semantics.Error
    (

    ) where

import Data.Text (Text)

import Parser.AST
     
data SemanticsError = SemanticsError { offendingLine :: Text,
                                       line :: Int,
                                       startCol :: Int,
                                       endCol :: Int,
                                       errorType :: SemanticsErrorType }
data SemanticsErrorType = DuplicateDeclaration Text 
                        | VoidVarDeclaration Text
                        | UndefinedSymbol Text
                        | TypeError Type Type
                        | CastError Type Type
                        | CallError Text Int Int
                        | AddressError
                        | AssignError
                        | ImproperIdentifier Text
                        | AccessError Text (Maybe Text)
                        | DeadCode
