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

module Semantics.Marshal
    (

    ) where

import Control.Applicative 

import qualified Data.Text as T
import Data.Word

import Foreign.C.String
import Foreign.Storable

import Parser.AST
    
import Semantics.SAST

#include "codegen.h"
    
instance Storable Type where
    alignment _ = #alignment type
    sizeOf _ = #size type
    peek ptr = do
      enum <- (id :: Word8 -> Word8) <$> (#peek type, type_e) ptr
      cstruct_name <- (#peek type, struct_name) ptr
      [return Void,
       return Bool,
       return U8,
       return U16,
       return U32,
       return U64,
       return I8,
       return I16,
       return I32,
       return I64,
       return F32,
       return F64,
       (StructType . T.pack) <$> peekCString cstruct_name] !! fromIntegral enum
    poke ptr Void = sequence_ [(#poke type, type_e) ptr ((#const VOID) :: Word8), withCString "" $ \x -> (#poke type, struct_name) ptr x]
    poke ptr Bool = sequence_ [(#poke type, type_e) ptr ((#const BOOL) :: Word8), withCString "" $ \x -> (#poke type, struct_name) ptr x]
    poke ptr U8 = sequence_ [(#poke type, type_e) ptr ((#const U8) :: Word8), withCString "" $ \x -> (#poke type, struct_name) ptr x]
    poke ptr U16 = sequence_ [(#poke type, type_e) ptr ((#const U16) :: Word8), withCString "" $ \x -> (#poke type, struct_name) ptr x]
    poke ptr U32 = sequence_ [(#poke type, type_e) ptr ((#const U32) :: Word8), withCString "" $ \x -> (#poke type, struct_name) ptr x]
    poke ptr U64 = sequence_ [(#poke type, type_e) ptr ((#const U64) :: Word8), withCString "" $ \x -> (#poke type, struct_name) ptr x]
    poke ptr I8 = sequence_ [(#poke type, type_e) ptr ((#const I8) :: Word8), withCString "" $ \x -> (#poke type, struct_name) ptr x]
    poke ptr I16 = sequence_ [(#poke type, type_e) ptr ((#const I16) :: Word8), withCString "" $ \x -> (#poke type, struct_name) ptr x]
    poke ptr I32 = sequence_ [(#poke type, type_e) ptr ((#const I32) :: Word8), withCString "" $ \x -> (#poke type, struct_name) ptr x]
    poke ptr I64 = sequence_ [(#poke type, type_e) ptr ((#const I64) :: Word8), withCString "" $ \x -> (#poke type, struct_name) ptr x]
    poke ptr F32 = sequence_ [(#poke type, type_e) ptr ((#const F32) :: Word8), withCString "" $ \x -> (#poke type, struct_name) ptr x]
    poke ptr F64 = sequence_ [(#poke type, type_e) ptr ((#const F64) :: Word8), withCString "" $ \x -> (#poke type, struct_name) ptr x]
    poke ptr (StructType struct_name) = do
                         (#poke type, type_e) ptr ((#const STRUCT) :: Word8)
                         cstruct_name <- newCString $ T.unpack struct_name
                         (#poke type, struct_name) ptr cstruct_name
