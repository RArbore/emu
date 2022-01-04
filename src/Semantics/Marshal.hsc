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

import Foreign
import Foreign.C.String
import Foreign.Storable

import Parser.AST (Type (..))
    
import Semantics.SAST

#include "codegen.h"

simpleTypePoke :: Word8 -> Ptr a -> IO ()
simpleTypePoke e ptr = do
  (#poke type, type_e) ptr e
  withCString "" $ \x -> (#poke type, struct_name) ptr x
    
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
    poke ptr Void = simpleTypePoke (#const VOID) ptr
    poke ptr Bool = simpleTypePoke (#const BOOL) ptr
    poke ptr U8 = simpleTypePoke (#const U8) ptr
    poke ptr U16 = simpleTypePoke (#const U16) ptr
    poke ptr U32 = simpleTypePoke (#const U32) ptr
    poke ptr U64 = simpleTypePoke (#const U64) ptr
    poke ptr I8 = simpleTypePoke (#const I8) ptr
    poke ptr I16 = simpleTypePoke (#const I16) ptr
    poke ptr I32 = simpleTypePoke (#const I32) ptr
    poke ptr I64 = simpleTypePoke (#const I64) ptr
    poke ptr F32 = simpleTypePoke (#const F32) ptr
    poke ptr F64 = simpleTypePoke (#const F64) ptr
    poke ptr (StructType struct_name) = do
                         (#poke type, type_e) ptr ((#const STRUCT) :: Word8)
                         cstruct_name <- newCString $ T.unpack struct_name
                         (#poke type, struct_name) ptr cstruct_name

instance Storable DecoratedType where
    alignment _ = #alignment decorated_type
    sizeOf _ = #size decorated_type
    peek ptr = do
      enum <- (id :: Word8 -> Word8) <$> (#peek decorated_type, decorated_type_e) ptr
      case enum of
        (#const PURE_TYPE) -> PureType <$> (peek =<< ((#peek decorated_type, pure_type) ptr))
        (#const DEREF_TYPE) -> DerefType <$> (peek =<< ((#peek decorated_type, deref_type) ptr))
        (#const ARRAY_TYPE) -> ArrayType <$> (peek =<< ((#peek decorated_type, array_type) ptr)) <*> (#peek decorated_type, array_size) ptr
    poke ptr (PureType t) = do
      (#poke decorated_type, decorated_type_e) ptr ((#const PURE_TYPE) :: Word8)
      typePtr <- calloc
      poke typePtr t
      (#poke decorated_type, pure_type) ptr typePtr
    poke ptr (DerefType dt) = do
      (#poke decorated_type, decorated_type_e) ptr ((#const DEREF_TYPE) :: Word8)
      decTypePtr <- calloc
      poke decTypePtr dt
      (#poke decorated_type, deref_type) ptr decTypePtr
    poke ptr (ArrayType dt w) = do
      (#poke decorated_type, decorated_type_e) ptr ((#const ARRAY_TYPE) :: Word8)
      decTypePtr <- calloc
      poke decTypePtr dt
      (#poke decorated_type, array_type) ptr decTypePtr
      (#poke decorated_type, array_size) ptr w
