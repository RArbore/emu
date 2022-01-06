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

import Control.Applicative ((<$>), (<*>))

import qualified Data.Text as T
import Data.Word

import Foreign
import Foreign.C.String
import Foreign.Storable

import Parser.AST (Type (..), Modifier (..))
    
import Semantics.SAST

#include "codegen.h"

simpleTypePoke :: Word32 -> Ptr a -> IO ()
simpleTypePoke e ptr = do
  (#poke type, type_e) ptr e
  withCString "" $ \x -> (#poke type, struct_name) ptr x

simpleCVPoke :: Word32 -> Ptr a -> (Ptr a -> b -> IO ()) -> b -> IO ()
simpleCVPoke e ptr poke_val x = do
  (#poke comptime_value, type) ptr e
  poke_val ptr x

tEnum :: Enum a => Word32 -> a
tEnum = toEnum . fromIntegral

fEnum :: Enum a => a -> Word32
fEnum = fromIntegral . fromEnum
    
instance Storable Type where
    alignment _ = #alignment type
    sizeOf _ = #size type
    peek ptr = do
      enum <- (#peek type, type_e) ptr :: IO Word32
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
       (StructType . T.pack) <$> (peekCString =<< (#peek type, struct_name) ptr)] !! fromIntegral enum
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
                         (#poke type, type_e) ptr ((#const STRUCT) :: Word32)
                         cstruct_name <- newCString $ T.unpack struct_name
                         (#poke type, struct_name) ptr cstruct_name

instance Storable DecoratedType where
    alignment _ = #alignment decorated_type
    sizeOf _ = #size decorated_type
    peek ptr = do
      enum <- (#peek decorated_type, decorated_type_e) ptr :: IO Word32
      case enum of
        (#const PURE_TYPE) -> PureType <$> (peek =<< ((#peek decorated_type, pure_type) ptr))
        (#const DEREF_TYPE) -> DerefType <$> (peek =<< ((#peek decorated_type, deref_type) ptr))
        (#const ARRAY_TYPE) -> ArrayType <$> (peek =<< ((#peek decorated_type, array_type) ptr)) <*> (#peek decorated_type, array_size) ptr
    poke ptr (PureType t) = do
      (#poke decorated_type, decorated_type_e) ptr ((#const PURE_TYPE) :: Word32)
      typePtr <- calloc
      poke typePtr t
      (#poke decorated_type, pure_type) ptr typePtr
    poke ptr (DerefType dt) = do
      (#poke decorated_type, decorated_type_e) ptr ((#const DEREF_TYPE) :: Word32)
      decTypePtr <- calloc
      poke decTypePtr dt
      (#poke decorated_type, deref_type) ptr decTypePtr
    poke ptr (ArrayType dt w) = do
      (#poke decorated_type, decorated_type_e) ptr ((#const ARRAY_TYPE) :: Word32)
      decTypePtr <- calloc
      poke decTypePtr dt
      (#poke decorated_type, array_type) ptr decTypePtr
      (#poke decorated_type, array_size) ptr w

instance Storable DecoratedIdentifier where
    alignment _ = #alignment decorated_identifier
    sizeOf _ = #size decorated_identifier
    peek ptr = do
      numMods <- (#peek decorated_identifier, num_mods) ptr :: IO Word64
      ptrMods <- (#peek decorated_identifier, mods) ptr :: IO (Ptr Word32)
      w32Mods <- peekArray (fromIntegral numMods) ptrMods
      cname <- (#peek decorated_identifier, name) ptr
      name <- peekCString cname
      decTypePtr <- (#peek decorated_identifier, type) ptr :: IO (Ptr DecoratedType)
      decType <- peek decTypePtr
      return $ DecoratedIdentifier (map tEnum w32Mods) (T.pack name) decType
    poke ptr (DecoratedIdentifier mods name dt) = do
                                        modsArrPtr <- callocArray (length mods)
                                        pokeArray modsArrPtr (map fEnum mods)
                                        (#poke decorated_identifier, mods) ptr modsArrPtr
                                        (#poke decorated_identifier, num_mods) ptr (fromIntegral $ length mods :: Word64)
                                        cname <- newCString $ T.unpack name
                                        (#poke decorated_identifier, name) ptr cname
                                        decTypePtr <- calloc
                                        poke decTypePtr dt
                                        (#poke decorated_identifier, type) ptr decTypePtr
      
instance Storable ComptimeValue where
    alignment _ = #alignment comptime_value
    sizeOf _ = #size comptime_value
    peek ptr = do
      enum <- (#peek comptime_value, type) ptr :: IO Word32
      [ComptimePointer <$> (#peek comptime_value, comptime_ptr) ptr <*> (peek =<< (#peek comptime_value, ptr_type) ptr),
       ComptimeBool <$> (#peek comptime_value, comptime_bool) ptr,
       ComptimeU8 <$> (#peek comptime_value, comptime_u8) ptr,
       ComptimeU16 <$> (#peek comptime_value, comptime_u16) ptr,
       ComptimeU32 <$> (#peek comptime_value, comptime_u32) ptr,
       ComptimeU64 <$> (#peek comptime_value, comptime_u64) ptr,
       ComptimeI8 <$> (#peek comptime_value, comptime_i8) ptr,
       ComptimeI16 <$> (#peek comptime_value, comptime_i16) ptr,
       ComptimeI32 <$> (#peek comptime_value, comptime_i32) ptr,
       ComptimeI64 <$> (#peek comptime_value, comptime_i64) ptr,
       ComptimeF32 <$> (#peek comptime_value, comptime_f32) ptr,
       ComptimeF64 <$> (#peek comptime_value, comptime_f64) ptr,
       ComptimeStruct <$> (do size <- fromIntegral <$> ((#peek comptime_value, num_fields) ptr :: IO Word64)
                              fields <- (#peek comptime_value, fields) ptr
                              peekArray size fields) <*> (T.pack <$> (peekCString =<< (#peek comptime_value, struct_name) ptr)),
       ComptimeArr <$> (do size <- fromIntegral <$> ((#peek comptime_value, size) ptr :: IO Word64)
                           elems <- (#peek comptime_value, elements) ptr
                           peekArray size elems)] !! fromIntegral enum
    poke ptr (ComptimePointer p dt) = do
                                  (#poke comptime_value, type) ptr ((#const CT_PTR) :: Word32)
                                  (#poke comptime_value, comptime_ptr) ptr p
                                  decTypePtr <- calloc
                                  poke decTypePtr dt
                                  (#poke comptime_value, ptr_type) ptr decTypePtr
    poke ptr (ComptimeBool b) = simpleCVPoke (#const CT_BOOL) ptr (#poke comptime_value, comptime_bool) b
    poke ptr (ComptimeU8 u8) = simpleCVPoke (#const CT_U8) ptr (#poke comptime_value, comptime_u8) u8
    poke ptr (ComptimeU16 u16) = simpleCVPoke (#const CT_U16) ptr (#poke comptime_value, comptime_u16) u16
    poke ptr (ComptimeU32 u32) = simpleCVPoke (#const CT_U32) ptr (#poke comptime_value, comptime_u32) u32
    poke ptr (ComptimeU64 u64) = simpleCVPoke (#const CT_U64) ptr (#poke comptime_value, comptime_u64) u64
    poke ptr (ComptimeI8 i8) = simpleCVPoke (#const CT_I8) ptr (#poke comptime_value, comptime_i8) i8
    poke ptr (ComptimeI16 i16) = simpleCVPoke (#const CT_I16) ptr (#poke comptime_value, comptime_i16) i16
    poke ptr (ComptimeI32 i32) = simpleCVPoke (#const CT_I32) ptr (#poke comptime_value, comptime_i32) i32
    poke ptr (ComptimeI64 i64) = simpleCVPoke (#const CT_I64) ptr (#poke comptime_value, comptime_i64) i64
    poke ptr (ComptimeF32 f32) = simpleCVPoke (#const CT_F32) ptr (#poke comptime_value, comptime_f32) f32
    poke ptr (ComptimeF64 f64) = simpleCVPoke (#const CT_F64) ptr (#poke comptime_value, comptime_f64) f64
    poke ptr (ComptimeStruct cvs name) = do
                                  (#poke comptime_value, type) ptr ((#const CT_STRUCT) :: Word32)
                                  (#poke comptime_value, num_fields) ptr (fromIntegral $ length cvs :: Word64)
                                  cname <- newCString $ T.unpack name
                                  (#poke comptime_value, struct_name) ptr cname
                                  arrPtr <- callocArray (length cvs)
                                  pokeArray arrPtr cvs
                                  (#poke comptime_value, fields) ptr arrPtr
    poke ptr (ComptimeArr cvs) = do
                                  (#poke comptime_value, type) ptr ((#const CT_ARR) :: Word32)
                                  (#poke comptime_value, size) ptr (fromIntegral $ length cvs :: Word64)
                                  arrPtr <- callocArray (length cvs)
                                  pokeArray arrPtr cvs
                                  (#poke comptime_value, elements) ptr arrPtr

instance Storable LValue where
    alignment _ = #alignment lvalue
    sizeOf _ = #size lvalue
    peek ptr = do
      enum <- (#peek lvalue, type) ptr :: IO Word32
      [Dereference <$> (peek =<< (#peek lvalue, dereferenced) ptr),
       Access <$> (peek =<< (#peek lvalue, accessed) ptr) <*> (#peek lvalue, offset) ptr <*> (peek =<< (#peek lvalue, access_result_type) ptr),
       Index <$> (peek =<< (#peek lvalue, indexed) ptr) <*> (peek =<< (#peek lvalue, index) ptr) <*> (peek =<< (#peek lvalue, index_result_type) ptr),
       Identifier <$> (T.pack <$> (peekCString =<< (#peek lvalue, name) ptr)) <*> (peek =<< (#peek lvalue, iden_type) ptr)] !! fromIntegral enum
    poke ptr (Dereference e) = do
                           (#poke lvalue, type) ptr ((#const DEREF) :: Word32)
                           ePtr <- calloc
                           poke ePtr e
                           (#poke lvalue, dereferenced) ptr ePtr
    poke ptr (Access lv w dt) = do
                           (#poke lvalue, type) ptr ((#const ACCESS) :: Word32)
                           (#poke lvalue, offset) ptr w
                           lvPtr <- calloc
                           dtPtr <- calloc
                           poke lvPtr lv
                           poke dtPtr dt
                           (#poke lvalue, accessed) ptr lvPtr
                           (#poke lvalue, access_result_type) ptr dtPtr
    poke ptr (Index lv e dt) = do
                           (#poke lvalue, type) ptr ((#const INDEX) :: Word32)
                           lvPtr <- calloc
                           ePtr <- calloc
                           dtPtr <- calloc
                           poke lvPtr lv
                           poke ePtr e
                           poke dtPtr dt
                           (#poke lvalue, indexed) ptr lvPtr
                           (#poke lvalue, index) ptr ePtr
                           (#poke lvalue, index_result_type) ptr dtPtr
    poke ptr (Identifier n dt) = do
                           (#poke lvalue, type) ptr ((#const IDENTIFIER) :: Word32)
                           cn <- newCString $ T.unpack n
                           dtPtr <- calloc
                           poke dtPtr dt
                           (#poke lvalue, name) ptr cn
                           (#poke lvalue, iden_type) ptr dtPtr

instance Storable Expression where
    alignment _ = #alignment expression
    sizeOf _ = #size expression
    peek ptr = do
      enum <- (#peek expression, type) ptr :: IO Word32
      [let iobptr = (#peek expression, binary_expr) ptr :: IO (Ptr ())
       in Binary <$> (tEnum <$> ((#peek binary_expr, op) =<< iobptr)) <*> (peek =<< (#peek binary_expr, expr1) =<< iobptr) <*> (peek =<< (#peek binary_expr, expr2) =<< iobptr) <*> (peek =<< (#peek binary_expr, type) =<< iobptr),
       let ioeptr = (#peek expression, unary_expr) ptr :: IO (Ptr ())
       in Unary <$> (tEnum <$> ((#peek unary_expr, op) =<< ioeptr)) <*> (peek =<< (#peek unary_expr, expr) =<< ioeptr) <*> (peek =<< (#peek unary_expr, type) =<< ioeptr),
       let iolptr = (#peek expression, literal_expr) ptr :: IO (Ptr ())
       in Literal <$> (peek =<< (#peek literal_expr, comptime_value) =<< iolptr),
       let ioarptr = (#peek expression, array_expr) ptr :: IO (Ptr ())
       in Array <$>
              (do
                size <- (#peek array_expr, size) =<< ioarptr
                contents <- (#peek array_expr, elements) =<< ioarptr
                peekArray size contents),
       let iocptr = (#peek expression, call_expr) ptr :: IO (Ptr ())
       in Call <$> (T.pack <$> (peekCString =<< (#peek call_expr, func_name) =<< iocptr)) <*>
              (do
                num_args <- (#peek call_expr, num_args) =<< iocptr
                args <- (#peek call_expr, args) =<< iocptr
                peekArray num_args args) <*> (peek =<< (#peek call_expr, result_type) =<< iocptr),
       let iolvptr = (#peek expression, lvalue_expr) ptr :: IO (Ptr ())
       in LValueExpression <$> (peek =<< (#peek lvalue_expr, lvalue) =<< iolvptr),
       let ioasptr = (#peek expression, assign_expr) ptr :: IO (Ptr ())
       in Assign <$> (tEnum <$> ((#peek assign_expr, op) =<< ioasptr)) <*> (peek =<< (#peek assign_expr, lvalue) =<< ioasptr) <*> (peek =<< (#peek assign_expr, expr) =<< ioasptr),
       let ioadptr = (#peek expression, address_expr) ptr :: IO (Ptr ())
       in Address <$> (peek =<< (#peek address_expr, lvalue) =<< ioadptr),
       return Undefined] !! fromIntegral enum
      
instance Storable Statement where

instance Storable Declaration where
