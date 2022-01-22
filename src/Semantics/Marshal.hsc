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

#include "lib.h"

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
      [ComptimePointer <$> (#peek comptime_value, comptime_ptr) ptr <*> ((\(DerefType t) -> t) <$> (peek =<< (#peek comptime_value, ptr_type) ptr)),
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
                           peekArray size elems) <*> ((\(ArrayType t _) -> t) <$> (peek =<< (#peek comptime_value, array_type) ptr))] !! fromIntegral enum
    poke ptr (ComptimePointer p dt) = do
                                  (#poke comptime_value, type) ptr ((#const CT_PTR) :: Word32)
                                  (#poke comptime_value, comptime_ptr) ptr p
                                  decTypePtr <- calloc
                                  poke decTypePtr $ DerefType dt
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
    poke ptr (ComptimeArr cvs dt) = do
                                  (#poke comptime_value, type) ptr ((#const CT_ARR) :: Word32)
                                  (#poke comptime_value, size) ptr (fromIntegral $ length cvs :: Word64)
                                  arrPtr <- callocArray (length cvs)
                                  pokeArray arrPtr cvs
                                  dtPtr <- calloc
                                  poke dtPtr (ArrayType dt (fromIntegral $ length cvs))
                                  (#poke comptime_value, elements) ptr arrPtr
                                  (#poke comptime_value, array_type) ptr dtPtr

instance Storable LValue where
    alignment _ = #alignment lvalue
    sizeOf _ = #size lvalue
    peek ptr = do
      enum <- (#peek lvalue, type) ptr :: IO Word32
      [Dereference <$> (peek =<< (#peek lvalue, dereferenced) ptr) <*> (peek =<< (#peek lvalue, decorated_type) ptr),
       Access <$> (peek =<< (#peek lvalue, accessed) ptr) <*> (#peek lvalue, offset) ptr <*> (peek =<< (#peek lvalue, decorated_type) ptr),
       Index <$> (peek =<< (#peek lvalue, indexed) ptr) <*> (peek =<< (#peek lvalue, index) ptr) <*> (peek =<< (#peek lvalue, decorated_type) ptr),
       Identifier <$> (T.pack <$> (peekCString =<< (#peek lvalue, name) ptr)) <*> (peek =<< (#peek lvalue, decorated_type) ptr)] !! fromIntegral enum
    poke ptr (Dereference e dt) = do
                           (#poke lvalue, type) ptr ((#const DEREF) :: Word32)
                           ePtr <- calloc
                           dtPtr <- calloc
                           poke ePtr e
                           poke dtPtr dt
                           (#poke lvalue, dereferenced) ptr ePtr
                           (#poke lvalue, decorated_type) ptr dtPtr
    poke ptr (Access lv w dt) = do
                           (#poke lvalue, type) ptr ((#const ACCESS) :: Word32)
                           (#poke lvalue, offset) ptr w
                           lvPtr <- calloc
                           dtPtr <- calloc
                           poke lvPtr lv
                           poke dtPtr dt
                           (#poke lvalue, accessed) ptr lvPtr
                           (#poke lvalue, decorated_type) ptr dtPtr
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
                           (#poke lvalue, decorated_type) ptr dtPtr
    poke ptr (Identifier n dt) = do
                           (#poke lvalue, type) ptr ((#const IDENTIFIER) :: Word32)
                           cn <- newCString $ T.unpack n
                           dtPtr <- calloc
                           poke dtPtr dt
                           (#poke lvalue, name) ptr cn
                           (#poke lvalue, decorated_type) ptr dtPtr

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
       let iocptr = (#peek expression, cast_expr) ptr :: IO (Ptr ())
       in Cast <$> (peek =<< (#peek cast_expr, expr) =<< iocptr) <*> (peek =<< (#peek cast_expr, out_type) =<< iocptr),
       let iolvptr = (#peek expression, lvalue_expr) ptr :: IO (Ptr ())
       in LValueExpression <$> (peek =<< (#peek lvalue_expr, lvalue) =<< iolvptr),
       let ioasptr = (#peek expression, assign_expr) ptr :: IO (Ptr ())
       in Assign <$> (tEnum <$> ((#peek assign_expr, op) =<< ioasptr)) <*> (peek =<< (#peek assign_expr, lvalue) =<< ioasptr) <*> (peek =<< (#peek assign_expr, expr) =<< ioasptr),
       let ioadptr = (#peek expression, address_expr) ptr :: IO (Ptr ())
       in Address <$> (peek =<< (#peek address_expr, lvalue) =<< ioadptr),
       let iocrptr = (#peek expression, crement_expr) ptr :: IO (Ptr ())
       in Crement <$> (tEnum <$> ((#peek crement_expr, op) =<< iocrptr)) <*> (peek =<< (#peek crement_expr, lvalue) =<< iocrptr) <*> (peek =<< (#peek crement_expr, type) =<< iocrptr),
       return Undefined] !! fromIntegral enum
    poke ptr (Binary op e1 e2 dt) = do
                               (#poke expression, type) ptr ((#const BINARY_EXPR) :: Word32)
                               bptr <- callocBytes (#size binary_expr)
                               e1ptr <- calloc
                               e2ptr <- calloc
                               dtptr <- calloc
                               ldtptr <- calloc
                               rdtptr <- calloc
                               (#poke binary_expr, op) bptr $ fEnum op
                               poke e1ptr e1
                               poke e2ptr e2
                               poke dtptr dt
                               poke ldtptr $ typeOf e1
                               poke rdtptr $ typeOf e2
                               (#poke binary_expr, expr1) bptr e1ptr
                               (#poke binary_expr, expr2) bptr e2ptr
                               (#poke binary_expr, type) bptr dtptr
                               (#poke binary_expr, left_type) bptr ldtptr
                               (#poke binary_expr, right_type) bptr rdtptr
                               (#poke expression, binary_expr) ptr bptr
    poke ptr (Unary op e dt) = do
                               (#poke expression, type) ptr ((#const UNARY_EXPR) :: Word32)
                               uptr <- callocBytes (#size unary_expr)
                               eptr <- calloc
                               dtptr <- calloc
                               (#poke unary_expr, op) uptr $ fEnum op
                               poke eptr e
                               poke dtptr dt
                               (#poke unary_expr, expr) uptr eptr
                               (#poke unary_expr, type) uptr dtptr
                               (#poke expression, unary_expr) ptr uptr
    poke ptr (Literal cv) = do
      (#poke expression, type) ptr ((#const LITERAL_EXPR) :: Word32)
      lptr <- callocBytes (#size literal_expr)
      cvptr <- calloc
      poke cvptr cv
      (#poke literal_expr, comptime_value) lptr cvptr
      (#poke expression, literal_expr) ptr lptr
    poke ptr (Array es) = do
      (#poke expression, type) ptr ((#const ARRAY_EXPR) :: Word32)
      arptr <- callocBytes (#size array_expr)
      esptr <- callocArray $ length es
      dtptr <- calloc
      pokeArray esptr es
      poke dtptr (typeOf $ head es)
      (#poke array_expr, elements) arptr esptr
      (#poke array_expr, element_type) arptr dtptr
      (#poke array_expr, size) arptr (fromIntegral $ length es :: Word64)
      (#poke expression, array_expr) ptr arptr
    poke ptr (Call n es dt) = (do
                                (#poke expression, type) ptr ((#const CALL_EXPR) :: Word32)
                                cptr <- callocBytes (#size call_expr)
                                cn <- newCString $ T.unpack n
                                esptr <- callocArray $ length es
                                pokeArray esptr es
                                dtptr <- calloc
                                poke dtptr dt
                                (#poke call_expr, func_name) cptr cn
                                (#poke call_expr, args) cptr esptr
                                (#poke call_expr, num_args) cptr (fromIntegral $ length es :: Word64)
                                (#poke call_expr, result_type) cptr dtptr
                                (#poke expression, call_expr) ptr cptr)
    poke ptr (Cast e dt) = do
      (#poke expression, type) ptr ((#const CAST_EXPR) :: Word32)
      ceptr <- callocBytes (#size cast_expr)
      eptr <- calloc
      idtptr <- calloc
      odtptr <- calloc
      poke eptr e
      poke idtptr (typeOf e)
      poke odtptr dt
      (#poke cast_expr, expr) ceptr eptr
      (#poke cast_expr, in_type) ceptr idtptr
      (#poke cast_expr, out_type) ceptr odtptr
      (#poke expression, cast_expr) ptr ceptr
    poke ptr (LValueExpression lv) = do
                               (#poke expression, type) ptr ((#const LVALUE_EXPR) :: Word32)
                               lveptr <- callocBytes (#size lvalue_expr)
                               lvptr <- calloc
                               poke lvptr lv
                               (#poke lvalue_expr, lvalue) lveptr lvptr
                               (#poke expression, lvalue_expr) ptr lveptr
    poke ptr (Assign op lv e) = do
                               (#poke expression, type) ptr ((#const ASSIGN_EXPR) :: Word32)
                               asptr <- callocBytes (#size assign_expr)
                               (#poke assign_expr, op) asptr $ fEnum op
                               lvptr <- calloc
                               eptr <- calloc
                               ldtptr <- calloc
                               rdtptr <- calloc
                               poke lvptr lv
                               poke eptr e
                               poke ldtptr $ typeOf $ LValueExpression lv
                               poke rdtptr $ typeOf e
                               (#poke assign_expr, lvalue) asptr lvptr
                               (#poke assign_expr, expr) asptr eptr
                               (#poke assign_expr, left_type) asptr ldtptr 
                               (#poke assign_expr, right_type) asptr rdtptr 
                               (#poke expression, assign_expr) ptr asptr
    poke ptr (Address lv) = do
      (#poke expression, type) ptr ((#const ADDRESS_EXPR) :: Word32)
      adptr <- callocBytes (#size address_expr)
      lvptr <- calloc
      poke lvptr lv
      (#poke address_expr, lvalue) adptr lvptr
      (#poke expression, address_expr) ptr adptr
    poke ptr (Crement op lv dt) = do
                               (#poke expression, type) ptr ((#const CREMENT_EXPR) :: Word32)
                               crptr <- callocBytes (#size crement_expr)
                               lvptr <- calloc
                               dtptr <- calloc
                               (#poke crement_expr, op) crptr $ fEnum op
                               poke lvptr lv
                               poke dtptr dt
                               (#poke crement_expr, lvalue) crptr lvptr
                               (#poke crement_expr, type) crptr dtptr
                               (#poke expression, crement_expr) ptr crptr
    poke ptr Undefined = (#poke expression, type) ptr ((#const UNDEFINED) :: Word32)
      
instance Storable Statement where
    alignment _ = #alignment statement
    sizeOf _ = #size statement
    peek ptr = do
      enum <- (#peek statement, type) ptr :: IO Word32
      [let ioeptr = (#peek statement, expr_stmt) ptr :: IO (Ptr ())
       in ExpressionStatement <$> (peek =<< (#peek expr_stmt, expr) =<< ioeptr),
       let ioieptr = (#peek statement, ifelse_stmt) ptr :: IO (Ptr ())
       in IfElseStatement <$> (peek =<< (#peek ifelse_stmt, cond) =<< ioieptr) <*> (peek =<< (#peek ifelse_stmt, pos) =<< ioieptr) <*> (peek =<< (#peek ifelse_stmt, neg) =<< ioieptr),
       let iodwptr = (#peek statement, dowhile_stmt) ptr :: IO (Ptr ())
       in DoWhileStatement <$> (peek =<< (#peek dowhile_stmt, cond) =<< iodwptr) <*> (peek =<< (#peek dowhile_stmt, body) =<< iodwptr),
       let iorptr = (#peek statement, return_stmt) ptr :: IO (Ptr ())
       in ReturnStatement <$> (peek =<< (#peek return_stmt, expr) =<< iorptr),
       Block <$> (do
                   blockptr <- (#peek statement, block) ptr
                   size <- (#peek statement, block_size) ptr
                   peekArray size blockptr),
       return EmptyStatement] !! fromIntegral enum
    poke ptr (ExpressionStatement e) = do
                              (#poke statement, type) ptr ((#const EXPR_STMT) :: Word32)
                              esptr <- callocBytes (#size expr_stmt)
                              eptr <- calloc
                              poke eptr e
                              (#poke expr_stmt, expr) esptr eptr
                              (#poke statement, expr_stmt) ptr esptr
    poke ptr (IfElseStatement e s1 s2) = do
                              (#poke statement, type) ptr ((#const IFELSE_STMT) :: Word32)
                              ieptr <- callocBytes (#size ifelse_stmt)
                              eptr <- calloc
                              s1ptr <- calloc
                              s2ptr <- calloc
                              poke eptr e
                              poke s1ptr s1
                              poke s2ptr s2
                              (#poke ifelse_stmt, cond) ieptr eptr
                              (#poke ifelse_stmt, pos) ieptr s1ptr
                              (#poke ifelse_stmt, neg) ieptr s2ptr
                              (#poke statement, ifelse_stmt) ptr ieptr
    poke ptr (DoWhileStatement e s) = do
                              (#poke statement, type) ptr ((#const DOWHILE_STMT) :: Word32)
                              dwptr <- callocBytes (#size dowhile_stmt)
                              eptr <- calloc
                              sptr <- calloc
                              poke eptr e
                              poke sptr s
                              (#poke dowhile_stmt, cond) dwptr eptr
                              (#poke dowhile_stmt, body) dwptr sptr
                              (#poke statement, dowhile_stmt) ptr dwptr
    poke ptr (ReturnStatement e) = do
                              (#poke statement, type) ptr ((#const RETURN_STMT) :: Word32)
                              rptr <- callocBytes (#size return_stmt)
                              eptr <- calloc
                              poke eptr e
                              (#poke return_stmt, expr) rptr eptr
                              (#poke statement, return_stmt) ptr rptr
    poke ptr (Block ds) = do
      (#poke statement, type) ptr ((#const BLOCK) :: Word32)
      bptr <- callocArray $ length ds
      pokeArray bptr ds
      (#poke statement, block) ptr bptr
      (#poke statement, block_size) ptr (fromIntegral $ length ds :: Word64)
    poke ptr EmptyStatement = (#poke statement, type) ptr ((#const EMPTY) :: Word32)

instance Storable Declaration where
    alignment _ = #alignment declaration
    sizeOf _ = #size declaration
    peek ptr = do
      enum <- (#peek declaration, type) ptr :: IO Word32
      [let sptr = (#peek declaration, struct_decl) ptr :: IO (Ptr ())
       in StructDecl <$> (Structure <$> ((map tEnum) <$> (do
                                                           modsptr <- (#peek struct_decl, mods) =<< sptr
                                                           num_mods <- (#peek struct_decl, num_mods) =<< sptr :: IO Word64
                                                           peekArray (fromIntegral num_mods) modsptr))
                         <*> (T.pack <$> (peekCString =<< (#peek struct_decl, name) =<< sptr))
                         <*> (do
                               fsptr <- (#peek struct_decl, fields) =<< sptr
                               nfs <- (#peek struct_decl, num_fields) =<< sptr :: IO Word64
                               peekArray (fromIntegral nfs) fsptr)),
       let fptr = (#peek declaration, func_decl) ptr :: IO (Ptr ())
       in FuncDecl <$> (Function <$> (FunctionSignature <$> ((map tEnum) <$> (do
                                                        modsptr <- (#peek func_decl, mods) =<< fptr
                                                        num_mods <- (#peek func_decl, num_mods) =<< fptr :: IO Word64
                                                        peekArray (fromIntegral num_mods) modsptr))
                       <*> (T.pack <$> (peekCString =<< (#peek func_decl, name) =<< fptr))
                       <*> (do
                             psptr <- (#peek func_decl, params) =<< fptr
                             nps <- (#peek func_decl, num_params) =<< fptr :: IO Word64
                             peekArray (fromIntegral nps) psptr)
                       <*> (peek =<< (#peek func_decl, ret_type) =<< fptr))
                       <*> (peek =<< (#peek func_decl, body) =<< fptr)),
       let vptr = (#peek declaration, var_decl) ptr :: IO (Ptr ())
       in VarDecl <$> (VarBinding <$> (peek =<< (#peek var_decl, iden) =<< vptr) <*> (peek =<< (#peek var_decl, init) =<< vptr)),
       StatementDecl <$> (peek =<< (#peek stmt_decl, stmt) =<< (#peek declaration, stmt_decl) ptr)] !! fromIntegral enum
    poke ptr (StructDecl (Structure ms n dis)) = do
                                (#poke declaration, type) ptr ((#const STRUCT_DECL) :: Word32)
                                sdptr <- callocBytes (#size struct_decl)
                                msptr <- callocArray $ length ms
                                pokeArray msptr (map fEnum ms)
                                cn <- newCString $ T.unpack n
                                disptr <- callocArray $ length dis
                                pokeArray disptr dis
                                (#poke struct_decl, mods) sdptr msptr
                                (#poke struct_decl, num_mods) sdptr (fromIntegral $ length ms :: Word64)
                                (#poke struct_decl, name) sdptr cn
                                (#poke struct_decl, fields) sdptr disptr
                                (#poke struct_decl, num_fields) sdptr (fromIntegral $ length dis :: Word64)
                                (#poke declaration, struct_decl) ptr sdptr
    poke ptr (FuncDecl (Function (FunctionSignature ms n dis dt) s)) = do
                                (#poke declaration, type) ptr ((#const FUNC_DECL) :: Word32)
                                fdptr <- callocBytes (#size func_decl)
                                msptr <- callocArray $ length ms
                                pokeArray msptr (map fEnum ms)
                                cn <- newCString $ T.unpack n
                                disptr <- callocArray $ length dis
                                pokeArray disptr dis
                                dtptr <- calloc
                                sptr <- calloc
                                poke dtptr dt
                                poke sptr s
                                (#poke func_decl, mods) fdptr msptr
                                (#poke func_decl, num_mods) fdptr (fromIntegral $ length ms :: Word64)
                                (#poke func_decl, name) fdptr cn
                                (#poke func_decl, params) fdptr disptr
                                (#poke func_decl, num_params) fdptr (fromIntegral $ length dis :: Word64)
                                (#poke func_decl, ret_type) fdptr dtptr
                                (#poke func_decl, body) fdptr sptr
                                (#poke declaration, func_decl) ptr fdptr
    poke ptr (VarDecl (VarBinding di e)) = do
                                (#poke declaration, type) ptr ((#const VAR_DECL) :: Word32)
                                vdptr <- callocBytes (#size var_decl)
                                diptr <- calloc
                                eptr <- calloc
                                poke eptr e
                                poke diptr di
                                (#poke var_decl, iden) vdptr diptr
                                (#poke var_decl, init) vdptr eptr
                                (#poke declaration, var_decl) ptr vdptr
    poke ptr (StatementDecl s) = do
                                (#poke declaration, type) ptr ((#const STMT_DECL) :: Word32)
                                sdptr <- callocBytes (#size stmt_decl)
                                sptr <- calloc
                                poke sptr s
                                (#poke stmt_decl, stmt) sdptr sptr
                                (#poke declaration, stmt_decl) ptr sdptr

instance Storable SAST where
    alignment _ = #alignment sast
    sizeOf _ = #size sast
    peek ptr = do
      dsptr <- (#peek sast, decls) ptr
      num_decls <- (#peek sast, num_decls) ptr :: IO Word64
      SAST <$> peekArray (fromIntegral num_decls) dsptr
    poke ptr (SAST ds) = do
                         dsptr <- callocArray $ length ds
                         pokeArray dsptr ds
                         (#poke sast, decls) ptr dsptr
                         (#poke sast, num_decls) ptr (fromIntegral $ length ds :: Word64)
