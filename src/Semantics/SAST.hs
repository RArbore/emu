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

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Semantics.SAST
    (

     SAST (..),
     Structure  (..),
     Function  (..),
     VarBinding  (..),
     Declaration  (..),
     Statement  (..),
     Expression  (..),
     LValue  (..),
     ComptimeValue  (..),
     DecoratedIdentifier (..),
     DecoratedType (..),
     AssignOp  (..),
     BinaryOp  (..),
     UnaryOp  (..),

     typeOf
     
    ) where

import Control.DeepSeq
    
import qualified Data.ByteString as B
import Data.Int
import Data.Text (Text)
import Data.Word

import GHC.Generics (Generic)

import Parser.AST (Type (..), Modifier (..), FixedPointVal (..), FloatingPointVal (..))

newtype SAST = SAST [Declaration] deriving (Show, Generic, NFData)

data Structure = Structure [Modifier] Text [DecoratedIdentifier] deriving (Show, Generic, NFData)

data Function = Function [Modifier] Text [DecoratedIdentifier] DecoratedType Statement deriving (Show, Generic, NFData)

data VarBinding = VarBinding DecoratedIdentifier Expression deriving (Show, Generic, NFData)

data Declaration = StructDecl Structure
                 | FuncDecl Function
                 | VarDecl VarBinding
                 | StatementDecl Statement deriving (Show, Generic, NFData)
    
data Statement = ExpressionStatement Expression
               | IfElseStatement Expression Statement Statement
               | DoWhileStatement Expression Statement
               | SwitchStatement Expression Statement
               | CaseStatement Expression Statement
               | ReturnStatement Expression
               | BreakStatement
               | ContinueStatement
               | Block [Declaration]
               | EmptyStatement deriving (Show, Generic, NFData)

data Expression = Binary BinaryOp Expression Expression DecoratedType
                | Unary UnaryOp Expression DecoratedType
                | Literal ComptimeValue
                | Array [Expression]
                | Call Text [Expression] DecoratedType
                | LValueExpression LValue
                | Assign AssignOp LValue Expression
                | Address LValue
                | Undefined deriving (Show, Generic, NFData, Eq)

data LValue = Dereference Expression
            | Access LValue Word64 DecoratedType
            | Identifier Text DecoratedType deriving (Show, Generic, NFData, Eq)

data ComptimeValue = ComptimePointer Word64 DecoratedType 
                   | ComptimeBool Bool
                   | ComptimeU8 Word8 
                   | ComptimeU16 Word16 
                   | ComptimeU32 Word32 
                   | ComptimeU64 Word64 
                   | ComptimeI8 Int8 
                   | ComptimeI16 Int16 
                   | ComptimeI32 Int32 
                   | ComptimeI64 Int64 
                   | ComptimeF32 Float 
                   | ComptimeF64 Double
                   | ComptimeStruct [ComptimeValue] Text
                   | ComptimePointerArr [Word64] DecoratedType [Word64]
                   | ComptimeBoolArr [Bool] [Word64]
                   | ComptimeU8Arr [Word8] [Word64]
                   | ComptimeU16Arr [Word16] [Word64]
                   | ComptimeU32Arr [Word32] [Word64]
                   | ComptimeU64Arr [Word64] [Word64]
                   | ComptimeI8Arr [Int8] [Word64]
                   | ComptimeI16Arr [Int16] [Word64]
                   | ComptimeI32Arr [Int32] [Word64]
                   | ComptimeI64Arr [Int64] [Word64]
                   | ComptimeF32Arr [Float] [Word64]
                   | ComptimeF64Arr [Double] [Word64]
                   | ComptimeStructArr [[ComptimeValue]] Text [Word64] deriving (Show, Generic, NFData, Eq)

data DecoratedIdentifier = DecoratedIdentifier [Modifier] Text DecoratedType deriving (Show, Generic, NFData)
data DecoratedType = PureType Type
                   | DerefType DecoratedType
                   | ArrayType DecoratedType Word64 deriving (Show, Generic, NFData, Eq)
 
data AssignOp = Equals
              | PlusEquals
              | MinusEquals
              | StarEquals
              | SlashEquals
              | PercentEquals
              | LShiftEquals
              | RShiftEquals
              | HatEquals
              | BarEquals
              | AndEquals deriving (Show, Generic, NFData, Eq)
            
data BinaryOp = LogicOr
              | LogicXor
              | LogicAnd
              | BitwiseOr
              | BitwiseXor
              | BitwiseAnd
              | EqualsEquals
              | ExclaEquals
              | Greater
              | Lesser
              | GreaterEquals
              | LesserEquals
              | LShift
              | RShift
              | TermPlus
              | TermMinus
              | FactorStar
              | FactorSlash
              | FactorPercent deriving (Show, Generic, NFData, Eq)
                
data UnaryOp = PrePlusPlus
             | PreMinusMinus
             | Plus
             | Minus
             | Excla
             | Tilda
             | Cast 
             | PostPlusPlus
             | PostMinusMinus
             | Index Expression deriving (Show, Generic, NFData, Eq)

typeOf :: Expression -> DecoratedType
typeOf (Binary _ _ _ t) = t
typeOf (Unary _ _ t) = t
typeOf (Literal (ComptimePointer _ t)) = DerefType t
typeOf (Literal (ComptimeBool _)) = PureType Bool
typeOf (Literal (ComptimeU8 _)) = PureType U8
typeOf (Literal (ComptimeU16 _)) = PureType U16
typeOf (Literal (ComptimeU32 _)) = PureType U32
typeOf (Literal (ComptimeU64 _)) = PureType U64
typeOf (Literal (ComptimeI8 _)) = PureType I8
typeOf (Literal (ComptimeI16 _)) = PureType I16
typeOf (Literal (ComptimeI32 _)) = PureType I32
typeOf (Literal (ComptimeI64 _)) = PureType I64
typeOf (Literal (ComptimeF32 _)) = PureType F32
typeOf (Literal (ComptimeF64 _)) = PureType F64
typeOf (Literal (ComptimeStruct _ t)) = PureType $ StructType t
typeOf (Literal (ComptimePointerArr _ t dims)) = arrayWrap dims $ DerefType t
typeOf (Literal (ComptimeBoolArr _ dims)) = arrayWrap dims $ PureType Bool
typeOf (Literal (ComptimeU8Arr _ dims)) = arrayWrap dims $ PureType U8
typeOf (Literal (ComptimeU16Arr _ dims)) = arrayWrap dims $ PureType U16
typeOf (Literal (ComptimeU32Arr _ dims)) = arrayWrap dims $ PureType U32
typeOf (Literal (ComptimeU64Arr _ dims)) = arrayWrap dims $ PureType U64
typeOf (Literal (ComptimeI8Arr _ dims)) = arrayWrap dims $ PureType I8
typeOf (Literal (ComptimeI16Arr _ dims)) = arrayWrap dims $ PureType I16
typeOf (Literal (ComptimeI32Arr _ dims)) = arrayWrap dims $ PureType I32
typeOf (Literal (ComptimeI64Arr _ dims)) = arrayWrap dims $ PureType I64
typeOf (Literal (ComptimeF32Arr _ dims)) = arrayWrap dims $ PureType F32
typeOf (Literal (ComptimeF64Arr _ dims)) = arrayWrap dims $ PureType F64
typeOf (Literal (ComptimeStructArr _ t dims)) = arrayWrap dims $ PureType $ StructType t
typeOf (Array x) = ArrayType (typeOf $ head x) (fromIntegral $ length x)
typeOf (Call _ _ t) = t
typeOf (LValueExpression (Dereference e)) = DerefType $ typeOf e
typeOf (LValueExpression (Access _ _ t)) = t
typeOf (LValueExpression (Identifier _ t)) = t
typeOf (Assign _ lval _) = typeOf $ LValueExpression lval
typeOf Undefined = PureType Void

arrayWrap :: [Word64] -> DecoratedType -> DecoratedType
arrayWrap [] t = t
arrayWrap (x:xs) t = ArrayType (arrayWrap xs t) x
