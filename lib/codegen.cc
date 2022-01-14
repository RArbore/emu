/*  This file is part of emu.
    emu is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.
    emu is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    You should have received a copy of the GNU General Public License
    along with emu. If not, see <https://www.gnu.org/licenses/>.  */

#include "codegen.h"

static LLVMContext context;
static IRBuilder<> builder(context);
static Module *module;
static std::map<std::string, std::vector<decorated_type*>> defined_structs;

bool is_floating(decorated_type *dt) {
    switch (dt->decorated_type_e) {
    case PURE_TYPE:
	switch (dt->pure_type->type_e) {
	case F32: return true;
	case F64: return true;
	default: return false;
	}
    default: return false;
    }
}

bool is_signed(decorated_type *dt) {
    switch (dt->decorated_type_e) {
    case PURE_TYPE:
	switch (dt->pure_type->type_e) {
	case I8: return true;
	case I16: return true;
	case I32: return true;
	case I64: return true;
	default: return false;
	}
    default: return false;
    }
}

bool is_pointer(decorated_type *dt) {
    return dt->decorated_type_e == DEREF_TYPE;
}

StructType *struct_name_to_llvm_type(char *cname) {
    std::string name(cname);
    auto def_emu = defined_structs.at(name);
    std::vector<Type*> def_llvm;
    std::transform(def_emu.begin(), def_emu.end(), def_llvm.begin(), emu_to_llvm_type);
    return StructType::get(context, def_llvm);
}

Type *emu_to_llvm_type(decorated_type *dec_type) {
    switch(dec_type->decorated_type_e) {
    case PURE_TYPE: {
	type *pure_type = dec_type->pure_type;
	switch (pure_type->type_e) {
	case VOID: return Type::getVoidTy(context);
	case BOOL: return Type::getInt1Ty(context);
	case U8: return Type::getInt8Ty(context);
	case U16: return Type::getInt16Ty(context);
	case U32: return Type::getInt32Ty(context);
	case U64: return Type::getInt64Ty(context);
	case I8: return Type::getInt8Ty(context);
	case I16: return Type::getInt16Ty(context);
	case I32: return Type::getInt32Ty(context);
	case I64: return Type::getInt64Ty(context);
	case F32: return Type::getFloatTy(context);
	case F64: return Type::getDoubleTy(context);
	case STRUCT: return struct_name_to_llvm_type(pure_type->struct_name);
	default: return nullptr;
	}
    }
    case DEREF_TYPE: {
	Type *embed = emu_to_llvm_type(dec_type->deref_type);
	return PointerType::getUnqual(embed);
    }
    case ARRAY_TYPE: {
	Type *embed = emu_to_llvm_type(dec_type->array_type);
	return ArrayType::get(embed, dec_type->array_size);
    }
    default: return nullptr;
    }
}

Value *binary_expr_codegen(binary_expr *expr) {
    Value *v1 = expr_codegen(expr->expr1);
    Value *v2 = expr_codegen(expr->expr2);
    if (!v1 || !v2) return nullptr;
    switch (expr->op) {
    case LOGIC_OR: return builder.CreateLogicalOr(v1, v2);
    case LOGIC_XOR: return builder.CreateLogicalAnd(builder.CreateLogicalOr(v1, v2), builder.CreateICmpEQ(builder.CreateLogicalAnd(v1, v2), ConstantInt::get(context, APInt())));
    case LOGIC_AND: return builder.CreateLogicalAnd(v1, v2);
    case BITWISE_OR: return builder.CreateOr(v1, v2);
    case BITWISE_XOR: return builder.CreateXor(v1, v2);
    case BITWISE_AND: return builder.CreateAnd(v1, v2);
    case EQUALS_EQUALS: return is_floating(expr->type) ? builder.CreateFCmpOEQ(v1, v2) : builder.CreateICmpEQ(v1, v2);
    case EXCLA_EQUALS: return is_floating(expr->type) ? builder.CreateFCmpONE(v1, v2) : builder.CreateICmpNE(v1, v2);
    case GREATER: return
	    is_floating(expr->type)
	    ? builder.CreateFCmpOGT(v1, v2)
	    : is_signed(expr->type)
	    ? builder.CreateICmpSGT(v1, v2)
	    : builder.CreateICmpUGT(v1, v2);
    case LESSER: return
	    is_floating(expr->type)
	    ? builder.CreateFCmpOLT(v1, v2)
	    : is_signed(expr->type)
	    ? builder.CreateICmpSLT(v1, v2)
	    : builder.CreateICmpULT(v1, v2);
    case GREATER_EQUALS: return
	    is_floating(expr->type)
	    ? builder.CreateFCmpOGE(v1, v2)
	    : is_signed(expr->type)
	    ? builder.CreateICmpSGE(v1, v2)
	    : builder.CreateICmpUGE(v1, v2);
    case LESSER_EQUALS: return
	    is_floating(expr->type)
	    ? builder.CreateFCmpOLE(v1, v2)
	    : is_signed(expr->type)
	    ? builder.CreateICmpSLE(v1, v2)
	    : builder.CreateICmpULE(v1, v2);
    case LSHIFT: return builder.CreateShl(v1, v2);
    case RSHIFT: return is_signed(expr->type) ? builder.CreateAShr(v1, v2) : builder.CreateLShr(v1, v2);
    case TERM_PLUS: return is_floating(expr->type) ? builder.CreateFAdd(v1, v2) : builder.CreateAdd(v1, v2);
    case TERM_MINUS: return is_floating(expr->type) ? builder.CreateFSub(v1, v2) : builder.CreateSub(v1, v2);
    case FACTOR_STAR: return is_floating(expr->type) ? builder.CreateFMul(v1, v2) : builder.CreateMul(v1, v2);
    case FACTOR_SLASH: return
	    is_floating(expr->type)
	    ? builder.CreateFDiv(v1, v2)
	    : is_signed(expr->type)
	    ? builder.CreateSDiv(v1, v2)
	    : builder.CreateUDiv(v1, v2);
    case FACTOR_PERCENT: is_signed(expr->type) ? builder.CreateSRem(v1, v2) : builder.CreateURem(v1, v2);
    default: return nullptr;
    }
}

Value *unary_expr_codegen(unary_expr *expr) {
    Value *v = expr_codegen(expr->expr);
    if (!v) return nullptr;
    switch (expr->op) {
    case PRE_PLUS_PLUS: return nullptr;
    case PRE_MINUS_MINUS: return nullptr;
    case POST_PLUS_PLUS: return nullptr;
    case POST_MINUS_MINUS: return nullptr;
    case PLUS: return v;
    case MINUS: return is_floating(expr->type) ? builder.CreateFNeg(v) : builder.CreateNeg(v);
    case EXCLA: return builder.CreateICmpEQ(v, ConstantInt::get(context, APInt()));
    case TILDA: return builder.CreateNot(v);
    default: return nullptr;
    }
}

Value *literal_expr_codegen(literal_expr *expr) {
    comptime_value *cv = expr->comptime_value;
    std::function<Constant*(comptime_value*)> lambda = [&](comptime_value *cv) -> Constant* {
	switch (cv->type) {
	case CT_PTR: return ConstantExpr::getIntToPtr(ConstantInt::get(context, APInt(64, cv->comptime_ptr, false)), emu_to_llvm_type(cv->ptr_type));
	case CT_BOOL: return cv->comptime_bool ? ConstantInt::getTrue(context) : ConstantInt::getFalse(context);
	case CT_U8: return ConstantInt::get(context, APInt(8, cv->comptime_u8, false));
	case CT_U16: return ConstantInt::get(context, APInt(16, cv->comptime_u16, false));
	case CT_U32: return ConstantInt::get(context, APInt(32, cv->comptime_u32, false));
	case CT_U64: return ConstantInt::get(context, APInt(64, cv->comptime_u64, false));
	case CT_I8: return ConstantInt::get(context, APInt(8, cv->comptime_i8, true));
	case CT_I16: return ConstantInt::get(context, APInt(16, cv->comptime_i16, true));
	case CT_I32: return ConstantInt::get(context, APInt(32, cv->comptime_i32, true));
	case CT_I64: return ConstantInt::get(context, APInt(64, cv->comptime_i64, true));
	case CT_F32: return ConstantFP::get(context, APFloat(cv->comptime_f32));
	case CT_F64: return ConstantFP::get(context, APFloat(cv->comptime_f64));
	case CT_STRUCT: {
	    std::vector<Constant*> struct_values;
	    for (u64 i = 0; i < cv->num_fields; i++) {
		struct_values.push_back(lambda(cv->fields + i));
	    }
	    return ConstantStruct::get(struct_name_to_llvm_type(cv->struct_name), struct_values);
	}
	case CT_ARR: {
	    std::vector<Constant*> array_values;
	    for (u64 i = 0; i < cv->size; i++) {
		array_values.push_back(lambda(cv->elements + i));
	    }
	    return ConstantArray::get(ArrayType::get(emu_to_llvm_type(cv->array_type), cv->size), array_values);
	}
	default: return nullptr;
	};
    };
    return lambda(cv);
}

Value *array_expr_codegen(array_expr *expr) {
    ArrayType *array_type = ArrayType::get(emu_to_llvm_type(expr->element_type), expr->size);
    AllocaInst *alloca = builder.CreateAlloca(array_type, ConstantInt::get(context, APInt(64, expr->size, false)));
    for(size_t i = 0; i < expr->size; i++) {
	Value *element = expr_codegen(expr->elements + i);
	builder.CreateInsertElement(alloca, element, i);
    }
    return alloca;
}

Value *call_expr_codegen(call_expr *expr) {
    Function *to_call = module->getFunction(expr->func_name);
    std::vector<Value*> args;
    for (size_t i = 0; i < expr->num_args; i++) {
	Value *arg = expr_codegen(expr->args + i);
	if (!arg) return nullptr;
	args.push_back(arg);
    }
    return builder.CreateCall(to_call, args);
}

#define TRUNC Instruction::Trunc // Truncate integers
#define ZEXT Instruction::ZExt // Zero extend integers
#define SEXT Instruction::SExt // Sign extend integers
#define FPTOUI Instruction::FPToUI // floating point -> UInt
#define FPTOSI Instruction::FPToSI // floating point -> SInt
#define UITOFP Instruction::UIToFP // UInt -> floating point
#define SITOFP Instruction::SIToFP // SInt -> floating point
#define FPTRUNC Instruction::FPTrunc // Truncate floating point
#define FPEXT Instruction::FPExt // Extend floating point
#define PTRTOINT Instruction::PtrToInt // Pointer -> Integer
#define INTTOPTR Instruction::IntToPtr // Integer -> Pointer
#define BITCAST Instruction::BitCast // Type cast
const static Instruction::CastOps simple_cast_rules[STRUCT][STRUCT] = {
    /*                                                           TO                                       */
    
    /*                VOID      BOOL      U8        U16       U32       U64       I8        I16       I32       I64       F32       F64  */
    /*     VOID*/    {BITCAST , TRUNC   , TRUNC   , TRUNC   , TRUNC   , BITCAST , TRUNC   , TRUNC   , TRUNC   , BITCAST , TRUNC   , BITCAST },
    /*     BOOL*/    {ZEXT    , BITCAST , ZEXT    , ZEXT    , ZEXT    , ZEXT    , ZEXT    , ZEXT    , ZEXT    , ZEXT    , UITOFP  , UITOFP  },
    /*     U8*/      {ZEXT    , TRUNC   , BITCAST , ZEXT    , ZEXT    , ZEXT    , BITCAST , ZEXT    , ZEXT    , ZEXT    , UITOFP  , UITOFP  },
    /*     U16*/     {ZEXT    , TRUNC   , TRUNC   , BITCAST , ZEXT    , ZEXT    , TRUNC   , BITCAST , ZEXT    , ZEXT    , UITOFP  , UITOFP  },
    /*     U32*/     {ZEXT    , TRUNC   , TRUNC   , TRUNC   , BITCAST , ZEXT    , TRUNC   , TRUNC   , BITCAST , ZEXT    , UITOFP  , UITOFP  },
    /*FROM U64*/     {BITCAST , TRUNC   , TRUNC   , TRUNC   , TRUNC   , BITCAST , TRUNC   , TRUNC   , TRUNC   , BITCAST , UITOFP  , UITOFP  },
    /*     I8*/      {ZEXT    , TRUNC   , BITCAST , SEXT    , SEXT    , SEXT    , BITCAST , SEXT    , SEXT    , SEXT    , UITOFP  , SITOFP  },
    /*     I16*/     {ZEXT    , TRUNC   , TRUNC   , BITCAST , SEXT    , SEXT    , TRUNC   , BITCAST , SEXT    , SEXT    , UITOFP  , SITOFP  },
    /*     I32*/     {ZEXT    , TRUNC   , TRUNC   , TRUNC   , BITCAST , SEXT    , TRUNC   , TRUNC   , BITCAST , SEXT    , UITOFP  , SITOFP  },
    /*     I64*/     {BITCAST , TRUNC   , TRUNC   , TRUNC   , TRUNC   , BITCAST , TRUNC   , TRUNC   , TRUNC   , BITCAST , UITOFP  , SITOFP  },
    /*     F32*/     {ZEXT    , TRUNC   , FPTOUI  , FPTOUI  , FPTOUI  , FPTOUI  , FPTOSI  , FPTOSI  , FPTOSI  , FPTOSI  , BITCAST , FPEXT   },
    /*     F64*/     {BITCAST , TRUNC   , FPTOUI  , FPTOUI  , FPTOUI  , FPTOUI  , FPTOSI  , FPTOSI  , FPTOSI  , FPTOSI  , FPTRUNC , BITCAST },
};

Value *cast_expr_codegen(cast_expr *expr) {
    return nullptr;
}

Value *lvalue_expr_codegen(lvalue_expr *expr) {
    return nullptr;
}

Value *assign_expr_codegen(assign_expr *expr) {
    return nullptr;
}

Value *address_expr_codegen(address_expr *expr) {
    return nullptr;
}

Value *undefined_expr_codegen() {
    return UndefValue::get(Type::getInt64Ty(context));
}

Value *expr_codegen(expression *expr) {
    switch (expr->type) {
    case BINARY_EXPR: return binary_expr_codegen(expr->binary_expr);
    case UNARY_EXPR: return unary_expr_codegen(expr->unary_expr);
    case LITERAL_EXPR: return literal_expr_codegen(expr->literal_expr);
    case ARRAY_EXPR: return array_expr_codegen(expr->array_expr);
    case CALL_EXPR: return call_expr_codegen(expr->call_expr);
    case LVALUE_EXPR: return lvalue_expr_codegen(expr->lvalue_expr);
    case ASSIGN_EXPR: return assign_expr_codegen(expr->assign_expr);
    case ADDRESS_EXPR: return address_expr_codegen(expr->address_expr);
    case UNDEFINED: return undefined_expr_codegen();
    default: return nullptr;
    }
}

void cxx_entry_point(sast *sast) {
    print_sast(sast);
}
