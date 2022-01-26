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

StructType* Codegen::struct_name_to_llvm_type(char *cname) {
    std::string name(cname);
    auto def_emu = defined_structs.at(name);
    std::vector<Type*> def_llvm;
    for (size_t i = 0; i < def_emu.size(); ++i) {
	def_llvm.push_back(emu_to_llvm_type(def_emu.at(i)));
    }
    return StructType::get(*context, def_llvm);
}

Type* Codegen::emu_to_llvm_type(decorated_type *dec_type) {
    switch(dec_type->decorated_type_e) {
    case PURE_TYPE: {
	type *pure_type = dec_type->pure_type;
	switch (pure_type->type_e) {
	case VOID: return Type::getVoidTy(*context);
	case BOOL: return Type::getInt1Ty(*context);
	case U8: return Type::getInt8Ty(*context);
	case U16: return Type::getInt16Ty(*context);
	case U32: return Type::getInt32Ty(*context);
	case U64: return Type::getInt64Ty(*context);
	case I8: return Type::getInt8Ty(*context);
	case I16: return Type::getInt16Ty(*context);
	case I32: return Type::getInt32Ty(*context);
	case I64: return Type::getInt64Ty(*context);
	case F32: return Type::getFloatTy(*context);
	case F64: return Type::getDoubleTy(*context);
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

void Codegen::clear_recent_locals() {
    u64 scoped_name_index = local_names.size() - 1;
    while (local_names.size() > 0 && std::get<1>(local_names.at(scoped_name_index)) == scope_level) {
	bound_named_allocas.erase(bound_named_allocas.find(std::get<0>(local_names.at(scoped_name_index))));
	local_names.pop_back();
	if (scoped_name_index == 0) break;
	--scoped_name_index;
    }
}

Value* Codegen::binary_expr_codegen(binary_expr *expr) {
    Value *v1 = expr_codegen(expr->expr1);
    Value *v2 = expr_codegen(expr->expr2);
    if (!v1 || !v2) return nullptr;
    switch (expr->op) {
    case LOGIC_OR: return builder->CreateLogicalOr(v1, v2);
    case LOGIC_XOR: return builder->CreateLogicalAnd(builder->CreateLogicalOr(v1, v2), builder->CreateICmpEQ(builder->CreateLogicalAnd(v1, v2), ConstantInt::get(*context, APInt())));
    case LOGIC_AND: return builder->CreateLogicalAnd(v1, v2);
    case BITWISE_OR: return builder->CreateOr(v1, v2);
    case BITWISE_XOR: return builder->CreateXor(v1, v2);
    case BITWISE_AND: return builder->CreateAnd(v1, v2);
    case EQUALS_EQUALS: return is_floating(expr->type) ? builder->CreateFCmpOEQ(v1, v2) : builder->CreateICmpEQ(v1, v2);
    case EXCLA_EQUALS: return is_floating(expr->type) ? builder->CreateFCmpONE(v1, v2) : builder->CreateICmpNE(v1, v2);
    case GREATER: return
	    is_floating(expr->type)
	    ? builder->CreateFCmpOGT(v1, v2)
	    : is_signed(expr->type)
	    ? builder->CreateICmpSGT(v1, v2)
	    : builder->CreateICmpUGT(v1, v2);
    case LESSER: return
	    is_floating(expr->type)
	    ? builder->CreateFCmpOLT(v1, v2)
	    : is_signed(expr->type)
	    ? builder->CreateICmpSLT(v1, v2)
	    : builder->CreateICmpULT(v1, v2);
    case GREATER_EQUALS: return
	    is_floating(expr->type)
	    ? builder->CreateFCmpOGE(v1, v2)
	    : is_signed(expr->type)
	    ? builder->CreateICmpSGE(v1, v2)
	    : builder->CreateICmpUGE(v1, v2);
    case LESSER_EQUALS: return
	    is_floating(expr->type)
	    ? builder->CreateFCmpOLE(v1, v2)
	    : is_signed(expr->type)
	    ? builder->CreateICmpSLE(v1, v2)
	    : builder->CreateICmpULE(v1, v2);
    case LSHIFT: return builder->CreateShl(v1, v2);
    case RSHIFT: return is_signed(expr->type) ? builder->CreateAShr(v1, v2) : builder->CreateLShr(v1, v2);
    case TERM_PLUS: return
	    is_floating(expr->type)
	    ? builder->CreateFAdd(v1, v2)
	    : is_pointer(expr->left_type)
	    ? builder->CreateGEP(emu_to_llvm_type(expr->type->deref_type), v1, v2)
	    : is_pointer(expr->right_type)
	    ? builder->CreateGEP(emu_to_llvm_type(expr->type->deref_type), v2, v1)
	    : builder->CreateAdd(v1, v2);
    case TERM_MINUS: return
	    is_floating(expr->type)
	    ? builder->CreateFSub(v1, v2)
	    : is_pointer(expr->left_type)
	    ? builder->CreateGEP(emu_to_llvm_type(expr->type->deref_type), v1, v2)
	    : is_pointer(expr->right_type)
	    ? builder->CreateGEP(emu_to_llvm_type(expr->type->deref_type), v2, v1)
	    : builder->CreateSub(v1, v2);
    case FACTOR_STAR: return is_floating(expr->type) ? builder->CreateFMul(v1, v2) : builder->CreateMul(v1, v2);
    case FACTOR_SLASH: return
	    is_floating(expr->type)
	    ? builder->CreateFDiv(v1, v2)
	    : is_signed(expr->type)
	    ? builder->CreateSDiv(v1, v2)
	    : builder->CreateUDiv(v1, v2);
    case FACTOR_PERCENT: return is_signed(expr->type) ? builder->CreateSRem(v1, v2) : builder->CreateURem(v1, v2);
    default: return nullptr;
    }
}

Value* Codegen::unary_expr_codegen(unary_expr *expr) {
    Value *v = expr_codegen(expr->expr);
    if (!v) return nullptr;
    switch (expr->op) {
    case PLUS: return v;
    case MINUS: return is_floating(expr->type) ? builder->CreateFNeg(v) : builder->CreateNeg(v);
    case EXCLA: return builder->CreateICmpEQ(v, ConstantInt::get(*context, APInt()));
    case TILDA: return builder->CreateNot(v);
    default: return nullptr;
    }
}

Constant* Codegen::literal_expr_codegen(literal_expr *expr) {
    comptime_value *cv = expr->comptime_value;
    std::function<Constant*(comptime_value*)> lambda = [&](comptime_value *cv) -> Constant* {
	switch (cv->type) {
	case CT_PTR: return ConstantExpr::getIntToPtr(ConstantInt::get(*context, APInt(64, cv->comptime_ptr, false)), emu_to_llvm_type(cv->ptr_type));
	case CT_BOOL: return cv->comptime_bool ? ConstantInt::getTrue(*context) : ConstantInt::getFalse(*context);
	case CT_U8: return ConstantInt::get(*context, APInt(8, cv->comptime_u8, false));
	case CT_U16: return ConstantInt::get(*context, APInt(16, cv->comptime_u16, false));
	case CT_U32: return ConstantInt::get(*context, APInt(32, cv->comptime_u32, false));
	case CT_U64: return ConstantInt::get(*context, APInt(64, cv->comptime_u64, false));
	case CT_I8: return ConstantInt::get(*context, APInt(8, cv->comptime_i8, true));
	case CT_I16: return ConstantInt::get(*context, APInt(16, cv->comptime_i16, true));
	case CT_I32: return ConstantInt::get(*context, APInt(32, cv->comptime_i32, true));
	case CT_I64: return ConstantInt::get(*context, APInt(64, cv->comptime_i64, true));
	case CT_F32: return ConstantFP::get(*context, APFloat(cv->comptime_f32));
	case CT_F64: return ConstantFP::get(*context, APFloat(cv->comptime_f64));
	case CT_STRUCT: {
	    std::vector<Constant*> struct_values;
	    for (u64 i = 0; i < cv->num_fields; ++i) {
		struct_values.push_back(lambda(cv->fields + i));
	    }
	    return ConstantStruct::get(struct_name_to_llvm_type(cv->struct_name), struct_values);
	}
	case CT_ARR: {
	    std::vector<Constant*> array_values;
	    for (u64 i = 0; i < cv->size; ++i) {
		array_values.push_back(lambda(cv->elements + i));
	    }
	    return ConstantArray::get(ArrayType::get(emu_to_llvm_type(cv->array_type), cv->size), array_values);
	}
	default: return nullptr;
	};
    };
    return lambda(cv);
}

Value* Codegen::array_expr_codegen(array_expr *expr) {
    ArrayType *array_type = ArrayType::get(emu_to_llvm_type(expr->element_type), expr->size);
    AllocaInst *alloca = builder->CreateAlloca(array_type, ConstantInt::get(*context, APInt(64, expr->size, false)));
    for(u64 i = 0; i < expr->size; ++i) {
	Value *element = expr_codegen(expr->elements + i);
	builder->CreateInsertElement(alloca, element, i);
    }
    return alloca;
}

Value* Codegen::call_expr_codegen(call_expr *expr) {
    Function *to_call = module->getFunction(expr->func_name);
    std::vector<Value*> args;
    for (u64 i = 0; i < expr->num_args; ++i) {
	Value *arg = expr_codegen(expr->args + i);
	if (!arg) return nullptr;
	args.push_back(arg);
    }
    return builder->CreateCall(to_call, args);
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
    /*                                                                    TO                                                                  */
    /*                  VOID      BOOL      U8        U16       U32       U64       I8        I16       I32       I64       F32       F64     */
    /*      VOID */    {BITCAST , TRUNC   , TRUNC   , TRUNC   , TRUNC   , BITCAST , TRUNC   , TRUNC   , TRUNC   , BITCAST , TRUNC   , BITCAST },
    /*      BOOL */    {ZEXT    , BITCAST , ZEXT    , ZEXT    , ZEXT    , ZEXT    , ZEXT    , ZEXT    , ZEXT    , ZEXT    , UITOFP  , UITOFP  },
    /*      U8   */    {ZEXT    , TRUNC   , BITCAST , ZEXT    , ZEXT    , ZEXT    , BITCAST , ZEXT    , ZEXT    , ZEXT    , UITOFP  , UITOFP  },
    /*      U16  */    {ZEXT    , TRUNC   , TRUNC   , BITCAST , ZEXT    , ZEXT    , TRUNC   , BITCAST , ZEXT    , ZEXT    , UITOFP  , UITOFP  },
    /*      U32  */    {ZEXT    , TRUNC   , TRUNC   , TRUNC   , BITCAST , ZEXT    , TRUNC   , TRUNC   , BITCAST , ZEXT    , UITOFP  , UITOFP  },
    /* FROM U64  */    {BITCAST , TRUNC   , TRUNC   , TRUNC   , TRUNC   , BITCAST , TRUNC   , TRUNC   , TRUNC   , BITCAST , UITOFP  , UITOFP  },
    /*      I8   */    {ZEXT    , TRUNC   , BITCAST , SEXT    , SEXT    , SEXT    , BITCAST , SEXT    , SEXT    , SEXT    , UITOFP  , SITOFP  },
    /*      I16  */    {ZEXT    , TRUNC   , TRUNC   , BITCAST , SEXT    , SEXT    , TRUNC   , BITCAST , SEXT    , SEXT    , UITOFP  , SITOFP  },
    /*      I32  */    {ZEXT    , TRUNC   , TRUNC   , TRUNC   , BITCAST , SEXT    , TRUNC   , TRUNC   , BITCAST , SEXT    , UITOFP  , SITOFP  },
    /*      I64  */    {BITCAST , TRUNC   , TRUNC   , TRUNC   , TRUNC   , BITCAST , TRUNC   , TRUNC   , TRUNC   , BITCAST , UITOFP  , SITOFP  },
    /*      F32  */    {ZEXT    , TRUNC   , FPTOUI  , FPTOUI  , FPTOUI  , FPTOUI  , FPTOSI  , FPTOSI  , FPTOSI  , FPTOSI  , BITCAST , FPEXT   },
    /*      F64  */    {BITCAST , TRUNC   , FPTOUI  , FPTOUI  , FPTOUI  , FPTOUI  , FPTOSI  , FPTOSI  , FPTOSI  , FPTOSI  , FPTRUNC , BITCAST },
};

Value* Codegen::cast_expr_codegen(cast_expr *expr) {
    if (is_pointer(expr->in_type) && is_pointer(expr->out_type))
	return builder->CreateCast(BITCAST, expr_codegen(expr->expr), emu_to_llvm_type(expr->out_type));
    if (expr->in_type->decorated_type_e == DEREF_TYPE && expr->out_type->decorated_type_e == PURE_TYPE)
	return builder->CreateCast(PTRTOINT, expr_codegen(expr->expr), emu_to_llvm_type(expr->out_type));
    if (expr->in_type->decorated_type_e == PURE_TYPE && expr->out_type->decorated_type_e == DEREF_TYPE)
	return builder->CreateCast(INTTOPTR, expr_codegen(expr->expr), emu_to_llvm_type(expr->out_type));
    if (expr->in_type->decorated_type_e == PURE_TYPE && expr->out_type->decorated_type_e == PURE_TYPE
	&& expr->in_type->pure_type->type_e != STRUCT && expr->out_type->pure_type->type_e != STRUCT) {
	if (expr->in_type->pure_type->type_e > BOOL && expr->out_type->pure_type->type_e == BOOL)
	    return builder->CreateNot(builder->CreateICmpEQ(builder->CreateCast(ZEXT,
									     expr_codegen(expr->expr),
									     Type::getInt64Ty(*context)),
							  ConstantInt::get(*context, APInt())));
	return builder->CreateCast(simple_cast_rules[expr->in_type->pure_type->type_e][expr->out_type->pure_type->type_e],
				  expr_codegen(expr->expr),
				  emu_to_llvm_type(expr->out_type));
    }
    return nullptr;
}

Value* Codegen::lvalue_codegen(lvalue *lvalue) {
    switch (lvalue->type) {
    case DEREF: return expr_codegen(lvalue->dereferenced);
    case ACCESS: return builder->CreateStructGEP(emu_to_llvm_type(lvalue->decorated_type), lvalue_codegen(lvalue->accessed), lvalue->offset);
    case INDEX: return builder->CreateGEP(emu_to_llvm_type(lvalue->decorated_type), lvalue_codegen(lvalue->indexed), expr_codegen(lvalue->index));
    case IDENTIFIER: return bound_named_allocas.at(std::string(lvalue->name));
    default: return nullptr;
    }
}

Value* Codegen::lvalue_expr_codegen(lvalue_expr *expr) {
    lvalue *lval = expr->lvalue;
    return builder->CreateLoad(emu_to_llvm_type(lval->decorated_type), lvalue_codegen(lval));
}

Value* Codegen::assign_expr_codegen(assign_expr *expr) {
    Value *left = lvalue_codegen(expr->lvalue);
    Value *right = expr_codegen(expr->expr);
    Value *result;

#define CREATE_BINARY(bop) {						\
	lvalue_expr lvalue = {.lvalue = expr->lvalue};			\
	expression lvalue_expr = {.type = LVALUE_EXPR, .lvalue_expr = &lvalue}; \
	binary_expr binary = {.op = bop, .expr1 = &lvalue_expr, .expr2 = expr->expr, .type = expr->left_type, .left_type = expr->left_type, .right_type = expr->right_type}; \
	result = binary_expr_codegen(&binary);				\
	break;								\
    }

    switch (expr->op) {
    case EQUALS: {
	result = right;
	break;
    }
    case PLUS_EQUALS: CREATE_BINARY(TERM_PLUS);
    case MINUS_EQUALS: CREATE_BINARY(TERM_MINUS);
    case STAR_EQUALS: CREATE_BINARY(FACTOR_STAR);
    case SLASH_EQUALS: CREATE_BINARY(FACTOR_SLASH);
    case PERCENT_EQUALS: CREATE_BINARY(FACTOR_PERCENT);
    case LSHIFT_EQUALS: CREATE_BINARY(LSHIFT);
    case RSHIFT_EQUALS: CREATE_BINARY(RSHIFT);
    case HAT_EQUALS: CREATE_BINARY(BITWISE_XOR);
    case BAR_EQUALS: CREATE_BINARY(BITWISE_OR);
    case AND_EQUALS: CREATE_BINARY(BITWISE_AND);
    }
    return builder->CreateStore(result, left);
}

Value* Codegen::address_expr_codegen(address_expr *expr) {
    return lvalue_codegen(expr->lvalue);
}

Value* Codegen::crement_expr_codegen(crement_expr *expr) {
    Value *lval = lvalue_codegen(expr->lvalue);
    if (!lval) return nullptr;
    Type *term_type = emu_to_llvm_type(expr->type);
    switch (expr->op) {
    case PRE_PLUS_PLUS: {
	if (expr->type->decorated_type_e == DEREF_TYPE) {
	    return builder->CreateStore(builder->CreateGEP(term_type, lval, ConstantInt::get(*context, APInt(64, 1, false))), lval);
	}
	Value *load = builder->CreateLoad(term_type, lval);
	switch (expr->type->pure_type->type_e) {
	case U8: return builder->CreateStore(builder->CreateAdd(load, ConstantInt::get(*context, APInt(8, 1, false))), lval);
	case U16: return builder->CreateStore(builder->CreateAdd(load, ConstantInt::get(*context, APInt(16, 1, false))), lval);
	case U32: return builder->CreateStore(builder->CreateAdd(load, ConstantInt::get(*context, APInt(32, 1, false))), lval);
	case U64: return builder->CreateStore(builder->CreateAdd(load, ConstantInt::get(*context, APInt(64, 1, false))), lval);
	case I8: return builder->CreateStore(builder->CreateAdd(load, ConstantInt::get(*context, APInt(8, 1, true))), lval);
	case I16: return builder->CreateStore(builder->CreateAdd(load, ConstantInt::get(*context, APInt(16, 1, true))), lval);
	case I32: return builder->CreateStore(builder->CreateAdd(load, ConstantInt::get(*context, APInt(32, 1, true))), lval);
	case I64: return builder->CreateStore(builder->CreateAdd(load, ConstantInt::get(*context, APInt(64, 1, true))), lval);
	case F32: return builder->CreateStore(builder->CreateFAdd(load, ConstantFP::get(*context, APFloat(1.0))), lval); 
	case F64: return builder->CreateStore(builder->CreateFAdd(load, ConstantFP::get(*context, APFloat(1.0))), lval);
	default: return nullptr;
	}
    }
    case PRE_MINUS_MINUS: {
	if (expr->type->decorated_type_e == DEREF_TYPE) {
	    return builder->CreateStore(builder->CreateGEP(term_type, lval, ConstantInt::get(*context, APInt(64, -1, false))), lval);
	}
	Value *load = builder->CreateLoad(term_type, lval);
	switch (expr->type->pure_type->type_e) {
	case U8: return builder->CreateStore(builder->CreateSub(load, ConstantInt::get(*context, APInt(8, 1, false))), lval);
	case U16: return builder->CreateStore(builder->CreateSub(load, ConstantInt::get(*context, APInt(16, 1, false))), lval);
	case U32: return builder->CreateStore(builder->CreateSub(load, ConstantInt::get(*context, APInt(32, 1, false))), lval);
	case U64: return builder->CreateStore(builder->CreateSub(load, ConstantInt::get(*context, APInt(64, 1, false))), lval);
	case I8: return builder->CreateStore(builder->CreateSub(load, ConstantInt::get(*context, APInt(8, 1, true))), lval);
	case I16: return builder->CreateStore(builder->CreateSub(load, ConstantInt::get(*context, APInt(16, 1, true))), lval);
	case I32: return builder->CreateStore(builder->CreateSub(load, ConstantInt::get(*context, APInt(32, 1, true))), lval);
	case I64: return builder->CreateStore(builder->CreateSub(load, ConstantInt::get(*context, APInt(64, 1, true))), lval);
	case F32: return builder->CreateStore(builder->CreateFSub(load, ConstantFP::get(*context, APFloat(1.0))), lval); 
	case F64: return builder->CreateStore(builder->CreateFSub(load, ConstantFP::get(*context, APFloat(1.0))), lval);
	default: return nullptr;
	}
    }
    case POST_PLUS_PLUS: {
	Value *load = builder->CreateLoad(term_type, lval);
	if (expr->type->decorated_type_e == DEREF_TYPE) {
	    builder->CreateStore(builder->CreateGEP(term_type, lval, ConstantInt::get(*context, APInt(64, 1, false))), lval);
	}
	else {
	    switch (expr->type->pure_type->type_e) {
	    case U8: return builder->CreateStore(builder->CreateAdd(load, ConstantInt::get(*context, APInt(8, 1, false))), lval);
	    case U16: return builder->CreateStore(builder->CreateAdd(load, ConstantInt::get(*context, APInt(16, 1, false))), lval);
	    case U32: return builder->CreateStore(builder->CreateAdd(load, ConstantInt::get(*context, APInt(32, 1, false))), lval);
	    case U64: return builder->CreateStore(builder->CreateAdd(load, ConstantInt::get(*context, APInt(64, 1, false))), lval);
	    case I8: return builder->CreateStore(builder->CreateAdd(load, ConstantInt::get(*context, APInt(8, 1, true))), lval);
	    case I16: return builder->CreateStore(builder->CreateAdd(load, ConstantInt::get(*context, APInt(16, 1, true))), lval);
	    case I32: return builder->CreateStore(builder->CreateAdd(load, ConstantInt::get(*context, APInt(32, 1, true))), lval);
	    case I64: return builder->CreateStore(builder->CreateAdd(load, ConstantInt::get(*context, APInt(64, 1, true))), lval);
	    case F32: return builder->CreateStore(builder->CreateFAdd(load, ConstantFP::get(*context, APFloat(1.0))), lval); 
	    case F64: return builder->CreateStore(builder->CreateFAdd(load, ConstantFP::get(*context, APFloat(1.0))), lval);
	    default: return nullptr;
	    }
	}
	return load;
    }
    case POST_MINUS_MINUS: {
	Value *load = builder->CreateLoad(term_type, lval);
	if (expr->type->decorated_type_e == DEREF_TYPE) {
	    builder->CreateStore(builder->CreateGEP(term_type, lval, ConstantInt::get(*context, APInt(64, -1, false))), lval);
	}
	else {
	switch (expr->type->pure_type->type_e) {
	    case U8: return builder->CreateStore(builder->CreateSub(load, ConstantInt::get(*context, APInt(8, 1, false))), lval);
	    case U16: return builder->CreateStore(builder->CreateSub(load, ConstantInt::get(*context, APInt(16, 1, false))), lval);
	    case U32: return builder->CreateStore(builder->CreateSub(load, ConstantInt::get(*context, APInt(32, 1, false))), lval);
	    case U64: return builder->CreateStore(builder->CreateSub(load, ConstantInt::get(*context, APInt(64, 1, false))), lval);
	    case I8: return builder->CreateStore(builder->CreateSub(load, ConstantInt::get(*context, APInt(8, 1, true))), lval);
	    case I16: return builder->CreateStore(builder->CreateSub(load, ConstantInt::get(*context, APInt(16, 1, true))), lval);
	    case I32: return builder->CreateStore(builder->CreateSub(load, ConstantInt::get(*context, APInt(32, 1, true))), lval);
	    case I64: return builder->CreateStore(builder->CreateSub(load, ConstantInt::get(*context, APInt(64, 1, true))), lval);
	    case F32: return builder->CreateStore(builder->CreateFSub(load, ConstantFP::get(*context, APFloat(1.0))), lval); 
	    case F64: return builder->CreateStore(builder->CreateFSub(load, ConstantFP::get(*context, APFloat(1.0))), lval);
	    default: return nullptr;
	    }
	}
	return load;
    }
    default: return nullptr;
    }
}

Value* Codegen::undefined_expr_codegen() {
    return UndefValue::get(Type::getVoidTy(*context));
}

Value* Codegen::expr_codegen(expression *expr) {
    switch (expr->type) {
    case BINARY_EXPR: return binary_expr_codegen(expr->binary_expr);
    case UNARY_EXPR: return unary_expr_codegen(expr->unary_expr);
    case LITERAL_EXPR: return literal_expr_codegen(expr->literal_expr);
    case ARRAY_EXPR: return array_expr_codegen(expr->array_expr);
    case CALL_EXPR: return call_expr_codegen(expr->call_expr);
    case CAST_EXPR: return cast_expr_codegen(expr->cast_expr);
    case LVALUE_EXPR: return lvalue_expr_codegen(expr->lvalue_expr);
    case ASSIGN_EXPR: return assign_expr_codegen(expr->assign_expr);
    case ADDRESS_EXPR: return address_expr_codegen(expr->address_expr);
    case CREMENT_EXPR: return crement_expr_codegen(expr->crement_expr);
    case UNDEFINED: return undefined_expr_codegen();
    default: return nullptr;
    }
}

Value* Codegen::expr_stmt_codegen(expr_stmt *stmt) {
    return expr_codegen(stmt->expr);
}

Value* Codegen::ifelse_stmt_codegen(ifelse_stmt *stmt) {
    ++scope_level;
    Value *cond = expr_codegen(stmt->cond);
    if (!cond) return nullptr;
    Function *cur_function = builder->GetInsertBlock()->getParent();
    BasicBlock *thenBB = BasicBlock::Create(*context, "", cur_function);
    BasicBlock *elseBB = BasicBlock::Create(*context);
    BasicBlock *mergeBB;
    if (!stmt->pos_terms || !stmt->neg_terms) mergeBB = BasicBlock::Create(*context);
    builder->CreateCondBr(cond, thenBB, elseBB);
    builder->SetInsertPoint(thenBB);
    Value *pos = stmt_codegen(stmt->pos);
    if (!pos) return nullptr;
    if (!stmt->pos_terms) builder->CreateBr(mergeBB);
    thenBB = builder->GetInsertBlock();
    cur_function->getBasicBlockList().push_back(elseBB);
    builder->SetInsertPoint(elseBB);
    Value *neg = stmt_codegen(stmt->neg);
    if (!neg) return nullptr;
    if (!stmt->neg_terms) builder->CreateBr(mergeBB);
    elseBB = builder->GetInsertBlock();
    if (!stmt->pos_terms || !stmt->neg_terms) {
	cur_function->getBasicBlockList().push_back(mergeBB);
	builder->SetInsertPoint(mergeBB);
    }
    clear_recent_locals();
    --scope_level;
    return Constant::getNullValue(Type::getVoidTy(*context));;
}

Value* Codegen::dowhile_stmt_codegen(dowhile_stmt *stmt) {
    ++scope_level;
    Function *cur_function = builder->GetInsertBlock()->getParent();
    BasicBlock *whileBB = BasicBlock::Create(*context, "", cur_function);
    BasicBlock *mergeBB;
    if (!stmt->terms) mergeBB = BasicBlock::Create(*context);
    builder->CreateBr(whileBB);
    builder->SetInsertPoint(whileBB);
    Value *body = stmt_codegen(stmt->body);
    if (!body) return nullptr;
    Value *cond = expr_codegen(stmt->cond);
    if (!stmt->terms) builder->CreateCondBr(cond, whileBB, mergeBB);
    whileBB = builder->GetInsertBlock();
    if (!stmt->terms) {
	cur_function->getBasicBlockList().push_back(mergeBB);
	builder->SetInsertPoint(mergeBB);
    }
    clear_recent_locals();
    --scope_level;
    return Constant::getNullValue(Type::getVoidTy(*context));;
}

Value* Codegen::return_stmt_codegen(return_stmt *stmt) {
    if (stmt->expr->type == UNDEFINED) return builder->CreateRetVoid();
    Value *ret = expr_codegen(stmt->expr);
    if (ret->getType()->isVoidTy()) return builder->CreateRetVoid();
    else return builder->CreateRet(ret);
}

Value* Codegen::block_codegen(declaration *body, u64 block_size) {
    ++scope_level;
    for (u64 i = 0; i < block_size; ++i) decl_codegen(body + i);
    clear_recent_locals();
    --scope_level;
    return undefined_expr_codegen();
}

Value* Codegen::empty_codegen() {
    return undefined_expr_codegen();
}

Value* Codegen::stmt_codegen(statement *stmt) {
    switch (stmt->type) {
    case EXPR_STMT: return expr_stmt_codegen(stmt->expr_stmt);
    case IFELSE_STMT: return ifelse_stmt_codegen(stmt->ifelse_stmt);
    case DOWHILE_STMT: return dowhile_stmt_codegen(stmt->dowhile_stmt);
    case RETURN_STMT: return return_stmt_codegen(stmt->return_stmt);
    case BLOCK: return block_codegen(stmt->block, stmt->block_size);
    case EMPTY: return empty_codegen();
    default: return nullptr;
    }
}

Value* Codegen::struct_decl_codegen(struct_decl *decl) {
    std::vector<decorated_type*> fields;
    for (u64 i = 0; i < decl->num_fields; ++i) {
	fields.push_back((decl->fields + i)->type);
    }
    defined_structs[std::string(decl->name)] = fields;
    return Constant::getNullValue(Type::getVoidTy(*context));;
}

Function* Codegen::func_decl_codegen(func_decl *decl) {
    inside_function = true;
    std::vector<Type*> args_llvm;
    for (u64 i = 0; i < decl->num_params; ++i) {
	args_llvm.push_back(emu_to_llvm_type((decl->params + i)->type));
    }
    FunctionType *ft = FunctionType::get(emu_to_llvm_type(decl->ret_type), args_llvm, false);
    Function *f = Function::Create(ft, Function::ExternalLinkage, std::string(decl->name), *module);
    u64 i = 0;
    for (auto &arg : f->args()) arg.setName(std::string((decl->params + i++)->name));

    BasicBlock *bb = BasicBlock::Create(*context, "", f);
    builder->SetInsertPoint(bb);
    ++scope_level;
    i = 0;
    for (auto &arg : f->args()) {
	AllocaInst *alloca = builder->CreateAlloca(args_llvm.at(i), nullptr, (decl->params + i)->name);
	builder->CreateStore(&arg, alloca);
	bound_named_allocas[std::string((decl->params + i)->name)] = alloca;
    }
    Value *body = stmt_codegen(decl->body);
    if (!body) return nullptr;
    verifyFunction(*f);
    fpm->run(*f);
    clear_recent_locals();
    --scope_level;
    
    inside_function = false;
    return f;
}

Value* Codegen::var_decl_codegen(var_decl *decl) {
    if (inside_function) {
	Value *expr_v = expr_codegen(decl->init);
	if (!expr_v) return nullptr;
	AllocaInst *alloca = builder->CreateAlloca(emu_to_llvm_type(decl->iden->type));
	local_names.push_back(std::make_pair(std::string(decl->iden->name), scope_level));
	bound_named_allocas[std::string(decl->iden->name)] = alloca;
	return builder->CreateStore(expr_v, alloca);
    }
    else {
	Constant *expr_c = literal_expr_codegen(decl->init->literal_expr);
	if (!expr_c) return nullptr;
	GlobalVariable *gv = new GlobalVariable(*module, emu_to_llvm_type(decl->iden->type), false, GlobalValue::InternalLinkage, expr_c);
	bound_named_allocas[std::string(decl->iden->name)] = gv;
	return gv;
    }
}

Value* Codegen::stmt_decl_codegen(stmt_decl *decl) {
    return stmt_codegen(decl->stmt);
}

Value* Codegen::decl_codegen(declaration *decl) {
    switch (decl->type) {
    case STRUCT_DECL: return struct_decl_codegen(decl->struct_decl);
    case FUNC_DECL: return func_decl_codegen(decl->func_decl);
    case VAR_DECL: return var_decl_codegen(decl->var_decl);
    case STMT_DECL: return stmt_decl_codegen(decl->stmt_decl);
    default: return nullptr;
    }
}

int Codegen::codegen(sast *sast, std::string out_file) {
    context = std::make_unique<LLVMContext>();
    module = std::make_unique<Module>("module", *context);
    builder = std::make_unique<IRBuilder<>>(*context);
    fpm = std::make_unique<legacy::FunctionPassManager>(module.get());

    fpm->add(createPromoteMemoryToRegisterPass());
    fpm->add(createInstructionCombiningPass());
    fpm->add(createReassociatePass());
    fpm->add(createGVNPass());
    fpm->add(createCFGSimplificationPass());
    fpm->doInitialization();

    for (u64 i = 0; i < sast->num_decls; i++) {
	decl_codegen(sast->decls + i);
    }

    //print_sast(sast);
    //module->print(errs(), nullptr);
    auto targetTriple = getDefaultTargetTriple();
    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmParsers();
    InitializeAllAsmPrinters();

    std::string error;
    auto target = TargetRegistry::lookupTarget(targetTriple, error);
    if (!target) {
	errs() << error;
	return 1;
    }
    auto cpu = "generic";
    auto features = "";
    TargetOptions opt;
    auto rm = Optional<Reloc::Model>();
    auto targetMachine = target->createTargetMachine(targetTriple, cpu, features, opt, rm);

    module->setDataLayout(targetMachine->createDataLayout());
    module->setTargetTriple(targetTriple);

    std::error_code ec;
    raw_fd_ostream dest(out_file, ec, fs::OF_None);

    if (ec) {
	errs() << "Could not open file " << out_file << ": " << ec.message();
	return 1;
    }

    LoopAnalysisManager lam;
    FunctionAnalysisManager fam;
    CGSCCAnalysisManager cgam;
    ModuleAnalysisManager mam;
    PassBuilder pb(targetMachine);

    fam.registerPass([&] { return pb.buildDefaultAAPipeline(); });
    pb.registerModuleAnalyses(mam);
    pb.registerCGSCCAnalyses(cgam);
    pb.registerFunctionAnalyses(fam);
    pb.registerLoopAnalyses(lam);
    pb.crossRegisterProxies(lam, fam, cgam, mam);
    ModulePassManager mpm = pb.buildPerModuleDefaultPipeline(PassBuilder::OptimizationLevel::O2);
    mpm.run(*module, mam);
    //module->print(errs(), nullptr);

    legacy::PassManager pass;
    auto fileType = CGFT_ObjectFile;

    if (targetMachine->addPassesToEmitFile(pass, dest, nullptr, fileType)) {
	errs() << "TargetMachine can't emit a file of this type.";
	return 1;
    }
    pass.run(*module);
    dest.flush();

    destruct_sast(sast);
    free(sast);
    
    return 0;
}

int cxx_entry_point(sast *sast, char* out_file) {
    Codegen cg;
    return cg.codegen(sast, std::string(out_file));
}
