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

#include <llvm/ADT/APFixedPoint.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/ADT/APFloat.h>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>

#include "lib.h"

using namespace llvm;

static LLVMContext context;
static IRBuilder<> builder(context);
static Module *module;

Type *emu_to_llvm_type(decorated_type *type) {
    return nullptr;
}

Value *binary_expr_codegen(binary_expr *expr) {
    return nullptr;
}

Value *unary_expr_codegen(unary_expr *expr) {
    return nullptr;
}

Value *literal_expr_codegen(literal_expr *expr) {
    comptime_value *cv = expr->comptime_value;
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
    case CT_STRUCT:
    case CT_ARR:
    default: return nullptr;
    }
}

Value *array_expr_codegen(array_expr *expr) {
    return nullptr;
}

Value *call_expr_codegen(call_expr *expr) {
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
    return nullptr;
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
