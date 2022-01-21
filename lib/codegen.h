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

#include <algorithm>
#include <string>
#include <vector>
#include <map>

#include "lib.h"

using namespace llvm;

StructType *struct_name_to_llvm_type(char *cname);

Type *emu_to_llvm_type(decorated_type *dec_type);

Value *binary_expr_codegen(binary_expr *expr);

Value *unary_expr_codegen(unary_expr *expr);

Value *literal_expr_codegen(literal_expr *expr);

Value *array_expr_codegen(array_expr *expr);

Value *call_expr_codegen(call_expr *expr);

Value *cast_expr_codegen(cast_expr *expr);

Value *lvalue_codegen(lvalue *lvalue);

Value *lvalue_expr_codegen(lvalue_expr *expr);

Value *assign_expr_codegen(assign_expr *expr);

Value *address_expr_codegen(address_expr *expr);

Value *crement_expr_codegen(crement_expr *expr);

Value *undefined_expr_codegen();

Value *expr_codegen(expression *expr);

Value *expr_stmt_codegen(expr_stmt *stmt);

Value *ifelse_stmt_codegen(ifelse_stmt *stmt);

Value *dowhile_stmt_codegen(dowhile_stmt *stmt);

Value *return_stmt_codegen(return_stmt *stmt);

Value *block_codegen(declaration *body, u64 block_size);

Value *empty_codegen();

Value *stmt_codegen(statement *stmt);

Value *struct_decl_codegen(struct_decl *decl);

Function *func_decl_codegen(func_decl *decl);

Value *var_decl_codegen(var_decl *decl);

Value *stmt_decl_codegen(stmt_decl *decl);

Value *decl_codegen(declaration *decl);

void cxx_entry_point(sast *sast);
