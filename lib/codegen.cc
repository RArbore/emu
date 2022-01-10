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

#include "lib.h"

using namespace llvm;

static LLVMContext context;
static IRBuilder<> builder(context);
static Module *module;

Value *binary_expr_codegen(binary_expr*) {

}

Value *unary_expr_codegen(unary_expr*) {

}

Value *literal_expr_codegen(literal_expr*) {

}

Value *array_expr_codegen(array_expr*) {

}

Value *call_expr_codegen(call_expr*) {

}

Value *lvalue_expr_codegen(lvalue_expr*) {

}

Value *assign_expr_codegen(assign_expr*) {

}

Value *address_expr_codegen(address_expr*) {

}

Value *undefined_expr_codegen() {

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
    }
}

void cxx_entry_point(sast *sast) {
    print_sast(sast);
}
