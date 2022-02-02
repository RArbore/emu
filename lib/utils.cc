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

#include <stdlib.h>
#include <string.h>

#include "lib.h"

void print_type(type *t) {
    switch (t->type_e) {
    case VOID: printf("Void"); break;
    case BOOL: printf("Bool"); break;
    case U8: printf("U8"); break;
    case U16: printf("U16"); break;
    case U32: printf("U32"); break;
    case U64: printf("U64"); break;
    case I8: printf("I8"); break;
    case I16: printf("I16"); break;
    case I32: printf("I32"); break;
    case I64: printf("I64"); break;
    case F32: printf("F32"); break;
    case F64: printf("F64"); break;
    case STRUCT: printf("(StructType \"%s\")", t->struct_name); break;
    default: printf("(INVALID ENUM CODE (print_type))");
    }
    fflush(stdout);
}

void print_decorated_type(decorated_type *dt) {
    switch (dt->decorated_type_e) {
    case PURE_TYPE: printf("(PureType "); print_type(dt->pure_type); printf(")"); break;
    case DEREF_TYPE: printf("(DerefType "); print_decorated_type(dt->deref_type); printf(")"); break;
    case ARRAY_TYPE: printf("(ArrayType "); print_decorated_type(dt->array_type); printf(" %lu)", dt->array_size); break;
    default: printf("(INVALID ENUM CODE (print_decorated_type))");
    }
    fflush(stdout);
}

void print_modifier(modifier mod) {
    switch (mod) {
    case PURE: printf("Pure"); break;
    case CONST: printf("Const"); break;
    case INLINE: printf("Inline"); break;
    case REGISTER: printf("Register"); break;
    case RESTRICT: printf("Restrict"); break;
    default: printf("(INVALID ENUM CODE (print_modifier))");
    }
    fflush(stdout);
}

void print_decorated_identifier(decorated_identifier *dec_iden) {
    printf("DecoratedIdentifier [");
    for (u64 i = 0; i < dec_iden->num_mods; ++i) {
	if (i) printf(",");
	print_modifier(dec_iden->mods[i]);
    }
    printf("] \"%s\" ", dec_iden->name);
    print_decorated_type(dec_iden->type);
    fflush(stdout);
}

void print_binary_op(binary_op bop) {
    switch (bop) {
    case LOGIC_OR: printf("LogicOr"); break;
    case LOGIC_XOR: printf("LogicXor"); break;
    case LOGIC_AND: printf("LogicAnd"); break;
    case BITWISE_OR: printf("BitwiseOr"); break;
    case BITWISE_XOR: printf("BitwiseXor"); break;
    case BITWISE_AND: printf("BitwiseAnd"); break;
    case EQUALS_EQUALS: printf("EqualsEquals"); break;
    case EXCLA_EQUALS: printf("ExclaEquals"); break;
    case GREATER: printf("Greater"); break;
    case LESSER: printf("Lesser"); break;
    case GREATER_EQUALS: printf("GreaterEquals"); break;
    case LESSER_EQUALS: printf("LesserEquals"); break;
    case LSHIFT: printf("LShift"); break;
    case RSHIFT: printf("RShift"); break;
    case TERM_PLUS: printf("TermPlus"); break;
    case TERM_MINUS: printf("TermMinus"); break;
    case FACTOR_STAR: printf("FactorStar"); break;
    case FACTOR_SLASH: printf("FactorSlash"); break;
    case FACTOR_PERCENT: printf("FactorPercent"); break;
    default: printf("(INVALID ENUM CODE (print_binary_op))");
    }
    fflush(stdout);
}

void print_unary_op(unary_op uop) {
    switch (uop) {
    case PLUS: printf("Plus"); break;
    case MINUS: printf("Minus"); break;
    case EXCLA: printf("Excla"); break;
    case TILDA: printf("Tilda"); break;
    default: printf("(INVALID ENUM CODE (print_unary_op))");
    }
    fflush(stdout);
}

void print_comptime_value(comptime_value *cv) {
    switch (cv->type) {
    case CT_PTR: printf("(ComptimePointer %p ", (void *) cv->comptime_ptr); print_decorated_type(cv->ptr_type); printf(")"); break;
    case CT_BOOL: printf(cv->comptime_bool ? "(ComptimeBool True)" : "(ComptimeBool False)"); break;
    case CT_U8: printf("(ComptimeU8 %u)", cv->comptime_u8); break;
    case CT_U16: printf("(ComptimeU16 %u)", cv->comptime_u16); break;
    case CT_U32: printf("(ComptimeU32 %u)", cv->comptime_u32); break;
    case CT_U64: printf("(ComptimeU64 %lu)", cv->comptime_u64); break;
    case CT_I8: printf("(ComptimeI8 %d)", cv->comptime_i8); break;
    case CT_I16: printf("(ComptimeI16 %d)", cv->comptime_i16); break;
    case CT_I32: printf("(ComptimeI32 %d)", cv->comptime_i32); break;
    case CT_I64: printf("(ComptimeI64 %ld)", cv->comptime_i64); break;
    case CT_F32: printf("(ComptimeF32 %f)", cv->comptime_f32); break;
    case CT_F64: printf("(ComptimeF64 %f)", cv->comptime_f64); break;
    case CT_STRUCT: {
	printf("(ComptimeStruct [");
	for (u64 i = 0; i < cv->num_fields; ++i) {
	    if (i) printf(",");
	    print_comptime_value(cv->fields + i);
	}
	printf("] \"%s\")", cv->struct_name);
	break;
    }
    case CT_ARR: {
	printf("(ComptimeArr [");
	for (u64 i = 0; i < cv->size; ++i) {
	    if (i) printf(",");
	    print_comptime_value(cv->elements + i);
	}
	printf("] %lu)", cv->size);
	break;
    }
    default: printf("(INVALID ENUM CODE (print_comptime_value))");
    }
    fflush(stdout);
}

void print_lvalue(lvalue *lv) {
    switch (lv->type) {
    case DEREF: printf("(Dereference "); print_expression(lv->dereferenced); printf(" "); print_decorated_type(lv->decorated_type); printf(")"); break;
    case ACCESS: printf("(Access "); print_lvalue(lv->accessed); printf(" %lu ", lv->offset); print_decorated_type(lv->decorated_type); printf(")"); break;
    case INDEX: printf("(Index "); print_lvalue(lv->indexed); printf(" "); print_expression(lv->index); printf(" "); print_decorated_type(lv->decorated_type); printf(")"); break;
    case IDENTIFIER: printf("(Identifier \"%s\" ", lv->name); print_decorated_type(lv->decorated_type); printf(")"); break;
    default: printf("(INVALID ENUM CODE (print_lvalue))");
    }
    fflush(stdout);
}

void print_assign_op(assign_op aop) {
    switch (aop) {
    case EQUALS: printf("Equals"); break;
    case PLUS_EQUALS: printf("PlusEquals"); break;
    case MINUS_EQUALS: printf("MinusEquals"); break;
    case STAR_EQUALS: printf("StarEquals"); break;
    case SLASH_EQUALS: printf("SlashEquals"); break;
    case PERCENT_EQUALS: printf("PercentEquals"); break;
    case LSHIFT_EQUALS: printf("LShiftEquals"); break;
    case RSHIFT_EQUALS: printf("RShiftEquals"); break;
    case HAT_EQUALS: printf("HatEquals"); break;
    case BAR_EQUALS: printf("BarEquals"); break;
    case AND_EQUALS: printf("AndEquals"); break;
    default: printf("(INVALID ENUM CODE (print_assign_op))");
    }
    fflush(stdout);
}

void print_crement_op(crement_op cop) {
    switch (cop) {
    case PRE_PLUS_PLUS: printf("PrePlusPlus"); break;
    case PRE_MINUS_MINUS: printf("PreMinusMinus"); break;
    case POST_PLUS_PLUS: printf("PostPlusPlus"); break;
    case POST_MINUS_MINUS: printf("PostMinusMinus"); break;
    default: printf("(INVALID ENUM CODE (print_crement_op))");
    }
    fflush(stdout);
}

void print_expression(expression *expr) {
    switch (expr->type) {
    case BINARY_EXPR: printf("(Binary "); print_binary_op(expr->binary_expr->op); printf(" "); print_expression(expr->binary_expr->expr1); printf(" "); print_expression(expr->binary_expr->expr2); printf(" "); print_decorated_type(expr->binary_expr->type); printf(")"); break; 
    case UNARY_EXPR: printf("(Unary "); print_unary_op(expr->unary_expr->op); printf(" "); print_expression(expr->unary_expr->expr); printf(" "); print_decorated_type(expr->unary_expr->type); printf(")"); break;  
    case LITERAL_EXPR: printf("(Literal "); print_comptime_value(expr->literal_expr->comptime_value); printf(")"); break;
    case ARRAY_EXPR: {
	printf("(Array [");
	for (u64 i = 0; i < expr->array_expr->size; ++i) {
	    if (i) printf(",");
	    print_expression(expr->array_expr->elements + i);
	}
	printf("] ");
	print_decorated_type(expr->array_expr->element_type);
	printf(")");
	break;
    }
    case CALL_EXPR: {
	printf("(Call \"%s\" [", expr->call_expr->func_name);
	for (u64 i = 0; i < expr->call_expr->num_args; ++i) {
	    if (i) printf(",");
	    print_expression(expr->call_expr->args + i);
	}
	printf("] ");
	print_decorated_type(expr->call_expr->result_type);
	printf(")");
	break;
    }
    case CAST_EXPR: {
	printf("(Cast ");
	print_expression(expr->cast_expr->expr);
	printf(" ");
	print_decorated_type(expr->cast_expr->in_type);
	printf(" ");
	print_decorated_type(expr->cast_expr->out_type);
	printf(")");
	break;
    }
    case LVALUE_EXPR: printf("(LValueExpression "); print_lvalue(expr->lvalue_expr->lvalue); printf(")"); break;
    case ASSIGN_EXPR: printf("(Assign "); print_assign_op(expr->assign_expr->op); printf(" "); print_lvalue(expr->assign_expr->lvalue); printf(" "); print_expression(expr->assign_expr->expr); printf(" "); print_decorated_type(expr->assign_expr->left_type); printf(" "); print_decorated_type(expr->assign_expr->right_type); printf(")"); break;
    case ADDRESS_EXPR: printf("(Address "); print_lvalue(expr->address_expr->lvalue); printf(")"); break;
    case CREMENT_EXPR: printf("(Crement "); print_crement_op(expr->crement_expr->op); printf(" "); print_lvalue(expr->crement_expr->lvalue); printf(" "); print_decorated_type(expr->crement_expr->type); printf(")"); break;  
    case UNDEFINED: printf("Undefined"); break;
    default: printf("(INVALID ENUM CODE (print_expression))");
    }
    fflush(stdout);
}

void print_statement(statement *stmt) {
    switch (stmt->type) {
    case EXPR_STMT: printf("(ExpressionStatement "); print_expression(stmt->expr_stmt->expr); printf(")"); break;
    case IFELSE_STMT: printf("(IfElseStatement "); print_expression(stmt->ifelse_stmt->cond); printf(" "); print_statement(stmt->ifelse_stmt->pos); printf(" "); print_statement(stmt->ifelse_stmt->neg); printf(")"); break;
    case DOWHILE_STMT: printf("(DoWhileStatement "); print_expression(stmt->dowhile_stmt->cond); printf(" "); print_statement(stmt->dowhile_stmt->body); printf(")"); break;
    case RETURN_STMT: printf("(ReturnStatement "); print_expression(stmt->return_stmt->expr); printf(")"); break;
    case BLOCK: {
	printf("(Block [");
	for (u64 i = 0; i < stmt->block_size; ++i) {
	    if (i) printf(",");
	    print_declaration(stmt->block + i);
	}
	printf("])");
	break;
    }
    case EMPTY: printf("EmptyStatement"); break;
    default: printf("(INVALID ENUM CODE (print_statement))");
    }
    fflush(stdout);
}

void print_declaration(declaration *decl) {
    switch (decl->type) {
    case STRUCT_DECL: {
	printf("(StructDecl (Structure [");
	for (u64 i = 0; i < decl->struct_decl->num_mods; ++i) {
	    if (i) printf(",");
	    print_modifier(decl->struct_decl->mods[i]);
	}
	printf("] \"%s\" [", decl->struct_decl->name);
	for (u64 i = 0; i < decl->struct_decl->num_fields; ++i) {
	    if (i) printf(",");
	    print_decorated_identifier(decl->struct_decl->fields + i);
	}
	printf("]))");
	break;
    }
    case FUNC_DECL: {
	printf("(FuncDecl (Function [");
	for (u64 i = 0; i < decl->func_decl->num_mods; ++i) {
	    if (i) printf(",");
	    print_modifier(decl->func_decl->mods[i]);
	}
	printf("] \"%s\" [", decl->func_decl->name);
	for (u64 i = 0; i < decl->func_decl->num_params; ++i) {
	    if (i) printf(",");
	    print_decorated_identifier(decl->func_decl->params + i);
	}
	printf("] ");
	print_decorated_type(decl->func_decl->ret_type);
	printf(" ");
	print_statement(decl->func_decl->body);
	printf("))");
	break;
    }
    case VAR_DECL: printf("(VarDecl (VarBinding "); print_decorated_identifier(decl->var_decl->iden); printf(" "); print_expression(decl->var_decl->init); printf("))"); break;
    case STMT_DECL: printf("(StatementDecl "); print_statement(decl->stmt_decl->stmt); printf(")"); break;
    default: printf("(INVALID ENUM CODE (print_declaration))");
    }
    fflush(stdout);
}

void print_sast(sast *sast) {
    printf("SAST [");
    for (u64 i = 0; i < sast->num_decls; ++i) {
	if (i) printf(",");
	print_declaration(sast->decls + i);
    }
    printf("]");
    fflush(stdout);
}

decorated_type* deepcopy_decorated_type(decorated_type *dt) {
    decorated_type* new_dt = (decorated_type*) malloc(sizeof(decorated_type));
    new_dt->decorated_type_e = dt->decorated_type_e;
    switch (new_dt->decorated_type_e) {
    case PURE_TYPE: {
	new_dt->pure_type = (type*) malloc(sizeof(type));
	new_dt->pure_type->type_e = dt->pure_type->type_e;
	if (new_dt->pure_type->type_e == STRUCT)
	    strcpy(new_dt->pure_type->struct_name, dt->pure_type->struct_name);
	break;
    }
    case DEREF_TYPE: {
	new_dt->deref_type = deepcopy_decorated_type(dt->deref_type);
	break;
    }
    case ARRAY_TYPE: {
	new_dt->array_type = deepcopy_decorated_type(dt->array_type);
	new_dt->array_size = dt->array_size;
	break;
    }
    }
    return new_dt;
}

void destruct_type(type *type) {
    if (type->type_e == STRUCT) free(type->struct_name);
}

void destruct_decorated_type(decorated_type *decorated_type) {
    switch (decorated_type->decorated_type_e) {
    case PURE_TYPE: {
	destruct_type(decorated_type->pure_type);
	free(decorated_type->pure_type);
	break;
    }
    case DEREF_TYPE: {
	destruct_decorated_type(decorated_type->deref_type);
	free(decorated_type->deref_type);
	break;
    }
    case ARRAY_TYPE: {
	destruct_decorated_type(decorated_type->array_type);
	free(decorated_type->array_type);
	break;
    }
    }
}

void destruct_decorated_identifier(decorated_identifier *decorated_identifier) {
    free(decorated_identifier->mods);
    free(decorated_identifier->name);
    destruct_decorated_type(decorated_identifier->type);
    free(decorated_identifier->type);
}

void destruct_comptime_value(comptime_value *comptime_value) {
    switch (comptime_value->type) {
    case CT_PTR: {
	destruct_decorated_type(comptime_value->ptr_type);
	free(comptime_value->ptr_type);
	break;
    }
    case CT_STRUCT: {
	for (u64 i = 0; i < comptime_value->num_fields; ++i) destruct_comptime_value(comptime_value->fields + i);
	free(comptime_value->fields);
	free(comptime_value->struct_name);
	break;
    }
    case CT_ARR: {
	for (u64 i = 0; i < comptime_value->size; ++i) destruct_comptime_value(comptime_value->elements + i);
	free(comptime_value->elements);
	destruct_decorated_type(comptime_value->array_type);
	free(comptime_value->array_type);
	break;
    }
    default: break;
    }
}

void destruct_lvalue(lvalue *lvalue) {
    switch (lvalue->type) {
    case DEREF: {
	destruct_expr(lvalue->dereferenced);
	free(lvalue->dereferenced);
	break;
    }
    case ACCESS: {
	destruct_lvalue(lvalue->accessed);
	free(lvalue->accessed);
	break;
    }
    case INDEX: {
	destruct_lvalue(lvalue->indexed);
	free(lvalue->indexed);
	destruct_expr(lvalue->index);
	free(lvalue->index);
	break;
    }
    case IDENTIFIER: {
	free(lvalue->name);
	break;
    }
    }
    destruct_decorated_type(lvalue->decorated_type);
    free(lvalue->decorated_type);
}

void destruct_expr(expression *expr) {
    switch (expr->type) {
    case BINARY_EXPR: {
	destruct_expr(expr->binary_expr->expr1);
	free(expr->binary_expr->expr1);
	destruct_expr(expr->binary_expr->expr2);
	free(expr->binary_expr->expr2);
	destruct_decorated_type(expr->binary_expr->type);
	free(expr->binary_expr->type);
	destruct_decorated_type(expr->binary_expr->left_type);
	free(expr->binary_expr->left_type);
	destruct_decorated_type(expr->binary_expr->right_type);
	free(expr->binary_expr->right_type);
	break;
    }
    case UNARY_EXPR: {
	destruct_expr(expr->unary_expr->expr);
	free(expr->unary_expr->expr);
	destruct_decorated_type(expr->unary_expr->type);
	free(expr->unary_expr->type);
	break;
    }
    case LITERAL_EXPR: {
	destruct_comptime_value(expr->literal_expr->comptime_value);
	free(expr->literal_expr->comptime_value);
	break;
    }
    case ARRAY_EXPR: {
	for (u64 i = 0; i < expr->array_expr->size; ++i) destruct_expr(expr->array_expr->elements + i);
	free(expr->array_expr->elements);
	destruct_decorated_type(expr->array_expr->element_type);
	free(expr->array_expr->element_type);
	break;
    }
    case CALL_EXPR: {
	free(expr->call_expr->func_name);
	for (u64 i = 0; i < expr->call_expr->num_args; ++i) destruct_expr(expr->call_expr->args + i);
	free(expr->call_expr->args);
	destruct_decorated_type(expr->call_expr->result_type);
	free(expr->call_expr->result_type);
	break;
    }
    case CAST_EXPR: {
	destruct_expr(expr->cast_expr->expr);
	free(expr->cast_expr->expr);
	destruct_decorated_type(expr->cast_expr->in_type);
	free(expr->cast_expr->in_type);
	destruct_decorated_type(expr->cast_expr->out_type);
	free(expr->cast_expr->out_type);
	break;
    }
    case LVALUE_EXPR: {
	destruct_lvalue(expr->lvalue_expr->lvalue);
	free(expr->lvalue_expr->lvalue);
	break;
    }
    case ASSIGN_EXPR: {
	destruct_lvalue(expr->assign_expr->lvalue);
	free(expr->assign_expr->lvalue);
	destruct_expr(expr->assign_expr->expr);
	free(expr->assign_expr->expr);
	destruct_decorated_type(expr->assign_expr->left_type);
	free(expr->assign_expr->left_type);
	destruct_decorated_type(expr->assign_expr->right_type);
	free(expr->assign_expr->right_type);
	break;
    }
    case ADDRESS_EXPR: {
	destruct_lvalue(expr->address_expr->lvalue);
	free(expr->address_expr->lvalue);
	break;
    }
    case CREMENT_EXPR: {
	destruct_lvalue(expr->crement_expr->lvalue);
	free(expr->crement_expr->lvalue);
	destruct_decorated_type(expr->crement_expr->type);
	free(expr->crement_expr->type);
	break;
    }
    case UNDEFINED: {
	break;
    }
    }
}

void destruct_stmt(statement *stmt) {
    switch (stmt->type) {
    case EXPR_STMT: {
	destruct_expr(stmt->expr_stmt->expr);
	free(stmt->expr_stmt->expr);
	break;
    }
    case IFELSE_STMT: {
	destruct_expr(stmt->ifelse_stmt->cond);
	free(stmt->ifelse_stmt->cond);
	destruct_stmt(stmt->ifelse_stmt->pos);
	free(stmt->ifelse_stmt->pos);
	destruct_stmt(stmt->ifelse_stmt->neg);
	free(stmt->ifelse_stmt->neg);
	break;
    }
    case DOWHILE_STMT: {
	destruct_expr(stmt->dowhile_stmt->cond);
	free(stmt->dowhile_stmt->cond);
	destruct_stmt(stmt->dowhile_stmt->body);
	free(stmt->dowhile_stmt->body);
	break;
    }
    case RETURN_STMT: {
	destruct_expr(stmt->return_stmt->expr);
	free(stmt->return_stmt->expr);
	break;
    }
    case BLOCK: {
	for (u64 i = 0; i < stmt->block_size; ++i) destruct_decl(stmt->block + i);
	free(stmt->block);
	break;
    }
    case EMPTY: {
	break;
    }
    }
}

void destruct_decl(declaration *decl) {
    switch (decl->type) {
    case STRUCT_DECL: {
	free(decl->struct_decl->mods);
	free(decl->struct_decl->name);
	for (u64 i = 0; i < decl->struct_decl->num_fields; ++i) destruct_decorated_identifier(decl->struct_decl->fields + i);
	free(decl->struct_decl->fields);
	break;
    }
    case FUNC_DECL: {
	free(decl->func_decl->mods);
	free(decl->func_decl->name);
	for (u64 i = 0; i < decl->func_decl->num_params; ++i) destruct_decorated_identifier(decl->func_decl->params + i);
	free(decl->func_decl->params);
	destruct_decorated_type(decl->func_decl->ret_type);
	free(decl->func_decl->ret_type);
	destruct_stmt(decl->func_decl->body);
	free(decl->func_decl->body);
	break;
    }
    case VAR_DECL: {
	destruct_decorated_identifier(decl->var_decl->iden);
	free(decl->var_decl->iden);
	destruct_expr(decl->var_decl->init);
	free(decl->var_decl->init);
	break;
    }
    case STMT_DECL: {
	destruct_stmt(decl->stmt_decl->stmt);
	free(decl->stmt_decl->stmt);
	break;
    }
    }
}

void destruct_sast(sast *sast) {
    for (u64 i = 0; i < sast->num_decls; ++i) destruct_decl(sast->decls + i);
    free(sast->decls);
}
