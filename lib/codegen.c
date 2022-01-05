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
    case STRUCT: printf("(StructType %s)", t->struct_name); break;
    default: printf("(INVALID ENUM CODE (print_type))");
    }
}

void print_decorated_type(decorated_type *dt) {
    switch (dt->decorated_type_e) {
    case PURE_TYPE: printf("(PureType "); print_type(dt->pure_type); printf(")"); break;
    case DEREF_TYPE: printf("(DerefType "); print_decorated_type(dt->deref_type); printf(")"); break;
    case ARRAY_TYPE: printf("(ArrayType "); print_decorated_type(dt->array_type); printf(" %lu)", dt->array_size); break;
    default: printf("(INVALID ENUM CODE (print_decorated_type))");
    }
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
}

void print_decorated_identifier(decorated_identifier *dec_iden) {
    printf("DecoratedIdentifier [");
    for (u64 i = 0; i < dec_iden->num_mods; i++) {
	if (i) printf(",");
	print_modifier(dec_iden->mods[i]);
    }
    printf("] %s ", dec_iden->name);
    print_decorated_type(dec_iden->type);
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
}

void print_unary_op(unary_op uop) {
    switch (uop) {
    case PRE_PLUS_PLUS: printf("PrePlusPlus"); break;
    case PRE_MINUS_MINUS: printf("PreMinusMinus"); break;
    case POST_PLUS_PLUS: printf("PostPlusPlus"); break;
    case POST_MINUS_MINUS: printf("PostMinusMinus"); break;
    case PLUS: printf("Plus"); break;
    case MINUS: printf("Minus"); break;
    case EXCLA: printf("Excla"); break;
    case TILDA: printf("Tilda"); break;
    case CAST: printf("Cast"); break;
    default: printf("(INVALID ENUM CODE (print_unary_op))");
    }
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
	for (u64 i = 0; i < cv->num_fields; cv++) {
	    if (i) printf(",");
	    print_comptime_value(cv->fields + i);
	}
	printf("] %s)", cv->struct_name);
	break;
    }
    case CT_ARR: {
	printf("(ComptimeArr [");
	for (u64 i = 0; i < cv->size; cv++) {
	    if (i) printf(",");
	    print_comptime_value(cv->elements + i);
	}
	printf("] %lu)", cv->size);
	break;
    }
    default: printf("(INVALID ENUM CODE (print_comptime_value))");
    }
}

void print_lvalue(lvalue *lv) {
    switch (lv->type) {
    case DEREF: printf("(Dereference "); print_expression(lv->dereferenced); printf(")"); break;
    case ACCESS: printf("(Access "); print_lvalue(lv->accessed); printf(" %lu ", lv->offset); print_decorated_type(lv->access_result_type); printf(")"); break;
    case INDEX: printf("(Index "); print_lvalue(lv->indexed); printf(" "); print_expression(lv->index); printf(" "); print_decorated_type(lv->index_result_type); printf(")"); break;
    case IDENTIFIER: printf("(Identifier %s ", lv->name); print_decorated_type(lv->iden_type); printf(")"); break;
    default: printf("(INVALID ENUM CODE (print_lvalue))");
    }
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
}

void print_expression(expression *expr) {

}
