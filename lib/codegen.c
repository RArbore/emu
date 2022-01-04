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
    for (uint64_t i = 0; i < dec_iden->num_mods; i++) {
	if (i) printf(",");
	print_modifier(dec_iden->mods[i]);
    }
    printf("] %s ", dec_iden->name);
    print_decorated_type(dec_iden->type);
}
