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
    case VOID: printf("Void");
    case BOOL: printf("Bool");
    case U8: printf("U8");
    case U16: printf("U16");
    case U32: printf("U32");
    case U64: printf("U64");
    case I8: printf("I8");
    case I16: printf("I16");
    case I32: printf("I32");
    case I64: printf("I64");
    case F32: printf("F32");
    case F64: printf("F64");
    case STRUCT: printf("StructType %s", t->struct_name);
    }
}
