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

#ifndef LIB_H
#define LIB_H

#ifdef __cplusplus
extern "C"
{
#endif

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;
typedef float f32;
typedef double f64;

typedef enum type_e {
    VOID = 0,
    BOOL,
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    STRUCT,
} type_e;

typedef struct type {
    type_e type_e;
    char *struct_name;
} type;

void print_type(type*);

typedef enum decorated_type_e {
    PURE_TYPE = 0,
    DEREF_TYPE,
    ARRAY_TYPE,
} decorated_type_e;

typedef struct decorated_type {
    decorated_type_e decorated_type_e;
    union {
	type *pure_type;
	struct decorated_type *deref_type;
	struct {
	    struct decorated_type *array_type;
	    u64 array_size;
	};
    };
} decorated_type;

void print_decorated_type(decorated_type*);

typedef enum modifier {
    PURE = 0,
    CONST,
    INLINE,
    REGISTER,
    RESTRICT,
} modifier;

void print_modifier(modifier);

typedef struct decorated_identifier {
    modifier *mods;
    u64 num_mods;
    char *name;
    decorated_type *type;
} decorated_identifier;

void print_decorated_identifier(decorated_identifier*);

struct expression;

typedef enum binary_op {
    LOGIC_OR = 0,
    LOGIC_XOR,
    LOGIC_AND,
    BITWISE_OR,
    BITWISE_XOR,
    BITWISE_AND,
    EQUALS_EQUALS,
    EXCLA_EQUALS,
    GREATER,
    LESSER,
    GREATER_EQUALS,
    LESSER_EQUALS,
    LSHIFT,
    RSHIFT,
    TERM_PLUS,
    TERM_MINUS,
    FACTOR_STAR,
    FACTOR_SLASH,
    FACTOR_PERCENT,
} binary_op;

void print_binary_op(binary_op);

typedef struct binary_expr {
    binary_op op;
    struct expression *expr1;
    struct expression *expr2;
    decorated_type *type;
    decorated_type *left_type;
    decorated_type *right_type;
} binary_expr;

typedef enum unary_op {
    PLUS = 0,
    MINUS,
    EXCLA,
    TILDA,
} unary_op;

void print_unary_op(unary_op);

typedef struct unary_expr {
    unary_op op;
    struct expression *expr;
    decorated_type *type;
} unary_expr;

typedef enum comptime_type {
    CT_PTR = 0,
    CT_BOOL,
    CT_U8,
    CT_U16,
    CT_U32,
    CT_U64,
    CT_I8,
    CT_I16,
    CT_I32,
    CT_I64,
    CT_F32,
    CT_F64,
    CT_STRUCT,
    CT_ARR,
} comptime_type;

typedef struct comptime_value {
    comptime_type type;
    union {
	struct {
	    u64 comptime_ptr;
	    decorated_type *ptr_type;
	};
	bool comptime_bool;
	u8 comptime_u8;
	u16 comptime_u16;
	u32 comptime_u32;
	u64 comptime_u64;
	i8 comptime_i8;
	i16 comptime_i16;
	i32 comptime_i32;
	i64 comptime_i64;
	f32 comptime_f32;
	f64 comptime_f64;
	struct {
	    struct comptime_value *fields;
	    u64 num_fields;
	    char *struct_name;
	};
	struct {
	    struct comptime_value *elements;
	    u64 size;
	    decorated_type *array_type;
	};
    };
} comptime_value;

void print_comptime_value(comptime_value*);

typedef struct literal_expr {
    comptime_value *comptime_value;
} literal_expr;

typedef struct array_expr {
    struct expression *elements;
    decorated_type *element_type;
    u64 size;
} array_expr;

typedef struct call_expr {
    char *func_name;
    struct expression *args;
    u64 num_args;
    decorated_type *result_type;
} call_expr;

typedef struct cast_expr {
    struct expression *expr;
    decorated_type *in_type;
    decorated_type *out_type;
} cast_expr;

typedef enum lvalue_type {
    DEREF = 0,
    ACCESS,
    INDEX,
    IDENTIFIER,
} lvalue_type;

typedef struct lvalue {
    lvalue_type type;
    union {
	struct expression *dereferenced;
	struct {
	    struct lvalue *accessed;
	    u64 offset;
	};
	struct {
	    struct lvalue *indexed;
	    struct expression *index;
	};
	char *name;
    };
    decorated_type *decorated_type;
} lvalue;

void print_lvalue(lvalue*);

typedef struct lvalue_expr {
    lvalue *lvalue;
} lvalue_expr;

typedef enum assign_op {
    EQUALS = 0,
    PLUS_EQUALS,
    MINUS_EQUALS,
    STAR_EQUALS,
    SLASH_EQUALS,
    PERCENT_EQUALS,
    LSHIFT_EQUALS,
    RSHIFT_EQUALS,
    HAT_EQUALS,
    BAR_EQUALS,
    AND_EQUALS,
} assign_op;

void print_assign_op(assign_op);

typedef struct assign_expr {
    assign_op op;
    lvalue *lvalue;
    struct expression *expr;
    decorated_type *left_type;
    decorated_type *right_type;
} assign_expr;

typedef struct address_expr {
    lvalue *lvalue;
} address_expr;

typedef enum crement_op {
    PRE_PLUS_PLUS = 0,
    PRE_MINUS_MINUS,
    POST_PLUS_PLUS,
    POST_MINUS_MINUS,
} crement_op;

void print_crement_op(crement_op);

typedef struct crement_expr {
    crement_op op;
    lvalue *lvalue;
    decorated_type *type;
} crement_expr;

typedef enum expression_type {
    BINARY_EXPR = 0,
    UNARY_EXPR,
    LITERAL_EXPR,
    ARRAY_EXPR,
    CALL_EXPR,
    CAST_EXPR,
    LVALUE_EXPR,
    ASSIGN_EXPR,
    ADDRESS_EXPR,
    CREMENT_EXPR,
    UNDEFINED,
} expression_type;

typedef struct expression {
    expression_type type;
    union {
	binary_expr *binary_expr;
	unary_expr *unary_expr;
	literal_expr *literal_expr;
	array_expr *array_expr;
	call_expr *call_expr;
	cast_expr *cast_expr;
	lvalue_expr *lvalue_expr;
	assign_expr *assign_expr;
	address_expr *address_expr;
	crement_expr *crement_expr;
    };
} expression;

void print_expression(expression*);

typedef struct expr_stmt {
    expression *expr;
} expr_stmt;

struct statement;

typedef struct ifelse_stmt {
    expression *cond;
    struct statement *pos;
    struct statement *neg;
    bool pos_terms;
    bool neg_terms;
} ifelse_stmt;

typedef struct dowhile_stmt {
    expression *cond;
    struct statement *body;
    bool terms;
} dowhile_stmt;

typedef struct return_stmt {
    expression *expr;
} return_stmt;

typedef enum statement_type {
    EXPR_STMT = 0,
    IFELSE_STMT,
    DOWHILE_STMT,
    RETURN_STMT,
    BLOCK,
    EMPTY,
} statement_type;

struct declaration;

typedef struct statement {
    statement_type type;
    union {
	expr_stmt *expr_stmt;
	ifelse_stmt *ifelse_stmt;
	dowhile_stmt *dowhile_stmt;
	return_stmt *return_stmt;
	struct {
	    struct declaration *block;
	    u64 block_size;
	};
    };
} statement;

void print_statement(statement*);

typedef struct struct_decl {
    modifier *mods;
    u64 num_mods;
    char *name;
    decorated_identifier *fields;
    u64 num_fields;
} struct_decl;

typedef struct func_decl {
    modifier *mods;
    u64 num_mods;
    char *name;
    decorated_identifier *params;
    u64 num_params;
    decorated_type *ret_type;
    statement *body;
} func_decl;

typedef struct var_decl {
    decorated_identifier *iden;
    expression *init;
} var_decl;

typedef struct stmt_decl {
    statement *stmt;
} stmt_decl;

typedef enum declaration_type {
    STRUCT_DECL = 0,
    FUNC_DECL,
    VAR_DECL,
    STMT_DECL,
} declaration_type;

typedef struct declaration {
    declaration_type type;
    union {
	struct_decl *struct_decl;
	func_decl *func_decl;
	var_decl *var_decl;
	stmt_decl *stmt_decl;
    };
} declaration;

void print_declaration(declaration*);

typedef struct sast {
    declaration *decls;
    u64 num_decls;
} sast;

void print_sast(sast*);

void free_sast(sast*);

int c_entry_point(sast*);
int cxx_entry_point(sast*);

#ifdef __cplusplus
}
#endif

#endif
