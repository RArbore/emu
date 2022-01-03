#ifndef CODEGEN_H
#define CODEGEN_H

#include <sys/types.h>
#include <stdbool.h>
#include <stdio.h>

typedef enum type_e {
    VOID,
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

typedef enum decorated_type_e {
    PURE_TYPE,
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
	    __uint64_t array_size;
	};
    };
} decorated_type;

typedef enum modifiers {
    PURE,
    CONST,
    INLINE,
    REGISTER,
    RESTRICT,
} modifiers;

typedef struct decorated_identifier {
    modifiers *mods;
    char *name;
    decorated_type *type;
} decorated_identifier;

typedef enum expression_type {
    BINARY_EXPR,
    UNARY_EXPR,
    LITERAL_EXPR,
    ARRAY_EXPR,
    CALL_EXPR,
    LVALUE_EXPR,
    ASSIGN_EXPR,
    ADDRESS_EXPR,
    UNDEFINED,
} expression_type;

struct expression;

typedef enum binary_op {
    LOGIC_OR,
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

typedef struct binary_expr {
    binary_op op;
    struct expression *expr1;
    struct expression *expr2;
    decorated_type *type;
} binary_expr;

typedef enum unary_op {
    PRE_PLUS_PLUS,
    PRE_MINUS_MINUS,
    POST_PLUS_PLUS,
    POST_MINUS_MINUS,
    PLUS,
    MINUS,
    EXCLA,
    TILDA,
    CAST,
} unary_op;

typedef struct unary_expr {
    unary_op op;
    struct expression *expr;
    decorated_type *type;
} unary_expr;

typedef enum comptime_type {
    CT_PTR,
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
	__uint64_t comptime_ptr;
	bool comptime_bool;
	__uint8_t comptime_u8;
	__uint16_t comptime_u16;
	__uint32_t comptime_u32;
	__uint64_t comptime_u64;
	__int8_t comptime_i8;
	__int16_t comptime_i16;
	__int32_t comptime_i32;
	__int64_t comptime_i64;
	struct {
	    struct comptime_value *fields;
	    char *struct_name;
	};
	struct {
	    struct comptime_value *elements;
	    __uint64_t size;
	};
    };
} comptime_value;

typedef struct literal_expr {
    comptime_value *comptime_value;
} literal_expr;

typedef struct array_expr {
    struct expression *elements;
    __uint64_t size;
} array_expr;

typedef struct expression {
    expression_type type;
    union {
	binary_expr binary_expr;
	unary_expr unary_expr;
	literal_expr literal_expr;
	array_expr array_expr;
	call_expr call_expr;
	lvalue_expr lvalue_expr;
	assign_expr assign_expr;
	address_expr address_expr;
    };
} expression;

typedef struct expr_stmt {
    expression *expr;
} expr_stmt;

struct statement;

typedef struct ifelse_stmt {
    expression *cond;
    struct statement *pos;
    struct statement *neg;
} ifelse_stmt;

typedef struct dowhile_stmt {
    expression *cond;
    struct statement *body;
} dowhile_stmt;

typedef struct return_stmt {
    expression *expr;
} return_stmt;

typedef enum statement_type {
    EXPR_STMT,
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
	expr_stmt expr_stmt;
	ifelse_stmt ifelse_stmt;
	dowhile_stmt dowhile_stmt;
	return_stmt return_stmt;
	struct {
	    struct declaration *block;
	    __uint64_t block_size;
	};
    };
} statement;

typedef struct struct_decl {
    modifiers *mods;
    char *name;
    decorated_identifier *fields;
} struct_decl;

typedef struct func_decl {
    modifiers *mods;
    char *name;
    decorated_identifier *params;
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
    STRUCT_DECL,
    FUNC_DECL,
    VAR_DECL,
    STMT_DECL,
} declaration_type;

typedef struct declaration {
    declaration_type type;
    union {
	struct_decl struct_decl;
	func_decl func_decl;
	var_decl var_decl;
	stmt_decl stmt_decl;
    };
} declaration;

void test(void);

#endif
