#include <sys/types.h>
#include <stdio.h>

typedef enum modifiers {
    PURE,
    CONST,
    INLINE,
    REGISTER,
    RESTRICT,
} modifiers;

typedef enum declaration_type {
    STRUCT_DECL,
    FUNC_DECL,
    VAR_DECL,
    STMT_DECL,
} declaration_type;

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

typedef struct decorated_identifier {
    modifiers *mods;
    char *name;
    decorated_type *type;
} decorated_identifier;

typedef struct struct_decl {
    modifiers *mods;
    char *name;
    struct decorated_identifier *fields;
} struct_decl;

typedef struct func_decl {
    modifiers *mods;
    char *name;
    struct decorated_identifier *params;
    struct decorated_type *ret_type;
    struct statement *body;
} func_decl;

typedef struct var_decl {
    struct decorated_identifier *iden;
    struct expression *init;
} var_decl;

typedef struct stmt_decl {
    struct statement *stmt;
} stmt_decl;

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
