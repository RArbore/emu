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

struct declaration;

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
