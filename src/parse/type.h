#ifndef __parse_type_h__
#define __parse_type_h__

#include "expr.h"

typedef enum
{
	PARSE_TYPE_NONE,
	PARSE_TYPE_LOGICAL,
	PARSE_TYPE_CHARACTER,
	PARSE_TYPE_INTEGER,
	PARSE_TYPE_REAL,
	PARSE_TYPE_COMPLEX,
	PARSE_TYPE_BYTE,
} parse_type_e;

typedef struct
{
	parse_type_e  type;
	unsigned      kind;
	parse_expr_t* count_expr;
} parse_type_t;


#define PARSE_TYPE_INTEGER_DEFAULT (parse_type_t)\
{\
	.type  = PARSE_TYPE_INTEGER,\
	.kind  = 0,\
	.count_expr = NULL,\
}

#define PARSE_TYPE_REAL_DEFAULT (parse_type_t)\
{\
	.type  = PARSE_TYPE_REAL,\
	.kind  = 0,\
	.count_expr = NULL,\
}


unsigned parse_type(
	const sparse_t* src, const char* ptr,
	parse_type_t* type);

void parse_type_cleanup(
	parse_type_t type);

bool parse_type_clone(
	parse_type_t* dst, const parse_type_t* src);

parse_type_t* parse_type_alloc(parse_type_t type);
parse_type_t* parse_type_copy(const parse_type_t* type);
void parse_type_delete(parse_type_t* type);

#endif
