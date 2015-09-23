#ifndef __parse_type_h__
#define __parse_type_h__

#include "expr.h"

typedef enum
{
	PARSE_TYPE_NONE = 0,
	PARSE_TYPE_LOGICAL,
	PARSE_TYPE_CHARACTER,
	PARSE_TYPE_INTEGER,
	PARSE_TYPE_REAL,
	PARSE_TYPE_DOUBLE_PRECISION,
	PARSE_TYPE_COMPLEX,
	PARSE_TYPE_DOUBLE_COMPLEX,
	PARSE_TYPE_BYTE,

	PARSE_TYPE_COUNT,
} parse_type_e;

typedef struct
{
	bool is_static;
	bool is_volatile;
	bool is_automatic;
} parse_decl_attr_t;

typedef struct
{
	parse_type_e           type;
	parse_decl_attr_t      attr;
	unsigned               kind;
	parse_expr_t*          count_expr;
	bool                   count_var;
	parse_call_arg_list_t* params;
} parse_type_t;



parse_type_t* parse_type(
	const sparse_t* src, const char* ptr,
	unsigned* len);
void parse_type_delete(parse_type_t* type);
bool parse_type_print(int fd, const parse_type_t* type);

#endif
