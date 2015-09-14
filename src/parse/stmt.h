#ifndef __parse_stmt_h__
#define __parse_stmt_h__

typedef union
{
	struct
	{
		str_ref_t    lhs;
		parse_expr_t rhs;
	} assign;
} parse_stmt_t;

#include "stmt/assign.h"

unsigned parse_stmt(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);

#endif
