#ifndef __parse_lhs_h__
#define __parse_lhs_h__

#include <stdint.h>
#include "../str_ref.h"


typedef enum
{
	PARSE_LHS_VARIABLE,
	PARSE_LHS_ARRAY,
	PARSE_LHS_MEMBER_TYPE,
	PARSE_LHS_MEMBER_STRUCTURE,
} parse_lhs_e;

typedef struct parse_lhs_s parse_lhs_t;

struct parse_lhs_s
{
	parse_lhs_e type;

	union
	{
		str_ref_t    variable;
		parse_lhs_t* parent;
	};
};


unsigned parse_lhs(
	const sparse_t* src, const char* ptr,
	parse_lhs_t* lhs);

void parse_lhs_cleanup(
	parse_lhs_t lhs);

#endif
