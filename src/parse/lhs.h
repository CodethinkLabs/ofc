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


struct parse_lhs_s
{
	parse_lhs_e type;

	union
	{
		str_ref_t variable;

		struct
		{
			parse_lhs_t* parent;

			union
			{
				struct
				{
					parse_expr_t* index;
				} array;

				struct
				{
					str_ref_t name;
				} member;
			};
		};
	};
};


unsigned parse_lhs(
	const sparse_t* src, const char* ptr,
	parse_lhs_t* lhs);

void parse_lhs_cleanup(
	parse_lhs_t lhs);

bool parse_lhs_base_name(
	const parse_lhs_t lhs,
	str_ref_t* name);

bool parse_lhs_clone(
	parse_lhs_t* dst, const parse_lhs_t* src);


parse_lhs_t* parse_lhs_alloc(
	parse_lhs_t lhs);
void parse_lhs_delete(
	parse_lhs_t* lhs);

#endif
