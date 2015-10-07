#ifndef __parse_lhs_h__
#define __parse_lhs_h__

#include <stdint.h>
#include "../str_ref.h"


typedef enum
{
	PARSE_LHS_VARIABLE,
	PARSE_LHS_ARRAY,
	PARSE_LHS_STAR_LEN,
	PARSE_LHS_MEMBER_TYPE,
	PARSE_LHS_MEMBER_STRUCTURE,
	PARSE_LHS_IMPLICIT_DO,
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
					parse_array_index_t* index;
				} array;

				struct
				{
					str_ref_t name;
				} member;

				struct
				{
					parse_expr_t* len;
					bool          var;
				} star_len;
			};
		};

		parse_implicit_do_t* implicit_do;
	};
};

typedef struct
{
	unsigned      count;
	parse_lhs_t** lhs;
} parse_lhs_list_t;

parse_lhs_t* parse_lhs_star_len(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len);
parse_lhs_t* parse_lhs(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len);
parse_lhs_t* parse_lhs_copy(
	parse_lhs_t* lhs);
void parse_lhs_delete(
	parse_lhs_t* lhs);

bool parse_lhs_print(
	 colstr_t* cs, const parse_lhs_t* lhs);

bool parse_lhs_base_name(
	const parse_lhs_t lhs,
	str_ref_t* name);

parse_lhs_list_t* parse_lhs_list(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len);
parse_lhs_list_t* parse_lhs_list_bracketed(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len);
void parse_lhs_list_delete(
	parse_lhs_list_t* list);
bool parse_lhs_list_print(
	colstr_t* cs, const parse_lhs_list_t* list);
bool parse_lhs_list_bracketed_print(
	colstr_t* cs, const parse_lhs_list_t* list);

#endif
