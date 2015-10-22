#ifndef __ofc_parse_lhs_h__
#define __ofc_parse_lhs_h__

#include <stdint.h>
#include "../str_ref.h"


typedef enum
{
	OFC_PARSE_LHS_VARIABLE,
	OFC_PARSE_LHS_ARRAY,
	OFC_PARSE_LHS_STAR_LEN,
	OFC_PARSE_LHS_MEMBER_TYPE,
	OFC_PARSE_LHS_MEMBER_STRUCTURE,
	OFC_PARSE_LHS_IMPLICIT_DO,
} ofc_parse_lhs_e;


struct ofc_parse_lhs_s
{
	ofc_parse_lhs_e type;

	union
	{
		ofc_str_ref_t variable;

		struct
		{
			ofc_parse_lhs_t* parent;

			union
			{
				struct
				{
					ofc_parse_array_index_t* index;
				} array;

				struct
				{
					ofc_str_ref_t name;
				} member;

				struct
				{
					ofc_parse_expr_t* len;
					bool              var;
				} star_len;
			};
		};

		ofc_parse_implicit_do_t* implicit_do;
	};
};

typedef struct
{
	unsigned      count;
	ofc_parse_lhs_t** lhs;
} ofc_parse_lhs_list_t;

ofc_parse_lhs_t* ofc_parse_lhs_star_len(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
ofc_parse_lhs_t* ofc_parse_lhs_variable(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
ofc_parse_lhs_t* ofc_parse_lhs(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
ofc_parse_lhs_t* ofc_parse_lhs_copy(
	ofc_parse_lhs_t* lhs);
void ofc_parse_lhs_delete(
	ofc_parse_lhs_t* lhs);

bool ofc_parse_lhs_print(
	ofc_colstr_t* cs, const ofc_parse_lhs_t* lhs,
	bool is_decl);

bool ofc_parse_lhs_base_name(
	const ofc_parse_lhs_t lhs,
	ofc_str_ref_t* name);

ofc_parse_lhs_list_t* ofc_parse_lhs_list(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
ofc_parse_lhs_list_t* ofc_parse_lhs_list_bracketed(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
void ofc_parse_lhs_list_delete(
	ofc_parse_lhs_list_t* list);
bool ofc_parse_lhs_list_print(
	ofc_colstr_t* cs, const ofc_parse_lhs_list_t* list,
	bool is_decl);
bool ofc_parse_lhs_list_bracketed_print(
	ofc_colstr_t* cs, const ofc_parse_lhs_list_t* list,
	bool is_decl);

#endif
