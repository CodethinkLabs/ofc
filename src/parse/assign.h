#ifndef __ofc_parse_assign_h__
#define __ofc_parse_assign_h__

typedef struct
{
	ofc_parse_lhs_t*  name;
	ofc_parse_expr_t* init;
} ofc_parse_assign_t;

typedef struct
{
	unsigned             count;
	ofc_parse_assign_t** assign;
} ofc_parse_assign_list_t;


ofc_parse_assign_t* ofc_parse_assign(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);

ofc_parse_assign_t* ofc_parse_assign_init(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);

ofc_parse_assign_t* ofc_parse_assign_copy(
	const ofc_parse_assign_t* assign);

void ofc_parse_assign_delete(
	ofc_parse_assign_t* assign);

bool ofc_parse_assign_print(
	ofc_colstr_t* cs, const ofc_parse_assign_t* assign);


ofc_parse_assign_list_t* ofc_parse_assign_list(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);

ofc_parse_assign_list_t* ofc_parse_assign_list_copy(
	const ofc_parse_assign_list_t* list);

void ofc_parse_assign_list_delete(
	ofc_parse_assign_list_t* list);

bool ofc_parse_assign_list_print(
ofc_colstr_t* cs, const ofc_parse_assign_list_t* list);


#endif
