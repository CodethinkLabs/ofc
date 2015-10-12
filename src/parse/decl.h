#ifndef __ofc_parse_decl_h__
#define __ofc_parse_decl_h__

typedef struct
{
	ofc_parse_lhs_t*   lhs;

	ofc_parse_expr_t*  init_expr;
	ofc_parse_clist_t* init_clist;
} ofc_parse_decl_t;

typedef struct
{
	unsigned           count;
	ofc_parse_decl_t** decl;
} ofc_parse_decl_list_t;


ofc_parse_decl_t* ofc_parse_decl(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
void ofc_parse_decl_delete(
	ofc_parse_decl_t* decl);
bool ofc_parse_decl_print(
	ofc_colstr_t* cs, const ofc_parse_decl_t* decl);

ofc_parse_decl_list_t* ofc_parse_decl_list(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
void ofc_parse_decl_list_delete(
	ofc_parse_decl_list_t* decl);
bool ofc_parse_decl_list_print(
	ofc_colstr_t* cs, const ofc_parse_decl_list_t* list);

#endif
