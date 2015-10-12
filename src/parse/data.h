#ifndef __ofc_parse_data_h__
#define __ofc_parse_data_h__

typedef struct
{
	unsigned          repeat;
	ofc_parse_expr_t* expr;
} ofc_parse_clist_entry_t;

typedef struct
{
	unsigned                  count;
	ofc_parse_clist_entry_t** entry;
} ofc_parse_clist_t;

typedef struct
{
	ofc_parse_lhs_list_t* nlist;
	ofc_parse_clist_t*    clist;
} ofc_parse_data_entry_t;

typedef struct
{
	unsigned                 count;
	ofc_parse_data_entry_t** entry;
} ofc_parse_data_list_t;

ofc_parse_clist_t* ofc_parse_clist(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
void ofc_parse_clist_delete(
	ofc_parse_clist_t* clist);
bool ofc_parse_clist_print(
	ofc_colstr_t* cs, const ofc_parse_clist_t* list);

ofc_parse_data_list_t* ofc_parse_data_list(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
void ofc_parse_data_list_delete(
	ofc_parse_data_list_t* list);
bool ofc_parse_data_list_print(
	ofc_colstr_t* cs, const ofc_parse_data_list_t* list);

#endif
