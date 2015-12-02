#ifndef __ofc_parse_array_h__
#define __ofc_parse_array_h__

typedef struct
{
	ofc_str_ref_t src;

	bool              is_slice;
	ofc_parse_expr_t* first;
	ofc_parse_expr_t* last;
	ofc_parse_expr_t* stride;
} ofc_parse_array_range_t;

typedef struct
{
	unsigned                  count;
	ofc_parse_array_range_t** range;
} ofc_parse_array_index_t;


ofc_parse_array_index_t* ofc_parse_array_index(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);

ofc_parse_array_index_t* ofc_parse_array_index_copy(
	const ofc_parse_array_index_t* index);

void ofc_parse_array_index_delete(
	ofc_parse_array_index_t* index);

bool ofc_parse_array_index_print(
	ofc_colstr_t* cs, const ofc_parse_array_index_t* index,
	bool is_decl);


#endif
