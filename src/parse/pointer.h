#ifndef __ofc_parse_pointer_h__
#define __ofc_parse_pointer_h__

typedef struct
{
	ofc_str_ref_t name;
	ofc_str_ref_t target;
} ofc_parse_pointer_t;

typedef struct
{
	unsigned              count;
	ofc_parse_pointer_t** pointer;
} ofc_parse_pointer_list_t;

ofc_parse_pointer_list_t* ofc_parse_pointer_list(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
void ofc_parse_pointer_list_delete(
	ofc_parse_pointer_list_t* list);
bool ofc_parse_pointer_list_print(
	ofc_colstr_t* cs, const ofc_parse_pointer_list_t* list);

#endif
