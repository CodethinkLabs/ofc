#ifndef __ofc_parse_save_h__
#define __ofc_parse_save_h__

typedef struct
{
	bool is_common;
	union
	{
		ofc_parse_lhs_t* lhs;
		ofc_str_ref_t    common;
	};
} ofc_parse_save_t;

typedef struct
{
	unsigned           count;
	ofc_parse_save_t** save;
} ofc_parse_save_list_t;


ofc_parse_save_t* ofc_parse_save(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
void ofc_parse_save_delete(
	ofc_parse_save_t* save);
bool ofc_parse_save_print(
	ofc_colstr_t* cs, const ofc_parse_save_t* save);

ofc_parse_save_list_t* ofc_parse_save_list(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
void ofc_parse_save_list_delete(
	ofc_parse_save_list_t* save);
bool ofc_parse_save_list_print(
	ofc_colstr_t* cs, const ofc_parse_save_list_t* list);

#endif
