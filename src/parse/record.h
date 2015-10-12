#ifndef __ofc_parse_record_h__
#define __ofc_parse_record_h__

typedef struct
{
	ofc_str_ref_t     structure;
	ofc_parse_lhs_t*  name;
} ofc_parse_record_t;

typedef struct
{
	unsigned             count;
	ofc_parse_record_t** record;
} ofc_parse_record_list_t;

ofc_parse_record_list_t* ofc_parse_record_list(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
void ofc_parse_record_list_delete(
	ofc_parse_record_list_t* list);
bool ofc_parse_record_list_print(
	ofc_colstr_t* cs, const ofc_parse_record_list_t* list);

#endif
