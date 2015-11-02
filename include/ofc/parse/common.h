#ifndef __ofc_parse_common_h__
#define __ofc_parse_common_h__

typedef struct
{
	ofc_str_ref_t         group; /* May be empty. */
	ofc_parse_lhs_list_t* names;
} ofc_parse_common_group_t;

typedef struct
{
	unsigned                   count;
	ofc_parse_common_group_t** group;
} ofc_parse_common_group_list_t;

ofc_parse_common_group_t* ofc_parse_common_group(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
void ofc_parse_common_group_delete(
	ofc_parse_common_group_t* group);
bool ofc_parse_common_group_print(
	ofc_colstr_t* cs, const ofc_parse_common_group_t* group);

ofc_parse_common_group_list_t* ofc_parse_common_group_list(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
void ofc_parse_common_group_list_delete(
	ofc_parse_common_group_list_t* list);
bool ofc_parse_common_group_list_print(
	ofc_colstr_t* cs, const ofc_parse_common_group_list_t* list);

#endif
