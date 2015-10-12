#ifndef __ofc_parse_implicit_h__
#define __ofc_parse_implicit_h__

typedef struct
{
	ofc_parse_type_t* type;
	uint32_t          mask; /* A..Z -> 0..25 */
} ofc_parse_implicit_t;

typedef struct
{
	unsigned               count;
	ofc_parse_implicit_t** rule;
} ofc_parse_implicit_list_t;


ofc_parse_implicit_t* ofc_parse_implicit(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
void ofc_parse_implicit_delete(
	ofc_parse_implicit_t* implicit);
bool ofc_parse_implicit_print(
	ofc_colstr_t* cs, const ofc_parse_implicit_t* implicit);

ofc_parse_implicit_list_t* ofc_parse_implicit_list(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
void ofc_parse_implicit_list_delete(
	ofc_parse_implicit_list_t* list);
bool ofc_parse_implicit_list_print(
	ofc_colstr_t* cs, const ofc_parse_implicit_list_t* list);

#endif
