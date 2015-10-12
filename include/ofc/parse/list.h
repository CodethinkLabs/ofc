#ifndef __ofc_parse_list_h__
#define __ofc_parse_list_h__

#include "../string.h"

unsigned ofc_parse_list(
	const ofc_sparse_t* sparse, const char* ptr,
	ofc_parse_debug_t* debug,
	char seperator, unsigned* elem_count, void*** elem,
	void* (*elem_parse)(const ofc_sparse_t*, const char*, ofc_parse_debug_t*, unsigned*),
	void (*elem_delete)(void*));

unsigned ofc_parse_list_seperator_optional(
	const ofc_sparse_t* sparse, const char* ptr,
	ofc_parse_debug_t* debug,
	char seperator, unsigned* elem_count, void*** elem,
	void* (*elem_parse)(const ofc_sparse_t*, const char*, ofc_parse_debug_t*, unsigned*),
	void (*elem_delete)(void*));

bool ofc_parse_list_copy(
	unsigned* dst_count, void*** dst,
	unsigned  src_count, const void** src,
	void* (*elem_copy)(const void*),
	void (*elem_delete)(void*));

void ofc_parse_list_delete(
	unsigned elem_count, void** elem,
	void (*elem_delete)(void*));

bool ofc_parse_list_print(
	ofc_colstr_t* cs,
	unsigned elem_count, const void** elem,
	bool (*elem_print)(ofc_colstr_t*, const void*));

#endif
