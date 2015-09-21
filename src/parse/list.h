#ifndef __parse_list_h__
#define __parse_list_h__

unsigned parse_list(
	const sparse_t* sparse, const char* ptr, char seperator,
	unsigned* elem_count, void*** elem,
	void* (*elem_parse)(const sparse_t*, const char*, unsigned*),
	void (*elem_delete)(void*));

bool parse_list_copy(
	unsigned* dst_count, void*** dst,
	unsigned  src_count, const void** src,
	void* (*elem_copy)(const void*),
	void (*elem_delete)(void*));

void parse_list_delete(
	unsigned elem_count, void** elem,
	void (*elem_delete)(void*));

#endif
