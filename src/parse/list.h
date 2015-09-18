#ifndef __parse_list_h__
#define __parse_list_h__

unsigned parse_list(
	const sparse_t* sparse, const char* ptr,
	unsigned* elem_count, void*** elem,
	void* (*elem_parse)(const sparse_t*, const char*, unsigned*),
	void (*elem_delete)(void*));

void parse_list_delete(
	unsigned elem_count, void** elem,
	void (*elem_delete)(void*));

#endif
