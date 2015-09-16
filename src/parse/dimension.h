#ifndef __parse_dimension_h__
#define __parse_dimension_h__

unsigned parse_dimension(
	const sparse_t* src, const char* ptr,
	const parse_implicit_t* implicit,
	hashmap_t* decl_map);

#endif
