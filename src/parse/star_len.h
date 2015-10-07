#ifndef __star_len_h__
#define __star_len_h__

unsigned parse_star_len(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_expr_t** count, bool* is_variable);

#endif
