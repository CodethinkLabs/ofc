#ifndef __ofc_parse_star_len_h__
#define __ofc_parse_star_len_h__

unsigned ofc_parse_star_len(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_expr_t** count, bool* is_variable);

#endif
