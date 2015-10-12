#ifndef __ofc_parse_file_h__
#define __ofc_parse_file_h__

#include "parse.h"

ofc_parse_stmt_list_t* ofc_parse_file(const ofc_sparse_t* src);

bool ofc_parse_file_print(
	ofc_colstr_t* cs,
	const ofc_parse_stmt_list_t* list);

#endif
