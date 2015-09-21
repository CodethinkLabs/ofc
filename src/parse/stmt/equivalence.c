#include "../parse.h"


unsigned parse_stmt_equivalence(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, PARSE_KEYWORD_EQUIVALENCE);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_EQUIVALENCE;
	stmt->equivalence.count = 0;
	stmt->equivalence.group = NULL;

	unsigned len = parse_list(src, ptr,
		&stmt->equivalence.count,
		(void***)&stmt->equivalence.group,
		(void*)parse_lhs_list_bracketed,
		(void*)parse_lhs_list_delete);
	if (len == 0) return 0;
	i += len;

	return i;
}
