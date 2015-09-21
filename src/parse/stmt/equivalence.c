#include "../parse.h"


unsigned parse_stmt_equivalence(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, PARSE_KEYWORD_EQUIVALENCE);
	if (i == 0) return 0;

	if (ptr[i++] !='(')
		return 0;

	stmt->type = PARSE_STMT_EQUIVALENCE;
	stmt->equivalence.count = 0;
	stmt->equivalence.name = NULL;

	unsigned len = parse_list(src, ptr,
		&stmt->equivalence.count,
		(void***)&stmt->equivalence.name,
		(void*)parse_lhs,
		(void*)parse_lhs_delete);
	if (len == 0) return 0;
	i += len;

	if (ptr[i++] != ')')
	{
		parse_list_delete(src, ptr,
			stmt->equivalence.count,
			(void**)stmt->equivalence.name,
			(void*)parse_lhs_delete);
		return 0;
	}

	/* TODO - Parse lists of equivalence groups. */

	return i;
}
