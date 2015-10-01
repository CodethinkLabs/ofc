#include "../parse.h"


unsigned parse_stmt_equivalence(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned dpos = parse_debug_position(debug);

	unsigned i = parse_keyword(
		src, ptr, debug, PARSE_KEYWORD_EQUIVALENCE);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_EQUIVALENCE;
	stmt->equivalence.count = 0;
	stmt->equivalence.group = NULL;

	unsigned len = parse_list(
		src, &ptr[i], debug, ',',
		&stmt->equivalence.count,
		(void***)&stmt->equivalence.group,
		(void*)parse_lhs_list_bracketed,
		(void*)parse_lhs_list_delete);
	if (len == 0)
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	return i;
}

bool parse_stmt_equivalence_print(
	string_t* tree_output, const parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	return (string_printf(tree_output, "EQUIVALENCE ")
		&& parse_list_print(tree_output,
			stmt->equivalence.count,
			(const void**)stmt->equivalence.group,
			(void*)parse_lhs_list_bracketed_print));
}
