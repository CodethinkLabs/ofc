#include "../parse.h"


unsigned parse_stmt_assign(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned dpos = parse_debug_position(debug);

	unsigned i = parse_keyword(
		src, ptr, debug, PARSE_KEYWORD_ASSIGN);
	if (i == 0) return 0;

	unsigned len = parse_unsigned(
		src, &ptr[i], debug, &stmt->assign.label);
	if (len == 0)
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	len = parse_keyword(
		src, &ptr[i], debug, PARSE_KEYWORD_TO);
	if (len == 0)
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += 2;

	len = parse_name(
		src, &ptr[i], debug,
		&stmt->assign.variable);
	if (len == 0)
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	stmt->type = PARSE_STMT_ASSIGN;
	return i;
}

bool parse_stmt_assign_print(
	string_t* tree_output, const parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	return (string_printf(tree_output, "ASSIGN %u TO ", stmt->assign.label)
		&& str_ref_print(tree_output, stmt->assign.variable));
}
