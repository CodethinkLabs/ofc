#include "../parse.h"


unsigned parse_stmt_pointer(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned dpos = parse_debug_position(debug);

	unsigned i = parse_keyword(
		src, ptr, debug, PARSE_KEYWORD_POINTER);
	if (i == 0) return 0;

	unsigned l;
	stmt->pointer = parse_pointer_list(
		src, &ptr[i], debug, &l);
	if (!stmt->pointer)
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += l;

	stmt->type = PARSE_STMT_POINTER;
	return i;
}

bool parse_stmt_pointer_print(
	string_t* tree_output, const parse_stmt_t* stmt)
{
	return (stmt && string_printf(tree_output, "POINTER ")
		&& parse_pointer_list_print(
			tree_output, stmt->pointer));
}
