#include "../parse.h"


unsigned parse_stmt_save(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned dpos = parse_debug_position(debug);

	unsigned i = parse_keyword(
		src, ptr, debug,
		PARSE_KEYWORD_SAVE);
	if (i == 0) return 0;

	unsigned l;
	stmt->save.list = parse_save_list(
		src, &ptr[i], debug, &l);
	if (!stmt->save.list)
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += l;

	stmt->type = PARSE_STMT_SAVE;
	return i;
}

bool parse_stmt_save_print(
	string_t* tree_output, const parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	return (string_printf(tree_output, "SAVE ")
		&& parse_save_list_print(
			tree_output, stmt->save.list));
}
