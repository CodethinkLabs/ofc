#include "../parse.h"



unsigned parse_stmt_data(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned dpos = parse_debug_position(debug);

	unsigned i = parse_keyword(
		src, ptr, debug,
		PARSE_KEYWORD_DATA);
	if (i == 0) return 0;

	unsigned l;
	stmt->data = parse_data_list(
		src, &ptr[i], debug, &l);
	if (!stmt->data)
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += l;

	stmt->type = PARSE_STMT_DATA;
	return i;
}

bool parse_stmt_data_print(
	string_t* tree_output, const parse_stmt_t* stmt)
{
	return (stmt && string_printf(tree_output, "DATA ")
		&& parse_data_list_print(tree_output, stmt->data));
}
