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
	colstr_t* cs, const parse_stmt_t* stmt)
{
	return (stmt && colstr_atomic_writef(cs, "DATA ")
		&& parse_data_list_print(cs, stmt->data));
}
