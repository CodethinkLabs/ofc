#include "../parse.h"


static unsigned parse_stmt__dimension_virtual(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_keyword_e keyword,
	parse_stmt_t* stmt)
{
	unsigned dpos = parse_debug_position(debug);

	unsigned i = parse_keyword(
		src, ptr, debug, keyword);
	if (i == 0) return 0;

	unsigned len;
	stmt->dimension = parse_lhs_list(
		src, &ptr[i], debug, &len);
	if (!stmt->dimension)
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	stmt->type = PARSE_STMT_DIMENSION;
	return i;
}

unsigned parse_stmt_dimension(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	return parse_stmt__dimension_virtual(
		src, ptr, debug, PARSE_KEYWORD_DIMENSION, stmt);
}

/* http://docs.oracle.com/cd/E19957-01/805-4939/6j4m0vnbo/index.html */
unsigned parse_stmt_virtual(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	return parse_stmt__dimension_virtual(
		src, ptr, debug, PARSE_KEYWORD_VIRTUAL, stmt);
}

bool parse_stmt_dimension_print(
	colstr_t* cs, const parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	return (colstr_atomic_writef(cs, "DIMENSION ")
		&& parse_lhs_list_print(cs, stmt->dimension, true));
}
