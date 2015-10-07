#include "../parse.h"



unsigned parse_stmt_implicit(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, debug,
		PARSE_KEYWORD_IMPLICIT_NONE);
	if (i > 0)
	{
		stmt->type = PARSE_STMT_IMPLICIT_NONE;
		return i;
	}

	unsigned dpos = parse_debug_position(debug);

	i = parse_keyword(
		src, ptr, debug,
		PARSE_KEYWORD_IMPLICIT);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_IMPLICIT;
	unsigned len;
	stmt->implicit = parse_implicit_list(
		src, &ptr[i], debug, &len);
	if (!stmt->implicit)
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	return i;
}

bool parse_stmt_implicit_print(
	colstr_t* cs, const parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	if (stmt->type == PARSE_STMT_IMPLICIT_NONE)
		return colstr_atomic_writef(cs, "IMPLICIT NONE");

	return (colstr_atomic_writef(cs, "IMPLICIT ")
		&& parse_implicit_list_print(cs, stmt->implicit));
}
