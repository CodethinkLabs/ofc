#include "../parse.h"

unsigned parse_stmt_cycle(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, debug, PARSE_KEYWORD_CYCLE);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_CYCLE;
	return i;
}

bool parse_stmt_cycle_print(
	colstr_t* cs, const parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	return colstr_atomic_writef(cs, "CYCLE");
}
