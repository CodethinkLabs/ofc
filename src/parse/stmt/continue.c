#include "../parse.h"

unsigned parse_stmt_continue(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, PARSE_KEYWORD_CONTINUE);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_CONTINUE;
	return i;
}

bool parse_stmt_continue_print(
	int fd, const parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	return dprintf_bool(fd, "CONTINUE");
}
