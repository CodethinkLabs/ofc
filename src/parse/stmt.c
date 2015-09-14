#include "parse.h"


static unsigned parse_stmt_empty(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	(void)src;

	if ((ptr[0] != '\r')
		&& (ptr[0] != '\n')
		&& (ptr[0] != ';'))
		return 0;

	stmt->type = PARSE_STMT_EMPTY;
	return 1;
}

unsigned parse_stmt(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned len = 0;

	/* TODO - Handle labels. */

	if (len == 0) len = parse_stmt_empty(src, ptr, stmt);
	if (len == 0) len = parse_stmt_continue(src, ptr, stmt);
	if (len == 0) len = parse_stmt_stop_pause(src, ptr, stmt);
	if (len == 0) len = parse_stmt_go_to(src, ptr, stmt);
	if (len == 0) len = parse_stmt_assign(src, ptr, stmt);

	if (len == 0)
		return 0;

	if ((ptr[len] == '\r')
		|| (ptr[len] == '\n')
		|| (ptr[len] == ';'))
	{
		len += 1;
	}
	else
	{
		sparse_warning(src, ptr,
			"Expected newline or semicolon after statement");
	}

	return len;
}
