#include "parse.h"


unsigned parse_stmt(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned len = 0;
	stmt->type = PARSE_STMT_EMPTY;

	/* TODO - Handle labels. */

	if (len == 0) len = parse_stmt_continue(src, ptr, stmt);
	if (len == 0) len = parse_stmt_stop_pause(src, ptr, stmt);
	if (len == 0) len = parse_stmt_go_to(src, ptr, stmt);
	if (len == 0) len = parse_stmt_assign(src, ptr, stmt);

	if ((ptr[len] == '\r')
		|| (ptr[len] == '\n')
		|| (ptr[len] == ';'))
	{
		len += 1;
	}
	else
	{
		if (len == 0)
			return 0;

		sparse_warning(src, ptr,
			"Expected newline or semicolon after statement");
	}

	return len;
}

void parse_stmt_cleanup(
	parse_stmt_t stmt)
{
	switch (stmt.type)
	{
		case PARSE_STMT_ASSIGN:
			parse_expr_cleanup(stmt.assign.rhs);
			break;
		case PARSE_STMT_STOP:
		case PARSE_STMT_PAUSE:
			if (stmt.stop_pause.has_code)
				parse_expr_cleanup(stmt.stop_pause.code);
			break;
		default:
			break;
	}
}
