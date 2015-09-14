#include "../parse.h"

unsigned parse_stmt_stop_pause(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, PARSE_KEYWORD_STOP);
	if (i > 0)
	{
		stmt->type = PARSE_STMT_STOP;
	}
	else
	{
		i = parse_keyword(
			src, ptr, PARSE_KEYWORD_PAUSE);
		if (i == 0) return 0;
		stmt->type = PARSE_STMT_PAUSE;
	}

	unsigned len = parse_expr(
		src, &ptr[i], &stmt->stop_pause.code);
	stmt->stop_pause.has_code = (len > 0);

	return (i + len);
}
