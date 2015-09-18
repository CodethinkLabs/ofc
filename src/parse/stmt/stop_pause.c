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

	unsigned len = 0;
	stmt->stop_pause.code = parse_expr(
		src, &ptr[i], &len);

	return (i + len);
}
