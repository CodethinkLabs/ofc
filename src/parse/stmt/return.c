#include "../parse.h"

static unsigned parse_stmt__return(
	const sparse_t* src, const char* ptr,
	parse_keyword_e keyword,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, keyword);
	if (i == 0) return 0;

	unsigned len = 0;
	stmt->stop_pause_return.value = parse_expr(
		src, &ptr[i], &len);

	return (i + len);
}

unsigned parse_stmt_stop(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__return(
		src, ptr, PARSE_KEYWORD_STOP, stmt);
	if (i == 0) return 0;
	stmt->type = PARSE_STMT_STOP;
	return i;
}

unsigned parse_stmt_pause(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__return(
		src, ptr, PARSE_KEYWORD_PAUSE, stmt);
	if (i == 0) return 0;
	stmt->type = PARSE_STMT_PAUSE;
	return i;
}

unsigned parse_stmt_return(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__return(
		src, ptr, PARSE_KEYWORD_RETURN, stmt);
	if (i == 0) return 0;
	stmt->type = PARSE_STMT_RETURN;
	return i;
}
