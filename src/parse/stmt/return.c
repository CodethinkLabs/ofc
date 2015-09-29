#include "../parse.h"

static unsigned parse_stmt__return(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_keyword_e keyword,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, debug, keyword);
	if (i == 0) return 0;

	unsigned len = 0;
	stmt->stop_pause_return.value = parse_expr(
		src, &ptr[i], debug, &len);

	return (i + len);
}

unsigned parse_stmt_stop(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__return(
		src, ptr, debug,
		PARSE_KEYWORD_STOP, stmt);
	if (i == 0) return 0;
	stmt->type = PARSE_STMT_STOP;
	return i;
}

unsigned parse_stmt_pause(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__return(
		src, ptr, debug,
		PARSE_KEYWORD_PAUSE, stmt);
	if (i == 0) return 0;
	stmt->type = PARSE_STMT_PAUSE;
	return i;
}

unsigned parse_stmt_return(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__return(
		src, ptr, debug,
		PARSE_KEYWORD_RETURN, stmt);
	if (i == 0) return 0;
	stmt->type = PARSE_STMT_RETURN;
	return i;
}


bool parse_stmt_stop_pause_return_print(
	int fd, const parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	const char* kwstr;
	switch (stmt->type)
	{
		case PARSE_STMT_STOP:
			kwstr = "STOP";
			break;
		case PARSE_STMT_PAUSE:
			kwstr = "PAUSE";
			break;
		case PARSE_STMT_RETURN:
			kwstr = "RETURN";
			break;
		default:
			return false;
	}

	if (!dprintf_bool(fd, "%s", kwstr))
		return false;

	if (stmt->stop_pause_return.value)
	{
		if (!dprintf_bool(fd, " ")
			|| !parse_expr_print(fd,
				stmt->stop_pause_return.value))
			return false;
	}

	return true;
}
