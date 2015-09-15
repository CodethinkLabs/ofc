#include "../parse.h"

unsigned parse_stmt_do(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, PARSE_KEYWORD_DO);
	if (i == 0) return 0;

	/* TODO - parse_label */
	unsigned len = parse_label(
		src, &ptr[i], &stmt->do_loop.end_label);
	if (len == 0) return 0;
	i += len;

	len = parse_lhs(src, &ptr[i],
		&stmt->do_loop.iterator);
	if (len == 0) return 0;
	i += len;

	if (ptr[i++] != '=')
		return 0;

	stmt->type = PARSE_STMT_DO;
	stmt->do_loop.init = PARSE_EXPR_EMPTY;
	stmt->do_loop.last = PARSE_EXPR_EMPTY;
	stmt->do_loop.step = PARSE_EXPR_EMPTY;

	len = parse_expr(
		src, &ptr[i], &stmt->do_loop.init);
	if (len == 0) return 0;
	i += len;

	if (ptr[i++] != ',')
	{
		parse_stmt_cleanup(*stmt);
		return 0;
	}

	len = parse_expr(
		src, &ptr[i], &stmt->do_loop.last);
	if (len == 0)
	{
		parse_stmt_cleanup(*stmt);
		return 0;
	}
	i += len;

	if (ptr[i] == ',')
	{
		i += 1;

		len = parse_expr(
			src, &ptr[i], &stmt->do_loop.step);
		if (len == 0)
		{
			parse_stmt_cleanup(*stmt);
			return 0;
		}
		i += len;
	}

	return i;
}
