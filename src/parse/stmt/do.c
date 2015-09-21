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

	stmt->do_loop.iterator
		= parse_lhs(src, &ptr[i], &len);
	if (!stmt->do_loop.iterator)
		return 0;
	i += len;

	if (ptr[i++] != '=')
		return 0;

	stmt->type = PARSE_STMT_DO;
	stmt->do_loop.init = NULL;
	stmt->do_loop.last = NULL;
	stmt->do_loop.step = NULL;

	stmt->do_loop.init = parse_expr(
		src, &ptr[i], &len);
	if (!stmt->do_loop.init)
		return 0;
	i += len;

	if (ptr[i++] != ',')
	{
		parse_expr_delete(stmt->do_loop.init);
		parse_lhs_delete(stmt->do_loop.iterator);
		return 0;
	}

	stmt->do_loop.last = parse_expr(
		src, &ptr[i], &len);
	if (!stmt->do_loop.last)
	{
		parse_expr_delete(stmt->do_loop.init);
		parse_lhs_delete(stmt->do_loop.iterator);
		return 0;
	}
	i += len;

	if (ptr[i] == ',')
	{
		i += 1;

		stmt->do_loop.step = parse_expr(
			src, &ptr[i], &len);
		if (!stmt->do_loop.step)
		{
			parse_expr_delete(stmt->do_loop.last);
			parse_expr_delete(stmt->do_loop.init);
			parse_lhs_delete(stmt->do_loop.iterator);
			return 0;
		}
		i += len;
	}

	return i;
}
