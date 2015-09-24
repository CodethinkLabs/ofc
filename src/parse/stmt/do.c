#include "../parse.h"

unsigned parse_stmt__do_while_block(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(src, ptr,
		PARSE_KEYWORD_WHILE);
	if (i == 0) return 0;

	if (ptr[i++] != '(')
		return 0;

	unsigned len;
	stmt->do_while.cond = parse_expr(
		src, &ptr[i], &len);
	if (!stmt->do_while.cond)
		return 0;
	i += len;

	if (ptr[i++] != ')')
	{
		parse_expr_delete(stmt->do_while_block.cond);
		return 0;
	}

	/* TODO - Make optional? */
	if (!is_end_statement(&ptr[i], &len))
	{
		parse_expr_delete(stmt->do_while_block.cond);
		return 0;
	}
	i += len;

	stmt->do_while_block.block
		= parse_stmt_list(src, &ptr[i], &len);
	if (stmt->do_while_block.block) i += len;

	len = parse_keyword_end(src, &ptr[i],
		PARSE_KEYWORD_DO);
	if (len == 0)
	{
		parse_stmt_list_delete(
			stmt->do_while_block.block);
		parse_expr_delete(stmt->do_while_block.cond);
		return 0;
	}
	i += len;

	stmt->type = PARSE_STMT_DO_WHILE_BLOCK;
	return i;
}

unsigned parse_stmt__do_while(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_label(
		src, ptr, &stmt->do_while.end_label);
	if (i == 0) return 0;

	if (ptr[i] == ',')
		i += 1;

	unsigned len = parse_keyword(src, &ptr[i],
		PARSE_KEYWORD_WHILE);
	if (len == 0) return 0;
	i += len;

	if (ptr[i++] != '(')
		return 0;

	stmt->do_while.cond = parse_expr(
		src, &ptr[i], &len);
	if (!stmt->do_while.cond)
		return 0;
	i += len;

	if (ptr[i++] != ')')
	{
		parse_expr_delete(stmt->do_while.cond);
		return 0;
	}

	stmt->type = PARSE_STMT_DO_WHILE;
	return i;
}

unsigned parse_stmt__do(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_label(
		src, ptr, &stmt->do_loop.end_label);
	if (i == 0) return 0;

	if (ptr[i] == ',')
		i += 1;

	unsigned len;
	stmt->do_loop.init
		= parse_assign_init(src, &ptr[i], &len);
	if (!stmt->do_loop.init)
		return 0;
	i += len;

	stmt->type = PARSE_STMT_DO;
	stmt->do_loop.last = NULL;
	stmt->do_loop.step = NULL;

	if (ptr[i++] != ',')
	{
		parse_assign_delete(stmt->do_loop.init);
		return 0;
	}

	stmt->do_loop.last = parse_expr(
		src, &ptr[i], &len);
	if (!stmt->do_loop.last)
	{
		parse_assign_delete(stmt->do_loop.init);
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
			parse_assign_delete(stmt->do_loop.init);
			return 0;
		}
		i += len;
	}

	return i;
}

unsigned parse_stmt_do(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, PARSE_KEYWORD_DO);
	if (i == 0) return 0;

	unsigned l;

	l = parse_stmt__do_while(
		src, &ptr[i], stmt);
	if (l > 0) return (i + l);

	l = parse_stmt__do_while_block(
		src, &ptr[i], stmt);
	if (l > 0) return (i + l);

	l = parse_stmt__do(
		src, &ptr[i], stmt);
	if (l > 0) return (i + l);

	return 0;
}
