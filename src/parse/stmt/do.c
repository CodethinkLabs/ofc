#include "../parse.h"

unsigned parse_stmt__do_while_block(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned dpos = parse_debug_position(debug);

	unsigned i = parse_keyword(
		src, ptr, debug,
		PARSE_KEYWORD_WHILE);
	if (i == 0) return 0;

	if (ptr[i++] != '(')
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}

	unsigned len;
	stmt->do_while.cond = parse_expr(
		src, &ptr[i], debug, &len);
	if (!stmt->do_while.cond)
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	if (ptr[i++] != ')')
	{
		parse_expr_delete(stmt->do_while_block.cond);
		parse_debug_rewind(debug, dpos);
		return 0;
	}

	/* TODO - Make optional? */
	if (!is_end_statement(&ptr[i], &len))
	{
		parse_expr_delete(stmt->do_while_block.cond);
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	stmt->do_while_block.block
		= parse_stmt_list(src, &ptr[i], debug, &len);
	if (stmt->do_while_block.block) i += len;

	len = parse_keyword_end(
		src, &ptr[i], debug,
		PARSE_KEYWORD_DO);
	if (len == 0)
	{
		parse_stmt_list_delete(
			stmt->do_while_block.block);
		parse_expr_delete(stmt->do_while_block.cond);
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	stmt->type = PARSE_STMT_DO_WHILE_BLOCK;
	return i;
}

unsigned parse_stmt__do_while(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned dpos = parse_debug_position(debug);

	unsigned i = parse_label(
		src, ptr, debug,
		&stmt->do_while.end_label);
	if (i == 0) return 0;

	if (ptr[i] == ',')
		i += 1;

	unsigned len = parse_keyword(
		src, &ptr[i], debug,
		PARSE_KEYWORD_WHILE);
	if (len == 0)
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	if (ptr[i++] != '(')
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}

	stmt->do_while.cond = parse_expr(
		src, &ptr[i], debug, &len);
	if (!stmt->do_while.cond)
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	if (ptr[i++] != ')')
	{
		parse_expr_delete(stmt->do_while.cond);
		parse_debug_rewind(debug, dpos);
		return 0;
	}

	stmt->type = PARSE_STMT_DO_WHILE;
	return i;
}

unsigned parse_stmt__do(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned dpos = parse_debug_position(debug);

	unsigned i = parse_label(
		src, ptr, debug,
		&stmt->do_loop.end_label);
	if (i == 0) return 0;

	if (ptr[i] == ',')
		i += 1;

	unsigned len;
	stmt->do_loop.init
		= parse_assign_init(
			src, &ptr[i], debug, &len);
	if (!stmt->do_loop.init)
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	stmt->type = PARSE_STMT_DO;
	stmt->do_loop.last = NULL;
	stmt->do_loop.step = NULL;

	if (ptr[i++] != ',')
	{
		parse_assign_delete(stmt->do_loop.init);
		parse_debug_rewind(debug, dpos);
		return 0;
	}

	stmt->do_loop.last = parse_expr(
		src, &ptr[i], debug, &len);
	if (!stmt->do_loop.last)
	{
		parse_assign_delete(stmt->do_loop.init);
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	if (ptr[i] == ',')
	{
		i += 1;

		stmt->do_loop.step = parse_expr(
			src, &ptr[i], debug, &len);
		if (!stmt->do_loop.step)
		{
			parse_expr_delete(stmt->do_loop.last);
			parse_assign_delete(stmt->do_loop.init);
			parse_debug_rewind(debug, dpos);
			return 0;
		}
		i += len;
	}

	return i;
}

unsigned parse_stmt_do(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned dpos = parse_debug_position(debug);

	unsigned i = parse_keyword(
		src, ptr, debug, PARSE_KEYWORD_DO);
	if (i == 0) return 0;

	unsigned l;

	l = parse_stmt__do_while(
		src, &ptr[i], debug, stmt);
	if (l > 0) return (i + l);

	l = parse_stmt__do_while_block(
		src, &ptr[i], debug, stmt);
	if (l > 0) return (i + l);

	l = parse_stmt__do(
		src, &ptr[i], debug, stmt);
	if (l > 0) return (i + l);

	parse_debug_rewind(debug, dpos);
	return 0;
}



bool parse_stmt__do_while_block_print(
	int fd, const parse_stmt_t* stmt, unsigned indent)
{
	if (!dprintf_bool(fd, "DO WHILE(")
		|| !parse_expr_print(fd, stmt->do_while_block.cond)
		|| !dprintf_bool(fd, ")\n")
		|| !parse_stmt_list_print(
			fd, stmt->do_while_block.block, (indent + 1)))
		return false;

	if (!dprintf_bool(fd, "      "))
		return false;

	unsigned j;
	for (j = 0; j < indent; j++)
	{
		if (!dprintf_bool(fd, "  "))
			return false;
	}

	return dprintf_bool(fd, "END DO");
}

bool parse_stmt__do_while_print(
	int fd, const parse_stmt_t* stmt)
{
	return (dprintf_bool(fd, "DO ")
		&& parse_label_print(fd, stmt->do_while.end_label)
		&& dprintf_bool(fd, ", WHILE(")
		&& parse_expr_print(fd, stmt->do_while.cond)
		&& dprintf_bool(fd, ")"));
}

bool parse_stmt__do_print(
	int fd, const parse_stmt_t* stmt)
{
	if (!dprintf_bool(fd, "DO ")
		|| !parse_label_print(fd, stmt->do_loop.end_label)
		|| !dprintf_bool(fd, ", ")
		|| !parse_assign_print(fd, stmt->do_loop.init)
		|| !dprintf_bool(fd, ", ")
		|| !parse_expr_print(fd, stmt->do_loop.last))
		return false;

	if (stmt->do_loop.step)
	{
		if (!dprintf_bool(fd, ", ")
			|| !parse_expr_print(
				fd, stmt->do_loop.step))
			return false;
	}

	return true;
}

bool parse_stmt_do_print(
	int fd, const parse_stmt_t* stmt, unsigned indent)
{
	if (!stmt)
		return false;

	switch (stmt->type)
	{
		case PARSE_STMT_DO:
			return parse_stmt__do_print(fd, stmt);
		case PARSE_STMT_DO_WHILE:
			return parse_stmt__do_while_print(fd, stmt);
		case PARSE_STMT_DO_WHILE_BLOCK:
			return parse_stmt__do_while_block_print(
				fd, stmt, indent);
		default:
			break;
	}

	return false;
}
