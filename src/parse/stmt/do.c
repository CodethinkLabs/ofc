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
	stmt->do_while_block.cond = parse_expr(
		src, &ptr[i], debug, &len);
	if (!stmt->do_while_block.cond)
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

unsigned parse_stmt__do_label(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned dpos = parse_debug_position(debug);

	unsigned i = parse_label(
		src, ptr, debug,
		&stmt->do_label.end_label);
	if (i == 0) return 0;

	if (ptr[i] != ',')
	{
		/* We only support numeric labels without a comma,
		   because identifiers would be ambiguous. */
		if (stmt->do_label.end_label.type
			!= PARSE_LABEL_NUMBER)
		{
			parse_debug_rewind(debug, dpos);
			return 0;
		}
	}
	else
	{
		i += 1;
	}

	unsigned len;
	stmt->do_label.init
		= parse_assign_init(
			src, &ptr[i], debug, &len);
	if (!stmt->do_label.init)
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	stmt->type = PARSE_STMT_DO_LABEL;
	stmt->do_label.last = NULL;
	stmt->do_label.step = NULL;

	if (ptr[i++] != ',')
	{
		parse_assign_delete(stmt->do_label.init);
		parse_debug_rewind(debug, dpos);
		return 0;
	}

	stmt->do_label.last = parse_expr(
		src, &ptr[i], debug, &len);
	if (!stmt->do_label.last)
	{
		parse_assign_delete(stmt->do_label.init);
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	if (ptr[i] == ',')
	{
		i += 1;

		stmt->do_label.step = parse_expr(
			src, &ptr[i], debug, &len);
		if (!stmt->do_label.step)
		{
			parse_expr_delete(stmt->do_label.last);
			parse_assign_delete(stmt->do_label.init);
			parse_debug_rewind(debug, dpos);
			return 0;
		}
		i += len;
	}

	return i;
}

unsigned parse_stmt__do_block(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned dpos = parse_debug_position(debug);

	unsigned i = 0;
	stmt->do_block.init
		= parse_assign_init(
			src, &ptr[i], debug, &i);
	if (!stmt->do_block.init)
		return 0;

	stmt->type = PARSE_STMT_DO_BLOCK;
	stmt->do_block.last = NULL;
	stmt->do_block.step = NULL;

	if (ptr[i++] != ',')
	{
		parse_assign_delete(stmt->do_block.init);
		parse_debug_rewind(debug, dpos);
		return 0;
	}

	unsigned len;
	stmt->do_block.last = parse_expr(
		src, &ptr[i], debug, &len);
	if (!stmt->do_block.last)
	{
		parse_assign_delete(stmt->do_block.init);
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	if (ptr[i] == ',')
	{
		i += 1;

		stmt->do_block.step = parse_expr(
			src, &ptr[i], debug, &len);
		if (!stmt->do_block.step)
		{
			parse_expr_delete(stmt->do_block.last);
			parse_assign_delete(stmt->do_block.init);
			parse_debug_rewind(debug, dpos);
			return 0;
		}
		i += len;
	}

	/* TODO - Make optional? */
	if (!is_end_statement(&ptr[i], &len))
	{
		parse_expr_delete(stmt->do_block.last);
		parse_assign_delete(stmt->do_block.init);
		parse_debug_rewind(debug, dpos);
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	stmt->do_block.block
		= parse_stmt_list(src, &ptr[i], debug, &len);
	if (stmt->do_block.block) i += len;

	len = parse_keyword_end(
		src, &ptr[i], debug,
		PARSE_KEYWORD_DO);
	if (len == 0)
	{
		parse_stmt_list_delete(
			stmt->do_block.block);
		parse_expr_delete(stmt->do_block.last);
		parse_assign_delete(stmt->do_block.init);
		parse_debug_rewind(debug, dpos);
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

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

	/* This must come before do_block. */
	l = parse_stmt__do_label(
		src, &ptr[i], debug, stmt);
	if (l > 0) return (i + l);

	l = parse_stmt__do_block(
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
		|| !dprintf_bool(fd, ")\n"))
		return false;

	if (stmt->do_while_block.block && !parse_stmt_list_print(
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

bool parse_stmt__do_label_print(
	int fd, const parse_stmt_t* stmt)
{
	if (!dprintf_bool(fd, "DO ")
		|| !parse_label_print(fd, stmt->do_label.end_label)
		|| !dprintf_bool(fd, ", ")
		|| !parse_assign_print(fd, stmt->do_label.init)
		|| !dprintf_bool(fd, ", ")
		|| !parse_expr_print(fd, stmt->do_label.last))
		return false;

	if (stmt->do_label.step)
	{
		if (!dprintf_bool(fd, ", ")
			|| !parse_expr_print(
				fd, stmt->do_label.step))
			return false;
	}

	return true;
}

bool parse_stmt__do_block_print(
	int fd, const parse_stmt_t* stmt, unsigned indent)
{
	if (!dprintf_bool(fd, "DO ")
		|| !parse_assign_print(fd, stmt->do_block.init)
		|| !dprintf_bool(fd, ", ")
		|| !parse_expr_print(fd, stmt->do_block.last))
		return false;

	if (stmt->do_block.step)
	{
		if (!dprintf_bool(fd, ", ")
			|| !parse_expr_print(
				fd, stmt->do_block.step))
			return false;
	}

	if (!dprintf_bool(fd, "\n")
		|| !parse_stmt_list_print(
			fd, stmt->do_block.block, (indent + 1)))
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

bool parse_stmt_do_print(
	int fd, const parse_stmt_t* stmt, unsigned indent)
{
	if (!stmt)
		return false;

	switch (stmt->type)
	{
		case PARSE_STMT_DO_LABEL:
			return parse_stmt__do_label_print(fd, stmt);
		case PARSE_STMT_DO_BLOCK:
			return parse_stmt__do_block_print(
				fd, stmt, indent);
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
