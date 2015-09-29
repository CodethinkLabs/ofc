#include "../parse.h"

static unsigned parse_stmt_if__computed(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_expr_t* cond,
	parse_stmt_t* stmt)
{
	unsigned dpos = parse_debug_position(debug);

	parse_label_t label;
	unsigned i = parse_label(
		src, ptr, debug, &label);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_IF_COMPUTED;
	stmt->if_comp.cond = cond;

	stmt->if_comp.label_count = 1;
	stmt->if_comp.label = (parse_label_t*)malloc(
		sizeof(parse_label_t) * stmt->if_comp.label_count);
	if (!stmt->if_comp.label)
	{
		parse_expr_delete(
			stmt->if_comp.cond);
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	stmt->if_comp.label[0] = label;

	while (ptr[i] == ',')
	{
		unsigned j = (i + 1);
		unsigned len = parse_label(
			src, &ptr[j], debug, &label);
		if (len == 0) break;

		parse_label_t* nlabel = (parse_label_t*)realloc(stmt->if_comp.label,
			sizeof(parse_label_t) * (stmt->if_comp.label_count + 1));
		if (!nlabel)
		{
			free(stmt->if_comp.label);
			parse_expr_delete(
				stmt->if_comp.cond);
			parse_debug_rewind(debug, dpos);
			return 0;
		}
		stmt->if_comp.label = nlabel;
		stmt->if_comp.label[stmt->if_comp.label_count++] = label;
		i = (j + len);
	}

	return i;
}

static unsigned parse_stmt_if__statement(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_expr_t* cond,
	parse_stmt_t* stmt)
{
	unsigned i;
	stmt->if_stmt.stmt = parse_stmt(
		src, ptr, debug, &i);
	if (!stmt->if_stmt.stmt)
		return 0;

	/* Don't absorb the end of statement here. */
	if (ptr[i] != '\0')
		i -= 1;

	stmt->type = PARSE_STMT_IF_STATEMENT;
	stmt->if_stmt.cond = cond;
	return i;
}

static unsigned parse_stmt_if__then(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_expr_t* cond,
	parse_stmt_t* stmt)
{
	unsigned dpos = parse_debug_position(debug);

	unsigned i;

	i = parse_keyword(
		src, ptr, debug,
		PARSE_KEYWORD_THEN);
	if (i == 0) return 0;

	unsigned len;
	/* TODO - Make this optional? */
	if (!is_end_statement(&ptr[i], &len))
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	stmt->if_then.block_then
		= parse_stmt_list(src, &ptr[i], debug, &len);
	if (stmt->if_then.block_then) i += len;

	bool expect_end = true;

	stmt->if_then.block_else = NULL;
	len = parse_keyword(
		src, &ptr[i], debug,
		PARSE_KEYWORD_ELSE);
	if (len > 0)
	{
		i += len;

		parse_stmt_t* stmt_else
			= parse_stmt(src, &ptr[i], debug, &len);
		if (stmt_else
			&& (stmt_else->type != PARSE_STMT_IF_THEN))
		{
			parse_stmt_delete(stmt_else);
			len = 0;
		}

		if (len > 0)
		{
			/* Don't absorb the end of statement here. */
			if (ptr[i + len] != '\0')
				len -= 1;

			i += len;
			expect_end = false;

			stmt->if_then.block_else
				= (parse_stmt_list_t*)malloc(
					sizeof(parse_stmt_list_t*));
			if (!stmt->if_then.block_else)
			{
				parse_stmt_delete(stmt_else);
				parse_stmt_list_delete(stmt->if_then.block_then);
				parse_debug_rewind(debug, dpos);
				return 0;
			}

			stmt->if_then.block_else->stmt
				= (parse_stmt_t**)malloc(
					sizeof(parse_stmt_t*));
			if (!stmt->if_then.block_else->stmt)
			{
				free(stmt->if_then.block_else);
				parse_stmt_delete(stmt_else);
				parse_stmt_list_delete(stmt->if_then.block_then);
				parse_debug_rewind(debug, dpos);
				return 0;
			}

			stmt->if_then.block_else->count = 1;
			stmt->if_then.block_else->stmt[0] = stmt_else;
		}
		else
		{
			/* TODO - Make this optional? */
			if (!is_end_statement(&ptr[i], &len))
			{
				parse_debug_rewind(debug, dpos);
				return 0;
			}
			i += len;

			stmt->if_then.block_else
				= parse_stmt_list(src, &ptr[i], debug, &len);
			if (stmt->if_then.block_else) i += len;
		}
	}

	if (expect_end)
	{
		len = parse_keyword_end(
			src, &ptr[i], debug,
			PARSE_KEYWORD_IF);
		if (len == 0)
		{
			parse_stmt_list_delete(
				stmt->if_then.block_then);
			parse_debug_rewind(debug, dpos);
			return 0;
		}
		i += len;
	}

	stmt->type = PARSE_STMT_IF_THEN;
	stmt->if_then.cond = cond;
	return i;
}


unsigned parse_stmt_if(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned dpos = parse_debug_position(debug);

	unsigned i = parse_keyword(
		src, ptr, debug,
		PARSE_KEYWORD_IF);
	if (i == 0) return 0;

	if (ptr[i++] != '(')
		return 0;

	unsigned len;
	parse_expr_t* cond = parse_expr(
		src, &ptr[i], debug, &len);
	if (!cond) return 0;
	i += len;

	if (ptr[i++] != ')')
	{
		parse_expr_delete(cond);
		parse_debug_rewind(debug, dpos);
		return 0;
	}

	len = parse_stmt_if__then(src, &ptr[i], debug, cond, stmt);
	if (len == 0)
		len = parse_stmt_if__statement(src, &ptr[i], debug, cond, stmt);
	if (len == 0)
		len = parse_stmt_if__computed(src, &ptr[i], debug, cond, stmt);

	if (len == 0)
	{
		parse_expr_delete(cond);
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	return i;
}

bool parse_stmt_if_print(
	int fd, const parse_stmt_t* stmt, unsigned indent)
{
	if (!stmt)
		return false;

	if (stmt->type == PARSE_STMT_IF_COMPUTED)
	{
		if (!dprintf_bool(fd, "IF (")
			|| !parse_expr_print(fd, stmt->if_comp.cond)
			|| !dprintf_bool(fd, ")"))
			return false;

		unsigned i;
		for (i = 0; i < stmt->if_comp.label_count; i++)
		{
			if (!dprintf_bool(fd, (i > 0 ? ", " : " "))
				|| !parse_label_print(fd ,stmt->if_comp.label[i]))
				return false;
		}
	}
	else if (stmt->type == PARSE_STMT_IF_STATEMENT)
	{
		if (!dprintf_bool(fd, "IF (")
			|| !parse_expr_print(fd, stmt->if_stmt.cond)
			|| !dprintf_bool(fd, ") ")
			|| !parse_stmt_print(fd, stmt->if_stmt.stmt, indent))
			return false;
	}
	else if (stmt->type == PARSE_STMT_IF_THEN)
	{
		if (!dprintf_bool(fd, "IF (")
			|| !parse_expr_print(fd, stmt->if_then.cond)
			|| !dprintf_bool(fd, ") THEN"))
			return false;

		if (stmt->if_then.block_then
			&& !parse_stmt_list_print(
				fd, stmt->if_then.block_then, (indent + 1)))
			return false;

		if (stmt->if_then.block_else)
		{
			if (!dprintf_bool(fd, "      "))
				return false;

			unsigned i;
			for (i = 0; i < indent; i++)
			{
				if (!dprintf_bool(fd, "  "))
					return false;
			}

			if (!dprintf_bool(fd, "ELSE\n"))
				return false;

			if (!parse_stmt_list_print(
				fd, stmt->if_then.block_else, (indent + 1)))
				return false;
		}

		if (!dprintf_bool(fd, "      "))
			return false;

		unsigned i;
		for (i = 0; i < indent; i++)
		{
			if (!dprintf_bool(fd, "  "))
				return false;
		}

		if (!dprintf_bool(fd, "END IF\n"))
			return false;
	}
	else
	{
		return false;
	}

	return true;
}
