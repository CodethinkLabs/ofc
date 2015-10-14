#include <ofc/parse.h>

static unsigned ofc_parse_stmt_if__computed(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_expr_t* cond,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	ofc_parse_label_t label;
	unsigned i = ofc_parse_label(
		src, ptr, debug, &label);
	if (i == 0) return 0;

	stmt->type = OFC_PARSE_STMT_IF_COMPUTED;
	stmt->if_comp.cond = cond;

	stmt->if_comp.label_count = 1;
	stmt->if_comp.label = (ofc_parse_label_t*)malloc(
		sizeof(ofc_parse_label_t) * stmt->if_comp.label_count);
	if (!stmt->if_comp.label)
	{
		ofc_parse_expr_delete(
			stmt->if_comp.cond);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	stmt->if_comp.label[0] = label;

	while (ptr[i] == ',')
	{
		unsigned j = (i + 1);
		unsigned len = ofc_parse_label(
			src, &ptr[j], debug, &label);
		if (len == 0) break;

		ofc_parse_label_t* nlabel = (ofc_parse_label_t*)realloc(stmt->if_comp.label,
			sizeof(ofc_parse_label_t) * (stmt->if_comp.label_count + 1));
		if (!nlabel)
		{
			free(stmt->if_comp.label);
			ofc_parse_expr_delete(
				stmt->if_comp.cond);
			ofc_parse_debug_rewind(debug, dpos);
			return 0;
		}
		stmt->if_comp.label = nlabel;
		stmt->if_comp.label[stmt->if_comp.label_count++] = label;
		i = (j + len);
	}

	return i;
}

static unsigned ofc_parse_stmt_if__statement(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_expr_t* cond,
	ofc_parse_stmt_t* stmt)
{
	unsigned i;
	stmt->if_stmt.stmt = ofc_parse_stmt(
		src, ptr, debug, &i);
	if (!stmt->if_stmt.stmt)
		return 0;

	/* Don't absorb the end of statement here. */
	if (ptr[i] != '\0')
		i -= 1;

	stmt->type = OFC_PARSE_STMT_IF_STATEMENT;
	stmt->if_stmt.cond = cond;
	return i;
}

static unsigned ofc_parse_stmt_if__then(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_expr_t* cond,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i;

	i = ofc_parse_keyword(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_THEN);
	if (i == 0) return 0;

	unsigned len;
	/* TODO - Make this optional? */
	if (!ofc_is_end_statement(&ptr[i], &len))
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	stmt->if_then.block_then
		= ofc_parse_stmt_list(src, &ptr[i], debug, &len);
	if (stmt->if_then.block_then)
	{
		if (ofc_parse_stmt_list_contains_error(
			stmt->if_then.block_then))
		{
			/* Don't rewind cause we want to report the error. */
			ofc_parse_stmt_list_delete(
				stmt->if_then.block_then);
			return 0;
		}

		i += len;
	}

	bool expect_end = true;

	stmt->if_then.block_else = NULL;
	len = ofc_parse_keyword(
		src, &ptr[i], debug,
		OFC_PARSE_KEYWORD_ELSE);
	bool has_else = (len > 0);
	if (has_else)
	{
		i += len;

		ofc_parse_stmt_t* stmt_else
			= ofc_parse_stmt(src, &ptr[i], debug, &len);
		if (stmt_else
			&& (stmt_else->type != OFC_PARSE_STMT_IF_THEN))
		{
			ofc_parse_stmt_delete(stmt_else);
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
				= (ofc_parse_stmt_list_t*)malloc(
					sizeof(ofc_parse_stmt_list_t));
			if (!stmt->if_then.block_else)
			{
				ofc_parse_stmt_delete(stmt_else);
				ofc_parse_stmt_list_delete(stmt->if_then.block_then);
				ofc_parse_debug_rewind(debug, dpos);
				return 0;
			}

			stmt->if_then.block_else->stmt
				= (ofc_parse_stmt_t**)malloc(
					sizeof(ofc_parse_stmt_t*));
			if (!stmt->if_then.block_else->stmt)
			{
				free(stmt->if_then.block_else);
				ofc_parse_stmt_delete(stmt_else);
				ofc_parse_stmt_list_delete(stmt->if_then.block_then);
				ofc_parse_debug_rewind(debug, dpos);
				return 0;
			}

			stmt->if_then.block_else->count = 1;
			stmt->if_then.block_else->stmt[0] = stmt_else;
		}
		else
		{
			/* TODO - Make this optional? */
			if (!ofc_is_end_statement(&ptr[i], &len))
			{
				ofc_parse_debug_rewind(debug, dpos);
				return 0;
			}
			i += len;

			stmt->if_then.block_else
				= ofc_parse_stmt_list(src, &ptr[i], debug, &len);
			if (stmt->if_then.block_else)
			{
				if (ofc_parse_stmt_list_contains_error(
					stmt->if_then.block_else))
				{
					/* Don't rewind cause we want to report the error. */
					ofc_parse_stmt_list_delete(
						stmt->if_then.block_else);
					ofc_parse_stmt_list_delete(
						stmt->if_then.block_then);
					return 0;
				}

				i += len;
			}
		}
	}

	if (expect_end)
	{
		len = ofc_parse_keyword_end(
			src, &ptr[i], debug,
			OFC_PARSE_KEYWORD_IF, false);
		if (len == 0)
		{
			ofc_sparse_error(src, &ptr[i],
				"Invalid statement in %s body",
				(has_else ? "ELSE" : "IF THEN"));

			ofc_parse_stmt_list_delete(
				stmt->if_then.block_else);
			ofc_parse_stmt_list_delete(
				stmt->if_then.block_then);

			stmt->type = OFC_PARSE_STMT_ERROR;
			return i;
		}
		i += len;
	}

	stmt->type = OFC_PARSE_STMT_IF_THEN;
	stmt->if_then.cond = cond;
	return i;
}


unsigned ofc_parse_stmt_if(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = ofc_parse_keyword(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_IF);
	if (i == 0) return 0;

	if (ptr[i++] != '(')
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}

	unsigned len;
	ofc_parse_expr_t* cond = ofc_parse_expr(
		src, &ptr[i], debug, &len);
	if (!cond)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	if (ptr[i++] != ')')
	{
		ofc_parse_expr_delete(cond);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}

	len = ofc_parse_stmt_if__then(src, &ptr[i], debug, cond, stmt);
	if (len == 0)
		len = ofc_parse_stmt_if__statement(src, &ptr[i], debug, cond, stmt);
	if (len == 0)
		len = ofc_parse_stmt_if__computed(src, &ptr[i], debug, cond, stmt);

	if (len == 0)
	{
		ofc_parse_expr_delete(cond);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	else if (stmt->type == OFC_PARSE_STMT_ERROR)
	{
		ofc_parse_expr_delete(cond);
	}
	i += len;

	return i;
}

bool ofc_parse_stmt_if_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt, unsigned indent)
{
	if (!stmt)
		return false;

	if (stmt->type == OFC_PARSE_STMT_IF_COMPUTED)
	{
		if (!ofc_colstr_atomic_writef(cs, "IF (")
			|| !ofc_parse_expr_print(cs, stmt->if_comp.cond)
			|| !ofc_colstr_atomic_writef(cs, ")"))
			return false;

		unsigned i;
		for (i = 0; i < stmt->if_comp.label_count; i++)
		{
			if (!ofc_colstr_atomic_writef(cs, (i > 0 ? ", " : " "))
				|| !ofc_parse_label_print(cs ,stmt->if_comp.label[i]))
				return false;
		}
	}
	else if (stmt->type == OFC_PARSE_STMT_IF_STATEMENT)
	{
		if (!ofc_colstr_atomic_writef(cs, "IF (")
			|| !ofc_parse_expr_print(cs, stmt->if_stmt.cond)
			|| !ofc_colstr_atomic_writef(cs, ") ")
			|| !ofc_parse_stmt_print(cs, stmt->if_stmt.stmt, indent))
			return false;
	}
	else if (stmt->type == OFC_PARSE_STMT_IF_THEN)
	{
		if (!ofc_colstr_atomic_writef(cs, "IF (")
			|| !ofc_parse_expr_print(cs, stmt->if_then.cond)
			|| !ofc_colstr_atomic_writef(cs, ") THEN"))
			return false;

		if (stmt->if_then.block_then
			&& !ofc_parse_stmt_list_print(
				cs, stmt->if_then.block_then, (indent + 1)))
			return false;

		if (stmt->if_then.block_else)
		{
			if (!ofc_colstr_newline(cs, NULL))
				return false;

			unsigned i;
			for (i = 0; i < indent; i++)
			{
				if (!ofc_colstr_atomic_writef(cs, "  "))
					return false;
			}

			if (!ofc_colstr_atomic_writef(cs, "ELSE"))
				return false;

			if (!ofc_parse_stmt_list_print(
				cs, stmt->if_then.block_else, (indent + 1)))
				return false;
		}

		if (!ofc_colstr_newline(cs, NULL))
				return false;

		unsigned i;
		for (i = 0; i < indent; i++)
		{
			if (!ofc_colstr_atomic_writef(cs, "  "))
				return false;
		}

		if (!ofc_colstr_atomic_writef(cs, "END IF"))
			return false;
	}
	else
	{
		return false;
	}

	return true;
}
