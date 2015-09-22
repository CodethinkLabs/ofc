#include "../parse.h"

static unsigned parse_stmt_if__computed(
	const sparse_t* src, const char* ptr,
	parse_expr_t* cond,
	parse_stmt_t* stmt)
{

	parse_label_t label;
	unsigned i = parse_label(
		src, ptr, &label);
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
		return 0;
	}
	stmt->if_comp.label[0] = label;

	while (ptr[i] == ',')
	{
		unsigned j = (i + 1);
		unsigned len = parse_label(
			src, &ptr[j], &label);
		if (len == 0) break;

		parse_label_t* nlabel = (parse_label_t*)realloc(stmt->if_comp.label,
			sizeof(parse_label_t) * (stmt->if_comp.label_count + 1));
		if (!nlabel)
		{
			free(stmt->if_comp.label);
			parse_expr_delete(
				stmt->if_comp.cond);
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
	parse_expr_t* cond,
	parse_stmt_t* stmt)
{
	unsigned i;
	stmt->if_stmt.stmt = parse_stmt(
		src, ptr, &i);
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
	parse_expr_t* cond,
	parse_stmt_t* stmt)
{
	unsigned i;

	i = parse_keyword(
		src, ptr, PARSE_KEYWORD_THEN);
	if (i == 0) return 0;

	unsigned len;
	/* TODO - Make this optional? */
	if (!is_end_statement(&ptr[i], &len))
		return 0;
	i += len;

	stmt->if_then.block_then
		= parse_stmt_list(src, &ptr[i], &len);
	if (stmt->if_then.block_then) i += len;

	/* TODO - Support ELSE */

	len = parse_keyword_end(
		src, &ptr[i], PARSE_KEYWORD_IF);
	if (len == 0)
	{
		parse_stmt_list_delete(
			stmt->if_then.block_then);
		return 0;
	}
	i += len;

	stmt->type = PARSE_STMT_IF_THEN;
	stmt->if_then.cond = cond;
	stmt->if_then.block_else = NULL;
	return i;
}

unsigned parse_stmt_if(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, PARSE_KEYWORD_IF);
	if (i == 0) return 0;

	if (ptr[i++] != '(')
		return 0;

	unsigned len;
	parse_expr_t* cond = parse_expr(
		src, &ptr[i], &len);
	if (!cond) return 0;
	i += len;

	if (ptr[i++] != ')')
	{
		parse_expr_delete(cond);
		return 0;
	}

	len = parse_stmt_if__then(src, &ptr[i], cond, stmt);
	if (len == 0)
		len = parse_stmt_if__statement(src, &ptr[i], cond, stmt);
	if (len == 0)
		len = parse_stmt_if__computed(src, &ptr[i], cond, stmt);

	if (len == 0)
		parse_expr_delete(cond);

	return (len == 0 ? 0 : (i + len));
}
