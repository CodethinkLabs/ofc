#include "../parse.h"

static unsigned parse_stmt_go_to_unconditional(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, PARSE_KEYWORD_GO_TO);
	if (i == 0) return 0;

	unsigned len = parse_label(
		src, &ptr[i], &stmt->go_to.label);
	if (len == 0)
	{
		sparse_error(src, &ptr[i],
			"Expected label in GO TO statement");
		return 0;
	}

	stmt->type = PARSE_STMT_GO_TO;
	return (i + len);
}

static unsigned parse_stmt_go_to_assigned(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, PARSE_KEYWORD_GO_TO);
	if (i == 0) return 0;

	unsigned len = parse_expr(
		src, &ptr[i], &stmt->go_to_comp.cond);
	if (len == 0) return 0;
	i += len;

	if ((ptr[i++] != ',')
		|| (ptr[i++] != '('))
	{
		parse_expr_cleanup(
			stmt->go_to_comp.cond);
		return 0;
	}

	parse_label_t label;
	len = parse_label(
		src, &ptr[i], &label);
	if (len == 0)
	{
		parse_expr_cleanup(
			stmt->go_to_comp.cond);
		return 0;
	}
	i += len;

	stmt->type = PARSE_STMT_GO_TO_ASSIGNED;

	stmt->go_to_comp.label_count = 1;
	stmt->go_to_comp.label = (parse_label_t*)malloc(
		sizeof(parse_label_t) * stmt->go_to_comp.label_count);
	if (!stmt->go_to_comp.label)
	{
		parse_expr_cleanup(
			stmt->go_to_comp.cond);
		return 0;
	}
	stmt->go_to_comp.label[0] = label;

	while (ptr[i] == ',')
	{
		unsigned j = (i + 1);
		len = parse_label(
			src, &ptr[j], &label);
		if (len == 0) break;

		parse_label_t* nlabel = (parse_label_t*)realloc(stmt->go_to_comp.label,
			sizeof(parse_label_t) * (stmt->go_to_comp.label_count + 1));
		if (!nlabel)
		{
			parse_stmt_cleanup(*stmt);
			return 0;
		}
		stmt->go_to_comp.label = nlabel;
		stmt->go_to_comp.label[stmt->go_to_comp.label_count++] = label;
		i = (j + len);
	}

	if (ptr[i++] != ')')
	{
		free(stmt->go_to_comp.label);
		parse_expr_cleanup(
			stmt->go_to_comp.cond);
		return 0;
	}

	return i;
}

unsigned parse_stmt_go_to(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned len = parse_stmt_go_to_assigned(
		src, ptr, stmt);
	if (len > 0) return len;

	/* TODO - Implement GO TO (COMPUTED). */

	return parse_stmt_go_to_unconditional(
		src, ptr, stmt);
}
