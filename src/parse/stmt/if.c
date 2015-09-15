#include "../parse.h"

unsigned parse_stmt_if_computed(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, PARSE_KEYWORD_IF);
	if (i == 0) return 0;

	if (ptr[i++] != '(')
		return 0;

	unsigned len = parse_expr(
		src, &ptr[i], &stmt->if_comp.cond);
	if (len == 0) return 0;
	i += len;

	if (ptr[i++] != ')')
	{
		parse_expr_cleanup(
			stmt->if_comp.cond);
		return 0;
	}

	/* TODO - Create parse_label and use here. */
	unsigned label = 0;
	len = parse_unsigned(
		src, &ptr[i], &label);
	if (len == 0)
	{
		parse_expr_cleanup(
			stmt->if_comp.cond);
		return 0;
	}
	i += len;

	stmt->type = PARSE_STMT_IF_COMPUTED;

	stmt->if_comp.label_count = 1;
	stmt->if_comp.label = (unsigned*)malloc(
		sizeof(unsigned) * stmt->if_comp.label_count);
	if (!stmt->if_comp.label)
	{
		parse_expr_cleanup(
			stmt->if_comp.cond);
		return 0;
	}
	stmt->if_comp.label[0] = label;

	while (ptr[i] == ',')
	{
		unsigned j = (i + 1);
		len = parse_unsigned(
			src, &ptr[j], &label);
		if (len == 0) break;

		unsigned* nlabel = (unsigned*)realloc(stmt->if_comp.label,
			sizeof(unsigned) * (stmt->if_comp.label_count + 1));
		if (!nlabel)
		{
			parse_stmt_cleanup(*stmt);
			return 0;
		}
		stmt->if_comp.label = nlabel;
		stmt->if_comp.label[stmt->if_comp.label_count++] = label;
		i = (j + len);
	}

	return i;
}
