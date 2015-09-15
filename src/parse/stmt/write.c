#include "../parse.h"

unsigned parse_stmt_write(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, PARSE_KEYWORD_WRITE);
	if (i == 0) return 0;

	if (ptr[i++] != '(')
		return 0;

	/* TODO - Implement WRITE statements fully, not just the NIST form. */

	unsigned len = parse_expr(
		src, &ptr[i], &stmt->write.file);
	if (len == 0) return 0;
	i += len;

	if (ptr[i++] != ',')
	{
		parse_expr_cleanup(stmt->write.file);
		return 0;
	}

	len = parse_unsigned(
		src, &ptr[i], &stmt->write.format_label);
	if (len == 0)
	{
		parse_expr_cleanup(
			stmt->write.file);
		return 0;
	}
	i += len;

	if (ptr[i++] != ')')
	{
		parse_expr_cleanup(
			stmt->write.file);
		return 0;
	}

	stmt->type = PARSE_STMT_WRITE;
	stmt->write.elem_count = 0;
	stmt->write.elem = NULL;

	parse_expr_t expr;
	len = parse_expr(src, &ptr[i], &expr);
	if (len == 0) return i;
	i += len;

	stmt->write.elem
		= (parse_expr_t*)malloc(
			sizeof(parse_expr_t));
	if (!stmt->write.elem)
	{
		parse_stmt_cleanup(*stmt);
		return 0;
	}

	stmt->write.elem[stmt->write.elem_count++] = expr;

	while (ptr[i] == ',')
	{
		unsigned j = (i + 1);

		len = parse_expr(src, &ptr[j], &expr);
		if (len == 0) return i;

		parse_expr_t* nelem
			= (parse_expr_t*)realloc(stmt->write.elem,
				sizeof(parse_expr_t) * (stmt->write.elem_count + 1));
		if (!nelem)
		{
			parse_expr_cleanup(expr);
			parse_stmt_cleanup(*stmt);
			return 0;
		}
		stmt->write.elem = nelem;
		stmt->write.elem[stmt->write.elem_count++] = expr;
		i = (j + len);
	}

	return i;
}
