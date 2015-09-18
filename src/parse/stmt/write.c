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

	unsigned len;
	stmt->write.file = parse_expr(
		src, &ptr[i], &len);
	if (!stmt->write.file) return 0;
	i += len;

	if (ptr[i++] != ',')
	{
		parse_expr_delete(stmt->write.file);
		return 0;
	}

	len = parse_label(
		src, &ptr[i], &stmt->write.format_label);
	if (len == 0)
	{
		parse_expr_delete(
			stmt->write.file);
		return 0;
	}
	i += len;

	if (ptr[i++] != ')')
	{
		parse_expr_delete(
			stmt->write.file);
		return 0;
	}

	stmt->type = PARSE_STMT_WRITE;
	stmt->write.elem_count = 0;
	stmt->write.elem = NULL;

	parse_expr_t* expr = parse_expr(
		src, &ptr[i], &len);
	if (!expr) return i;
	i += len;

	stmt->write.elem
		= (parse_expr_t**)malloc(
			sizeof(parse_expr_t*));
	if (!stmt->write.elem)
	{
		parse_expr_delete(expr);

		unsigned e;
		for (e = 0; e < stmt->write.elem_count; e++)
			parse_expr_delete(stmt->write.elem[e]);
		free(stmt->write.elem);

		parse_expr_delete(stmt->write.file);
		return 0;
	}

	stmt->write.elem[stmt->write.elem_count++] = expr;

	while (ptr[i] == ',')
	{
		unsigned j = (i + 1);

		expr = parse_expr(src, &ptr[j], &len);
		if (!expr) break;

		parse_expr_t** nelem
			= (parse_expr_t**)realloc(stmt->write.elem,
				sizeof(parse_expr_t*) * (stmt->write.elem_count + 1));
		if (!nelem)
		{
			parse_expr_delete(expr);

			unsigned e;
			for (e = 0; e < stmt->write.elem_count; e++)
				parse_expr_delete(stmt->write.elem[e]);
			free(stmt->write.elem);

			parse_expr_delete(stmt->write.file);
			return 0;
		}
		stmt->write.elem = nelem;
		stmt->write.elem[stmt->write.elem_count++] = expr;
		i = (j + len);
	}

	return i;
}
