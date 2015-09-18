#include "../parse.h"

unsigned parse_stmt_read(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, PARSE_KEYWORD_READ);
	if (i == 0) return 0;

	if (ptr[i++] != '(')
		return 0;

	/* TODO - Implement READ statements fully, not just the NIST form. */

	unsigned len;
	stmt->read.file = parse_expr(
		src, &ptr[i], &len);
	if (!stmt->read.file) return 0;
	i += len;

	if (ptr[i++] != ',')
	{
		parse_expr_delete(stmt->read.file);
		return 0;
	}

	len = parse_label(
		src, &ptr[i], &stmt->read.format_label);
	if (len == 0)
	{
		parse_expr_delete(
			stmt->read.file);
		return 0;
	}
	i += len;

	if (ptr[i++] != ')')
	{
		parse_expr_delete(
			stmt->read.file);
		return 0;
	}

	stmt->type = PARSE_STMT_READ;

	stmt->read.elem = parse_expr_list(
		src, &ptr[i], &len);
	if (stmt->read.elem) i += len;

	return i;
}
