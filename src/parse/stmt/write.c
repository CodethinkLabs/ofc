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

	stmt->write.elem = parse_expr_list(
		src, &ptr[i], &len);
	if (stmt->write.elem) i += len;

	return i;
}
