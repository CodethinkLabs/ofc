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
	stmt->read_write.file = parse_expr(
		src, &ptr[i], &len);
	if (!stmt->read_write.file) return 0;
	i += len;

	if (ptr[i++] != ',')
	{
		parse_expr_delete(stmt->read_write.file);
		return 0;
	}

	len = parse_label(
		src, &ptr[i], &stmt->read_write.format_label);
	if (len == 0)
	{
		parse_expr_delete(
			stmt->read_write.file);
		return 0;
	}
	i += len;

	if (ptr[i++] != ')')
	{
		parse_expr_delete(
			stmt->read_write.file);
		return 0;
	}

	stmt->type = PARSE_STMT_READ;

	stmt->read_write.args = parse_iolist(
		src, &ptr[i], &len);
	if (stmt->read_write.args) i += len;

	return i;
}
