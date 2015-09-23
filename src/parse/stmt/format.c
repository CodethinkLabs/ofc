#include "../parse.h"

unsigned parse_stmt_format(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, PARSE_KEYWORD_FORMAT);
	if (i == 0) return 0;

	if (ptr[i++] != '(')
		return 0;

	/* Empty FORMAT statements are valid as in nist/FM110.FOR */
	unsigned l;
	stmt->format = parse_format_desc_list(
		src, &ptr[i], &l);
	if (stmt->format) i += l;

	if (ptr[i++] != ')')
	{
		parse_format_desc_list_delete(
			stmt->format);
		return 0;
	}

	stmt->type = PARSE_STMT_FORMAT;
	return i;
}

bool parse_stmt_format_print(
	int fd, const parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	if (!dprintf_bool(fd, "FORMAT("))
		return false;

	if (stmt->format
		&& !parse_format_desc_list_print(
			fd, stmt->format))
		return false;

	return dprintf_bool(fd, ")");
}
