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
	if (ptr[i] == ')')
	{
		i += 1;
		stmt->format.desc = NULL;
		stmt->format.desc_count = 0;
	}
	else
	{
		unsigned len = parse_format_desc_list(
			src, &ptr[i], ')',
			&stmt->format.desc,
			&stmt->format.desc_count);
		if (len == 0) return 0;
		i += (len + 1);
	}

	stmt->type = PARSE_STMT_FORMAT;
	return i;
}
