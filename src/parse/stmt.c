#include "parse.h"

unsigned parse_stmt(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned len = 0;

	if ((ptr[len] == '\r')
		|| (ptr[len] == '\n')
		|| (ptr[len] == ';'))
	{
		sparse_warning(src, ptr,
			"Empty statement");

		return 1;
	}

	if (len == 0) len = parse_stmt_assign(src, ptr, stmt);

	if (len == 0)
		return 0;

	if ((ptr[len] == '\r')
		|| (ptr[len] == '\n')
		|| (ptr[len] == ';'))
	{
		len += 1;
	}
	else
	{
		sparse_warning(src, ptr,
			"Expected newline or semicolon after statement");
	}

	return len;
}
