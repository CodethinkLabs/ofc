#include "../parse.h"


unsigned parse_stmt_parameter(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, PARSE_KEYWORD_PARAMETER);
	if (i == 0) return 0;

	if (ptr[i++] != '(')
		return 0;

	unsigned l;
	stmt->parameter.list = parse_assign_list(
		src, &ptr[i], &l);
	if (!stmt->parameter.list) return 0;
	i += l;

	if (ptr[i++] != ')')
	{
		parse_assign_list_delete(
			stmt->parameter.list);
		return 0;
	}

	stmt->type = PARSE_STMT_PARAMETER;
	return i;
}

bool parse_stmt_parameter_print(
	int fd, const parse_stmt_t* stmt)
{
    if (!stmt)
		return false;

	return (dprintf_bool(fd, "PARAMETER ")
		&& parse_assign_list_print(
			fd, stmt->parameter.list));
}
