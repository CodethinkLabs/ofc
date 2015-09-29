#include "../parse.h"


unsigned parse_stmt_parameter(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned dpos = parse_debug_position(debug);

	unsigned i = parse_keyword(
		src, ptr, debug,
		PARSE_KEYWORD_PARAMETER);
	if (i == 0) return 0;

	if (ptr[i++] != '(')
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}

	unsigned l;
	stmt->parameter.list = parse_assign_list(
		src, &ptr[i], debug, &l);
	if (!stmt->parameter.list)
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += l;

	if (ptr[i++] != ')')
	{
		parse_assign_list_delete(
			stmt->parameter.list);
		parse_debug_rewind(debug, dpos);
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
