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

	bool has_brackets = (ptr[i] == '(');
	if (has_brackets) i += 1;

	unsigned l;
	stmt->parameter.list = parse_assign_list(
		src, &ptr[i], debug, &l);
	if (!stmt->parameter.list)
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += l;

	if (has_brackets)
	{
		if (ptr[i++] != ')')
		{
			parse_assign_list_delete(
				stmt->parameter.list);
			parse_debug_rewind(debug, dpos);
			return 0;
		}
	}

	stmt->type = PARSE_STMT_PARAMETER;
	return i;
}

bool parse_stmt_parameter_print(
	colstr_t* cs, const parse_stmt_t* stmt)
{
    if (!stmt)
		return false;

	return (colstr_atomic_writef(cs, "PARAMETER ")
		&& parse_assign_list_print(
			cs, stmt->parameter.list));
}
