#include "../parse.h"

unsigned parse_stmt_call(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, PARSE_KEYWORD_CALL);
	if (i == 0) return 0;

	unsigned len = parse_name(
		src, &ptr[i], &stmt->call.name);
	if (len == 0) return 0;
	i += len;

	stmt->call.args = NULL;
	if (ptr[i] == '(')
	{
		i += 1;

		stmt->call.args = parse_expr_list(
			src, &ptr[i], &len);
		if (stmt->call.args) i += len;

		if (ptr[i++] != ')')
		{
			parse_expr_list_delete(
				stmt->call.args);
			return 0;
		}
	}

	stmt->type = PARSE_STMT_CALL;
	return i;
}
