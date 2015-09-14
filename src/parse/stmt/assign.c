#include "../parse.h"

unsigned parse_stmt_assign(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_name(src, ptr);
	if (i == 0) return 0;

	stmt->assign.lhs = str_ref(ptr, i);

	/* TODO - Parse more complex lhs. */

	if (ptr[i++] != '=')
		return 0;

	unsigned len = parse_expr(
		src, &ptr[i], &stmt->assign.rhs);
	if (len == 0)
	{
		sparse_warning(src, &ptr[i],
			"Expected expression on right hand side of assignment");
		return 0;
	}

	stmt->type = PARSE_STMT_ASSIGN;
	return (i + len);
}
