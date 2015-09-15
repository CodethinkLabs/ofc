#include "../parse.h"

unsigned parse_stmt_assignment(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	/* TODO - parse_lhs. */
	unsigned i = parse_name(src, ptr,
		&stmt->assignment.lhs);
	if (i == 0) return 0;

	if (ptr[i++] != '=')
		return 0;

	unsigned len = parse_expr(
		src, &ptr[i], &stmt->assignment.rhs);
	if (len == 0)
	{
		sparse_warning(src, &ptr[i],
			"Expected expression on right hand side of assignment");
		return 0;
	}

	stmt->type = PARSE_STMT_ASSIGNMENT;
	return (i + len);
}

unsigned parse_stmt_assign(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, PARSE_KEYWORD_ASSIGN);
	if (i == 0) return 0;

	unsigned len = parse_unsigned(
		src, &ptr[i], &stmt->assign.label);
	if (len == 0) return 0;
	i += len;

	len = parse_keyword(
		src, &ptr[i], PARSE_KEYWORD_TO);
	if (len == 0) return 0;
	i += 2;

	len = parse_name(src, &ptr[i],
		&stmt->assign.variable);
	if (len == 0) return 0;
	i += len;

	stmt->type = PARSE_STMT_ASSIGN;
	return i;
}
