#include "../parse.h"

unsigned parse_stmt_assignment(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_lhs(src, ptr,
		&stmt->assignment.lhs);
	if (i == 0) return 0;

	if (ptr[i++] != '=')
		return 0;

	unsigned len;
	stmt->assignment.rhs = parse_expr(
		src, &ptr[i], &len);
	if (!stmt->assignment.rhs)
	{
		sparse_error(src, &ptr[i],
			"Expected expression on right hand side of assignment");
		parse_lhs_cleanup(stmt->assignment.lhs);
		return 0;
	}
	i += len;

	stmt->type = PARSE_STMT_ASSIGNMENT;
	return i;
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
