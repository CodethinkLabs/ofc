#include "../parse.h"


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

bool parse_stmt_assign_print(
	int fd, const parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	return (dprintf_bool(fd, "ASSIGN %u TO ", stmt->assign.label)
		&& str_ref_print(fd, stmt->assign.variable));
}
