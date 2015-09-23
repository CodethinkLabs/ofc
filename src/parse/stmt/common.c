#include "../parse.h"


unsigned parse_stmt_common(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, PARSE_KEYWORD_COMMON);
	if (i == 0) return 0;

	unsigned l;
	stmt->common
		= parse_common_group_list(
			src, &ptr[i], &l);
	if (!stmt->common) return 0;
	i += l;

	stmt->type = PARSE_STMT_COMMON;
	return i;
}

bool parse_stmt_common_print(
	int fd, const parse_stmt_t* stmt)
{
	return (stmt && dprintf_bool(fd, "COMMON ")
		&& parse_common_group_list_print(
			fd, stmt->common));
}
