#include "../parse.h"


static unsigned parse_stmt__external_intrinsic(
	const sparse_t* src, const char* ptr,
	parse_keyword_e keyword,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, keyword);
	if (i == 0) return 0;

	stmt->external_intrinsic.count = 0;
	stmt->external_intrinsic.name = NULL;

	unsigned l = parse_list(src, &ptr[i], ',',
		&stmt->external_intrinsic.count,
		(void***)&stmt->external_intrinsic.name,
		(void*)parse_name_alloc,
		free);
	if (l == 0) return 0;
	i += l;

	return i;
}

unsigned parse_stmt_external(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__external_intrinsic(
		src, ptr, PARSE_KEYWORD_EXTERNAL, stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_EXTERNAL;
	return i;
}

unsigned parse_stmt_intrinsic(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__external_intrinsic(
		src, ptr, PARSE_KEYWORD_INTRINSIC, stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_INTRINSIC;
	return i;
}
