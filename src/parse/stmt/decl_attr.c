#include "../parse.h"


static unsigned parse_stmt__decl_attr(
	const sparse_t* src, const char* ptr,
	parse_keyword_e keyword,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, keyword);
	if (i == 0) return 0;

	stmt->decl_attr.count = 0;
	stmt->decl_attr.name = NULL;

	unsigned l = parse_list(src, &ptr[i], ',',
		&stmt->decl_attr.count,
		(void***)&stmt->decl_attr.name,
		(void*)parse_name_alloc,
		free);
	if (l == 0) return 0;
	i += l;

	return i;
}

unsigned parse_stmt_decl_attr_external(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__decl_attr(
		src, ptr, PARSE_KEYWORD_EXTERNAL, stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_DECL_ATTR_EXTERNAL;
	return i;
}

unsigned parse_stmt_decl_attr_intrinsic(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__decl_attr(
		src, ptr, PARSE_KEYWORD_INTRINSIC, stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_DECL_ATTR_INTRINSIC;
	return i;
}

unsigned parse_stmt_decl_attr_automatic(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__decl_attr(
		src, ptr, PARSE_KEYWORD_AUTOMATIC, stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_DECL_ATTR_AUTOMATIC;
	return i;
}

unsigned parse_stmt_decl_attr_static(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__decl_attr(
		src, ptr, PARSE_KEYWORD_STATIC, stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_DECL_ATTR_STATIC;
	return i;
}

unsigned parse_stmt_decl_attr_volatile(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__decl_attr(
		src, ptr, PARSE_KEYWORD_VOLATILE, stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_DECL_ATTR_VOLATILE;
	return i;
}
