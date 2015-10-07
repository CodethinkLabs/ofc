#include "../parse.h"


static unsigned parse_stmt__decl_attr(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_keyword_e keyword,
	parse_stmt_t* stmt)
{
	unsigned dpos = parse_debug_position(debug);

	unsigned i = parse_keyword(
		src, ptr, debug, keyword);
	if (i == 0) return 0;

	stmt->decl_attr.count = 0;
	stmt->decl_attr.name = NULL;

	unsigned l = parse_list(
		src, &ptr[i], debug, ',',
		&stmt->decl_attr.count,
		(void***)&stmt->decl_attr.name,
		(void*)parse_name_alloc,
		free);
	if (l == 0)
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += l;

	return i;
}

unsigned parse_stmt_decl_attr_external(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__decl_attr(
		src, ptr, debug, PARSE_KEYWORD_EXTERNAL, stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_DECL_ATTR_EXTERNAL;
	return i;
}

unsigned parse_stmt_decl_attr_intrinsic(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__decl_attr(
		src, ptr, debug, PARSE_KEYWORD_INTRINSIC, stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_DECL_ATTR_INTRINSIC;
	return i;
}

unsigned parse_stmt_decl_attr_automatic(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__decl_attr(
		src, ptr, debug, PARSE_KEYWORD_AUTOMATIC, stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_DECL_ATTR_AUTOMATIC;
	return i;
}

unsigned parse_stmt_decl_attr_static(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__decl_attr(
		src, ptr, debug, PARSE_KEYWORD_STATIC, stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_DECL_ATTR_STATIC;
	return i;
}

unsigned parse_stmt_decl_attr_volatile(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__decl_attr(
		src, ptr, debug, PARSE_KEYWORD_VOLATILE, stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_DECL_ATTR_VOLATILE;
	return i;
}


bool parse_stmt_decl_attr_print(
	colstr_t* cs, const parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	const char* kwstr;
	switch (stmt->type)
	{
		case PARSE_STMT_DECL_ATTR_EXTERNAL:
			kwstr = "EXTERNAL";
			break;
		case PARSE_STMT_DECL_ATTR_INTRINSIC:
			kwstr = "INTRINSIC";
			break;
		case PARSE_STMT_DECL_ATTR_AUTOMATIC:
			kwstr = "AUTOMATIC";
			break;
		case PARSE_STMT_DECL_ATTR_STATIC:
			kwstr = "STATIC";
			break;
		case PARSE_STMT_DECL_ATTR_VOLATILE:
			kwstr = "VOLATILE";
			break;
		default:
			return false;
	}

	if (!colstr_atomic_writef(cs, "%s", kwstr))
		return false;

	unsigned i;
	for (i = 0; i < stmt->decl_attr.count; i++)
	{
		if (!colstr_atomic_writef(cs, "%s",
			(i == 0 ? " " : ", ")))
			return false;

		if (!str_ref_print(cs,
			*(stmt->decl_attr.name[i])))
			return false;
	}

	return true;
}
