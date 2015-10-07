#include "../parse.h"


static unsigned parse_stmt__common_namelist(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_keyword_e keyword,
	parse_stmt_t* stmt)
{
	unsigned dpos = parse_debug_position(debug);

	unsigned i = parse_keyword(
		src, ptr, debug, keyword);
	if (i == 0) return 0;

	unsigned l;
	stmt->common_namelist
		= parse_common_group_list(
			src, &ptr[i], debug, &l);
	if (!stmt->common_namelist)
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += l;

	return i;
}

unsigned parse_stmt_common(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__common_namelist(
		src, ptr, debug, PARSE_KEYWORD_COMMON, stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_COMMON;
	return i;
}

unsigned parse_stmt_namelist(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__common_namelist(
		src, ptr, debug, PARSE_KEYWORD_NAMELIST, stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_NAMELIST;
	return i;
}

bool parse_stmt_common_namelist_print(
	colstr_t* cs, const parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	const char* kwstr;
	switch (stmt->type)
	{
		case PARSE_STMT_COMMON:
			kwstr = "COMMON";
			break;
		case PARSE_STMT_NAMELIST:
			kwstr = "NAMELIST";
			break;
		default:
			return false;
	}

	return (stmt && colstr_atomic_writef(cs, "%s  ", kwstr)
		&& parse_common_group_list_print(
			cs, stmt->common_namelist));
}
