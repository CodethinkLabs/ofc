#include "../parse.h"


unsigned parse_stmt_decl(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i;
	stmt->decl.type = parse_type(
		src, ptr, &i);
	if (!stmt->decl.type)
		return 0;

	if ((ptr[i + 0] == ':')
		&& (ptr[i + 1] == ':'))
		i += 2;

	unsigned l;
	stmt->decl.decl = parse_decl_list(
		src, &ptr[i], &l);
	if (!stmt->decl.decl)
	{
		parse_type_delete(stmt->decl.type);
		return 0;
	}
	i += l;

	stmt->type = PARSE_STMT_DECL;
	return i;
}
