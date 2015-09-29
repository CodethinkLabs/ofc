#include "../parse.h"


unsigned parse_stmt_decl(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned dpos = parse_debug_position(debug);

	unsigned i;
	stmt->decl.type = parse_type(
		src, ptr, debug, &i);
	if (!stmt->decl.type)
		return 0;

	if ((ptr[i + 0] == ':')
		&& (ptr[i + 1] == ':'))
		i += 2;

	unsigned l;
	stmt->decl.decl = parse_decl_list(
		src, &ptr[i], debug, &l);
	if (!stmt->decl.decl)
	{
		parse_type_delete(stmt->decl.type);
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += l;

	stmt->type = PARSE_STMT_DECL;
	return i;
}

bool parse_stmt_decl_print(
	int fd, const parse_stmt_t* stmt)
{
	return (parse_type_print(fd, stmt->decl.type)
		&& dprintf_bool(fd, " ")
		&& parse_decl_list_print(fd, stmt->decl.decl));
}
