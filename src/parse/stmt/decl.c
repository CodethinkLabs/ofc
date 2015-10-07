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
	colstr_t* cs, const parse_stmt_t* stmt)
{
	return (parse_type_print(cs, stmt->decl.type)
		&& colstr_atomic_writef(cs, " ")
		&& parse_decl_list_print(cs, stmt->decl.decl));
}
