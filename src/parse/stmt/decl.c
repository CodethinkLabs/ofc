#include "../parse.h"


unsigned ofc_parse_stmt_decl(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i;
	stmt->decl.type = ofc_parse_type(
		src, ptr, debug, &i);
	if (!stmt->decl.type)
		return 0;

	if ((ptr[i + 0] == ':')
		&& (ptr[i + 1] == ':'))
		i += 2;

	unsigned l;
	stmt->decl.decl = ofc_parse_decl_list(
		src, &ptr[i], debug, &l);
	if (!stmt->decl.decl)
	{
		ofc_parse_type_delete(stmt->decl.type);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += l;

	stmt->type = OFC_PARSE_STMT_DECL;
	return i;
}

bool ofc_parse_stmt_decl_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
	return (ofc_parse_type_print(cs, stmt->decl.type, true)
		&& ofc_colstr_atomic_writef(cs, " ")
		&& ofc_parse_decl_list_print(cs, stmt->decl.decl));
}
