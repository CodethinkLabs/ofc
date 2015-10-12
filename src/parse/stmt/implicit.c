#include <ofc/parse.h>



unsigned ofc_parse_stmt_implicit(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned i = ofc_parse_keyword(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_IMPLICIT_NONE);
	if (i > 0)
	{
		stmt->type = OFC_PARSE_STMT_IMPLICIT_NONE;
		return i;
	}

	unsigned dpos = ofc_parse_debug_position(debug);

	i = ofc_parse_keyword(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_IMPLICIT);
	if (i == 0) return 0;

	stmt->type = OFC_PARSE_STMT_IMPLICIT;
	unsigned len;
	stmt->implicit = ofc_parse_implicit_list(
		src, &ptr[i], debug, &len);
	if (!stmt->implicit)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	return i;
}

bool ofc_parse_stmt_implicit_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	if (stmt->type == OFC_PARSE_STMT_IMPLICIT_NONE)
		return ofc_colstr_atomic_writef(cs, "IMPLICIT NONE");

	return (ofc_colstr_atomic_writef(cs, "IMPLICIT ")
		&& ofc_parse_implicit_list_print(cs, stmt->implicit));
}
