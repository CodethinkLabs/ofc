#include <ofc/parse.h>


static unsigned ofc_parse_stmt__dimension_virtual(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_keyword_e keyword,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = ofc_parse_keyword(
		src, ptr, debug, keyword);
	if (i == 0) return 0;

	unsigned len;
	stmt->dimension = ofc_parse_lhs_list(
		src, &ptr[i], debug, &len);
	if (!stmt->dimension)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	stmt->type = OFC_PARSE_STMT_DIMENSION;
	return i;
}

unsigned ofc_parse_stmt_dimension(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	return ofc_parse_stmt__dimension_virtual(
		src, ptr, debug, OFC_PARSE_KEYWORD_DIMENSION, stmt);
}

/* http://docs.oracle.com/cd/E19957-01/805-4939/6j4m0vnbo/index.html */
unsigned ofc_parse_stmt_virtual(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	return ofc_parse_stmt__dimension_virtual(
		src, ptr, debug, OFC_PARSE_KEYWORD_VIRTUAL, stmt);
}

bool ofc_parse_stmt_dimension_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	return (ofc_colstr_atomic_writef(cs, "DIMENSION ")
		&& ofc_parse_lhs_list_print(cs, stmt->dimension, true));
}
