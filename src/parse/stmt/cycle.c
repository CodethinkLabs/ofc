#include <ofc/parse.h>

unsigned ofc_parse_stmt_cycle(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned i = ofc_parse_keyword(
		src, ptr, debug, OFC_PARSE_KEYWORD_CYCLE);
	if (i == 0) return 0;

	stmt->type = OFC_PARSE_STMT_CYCLE;
	return i;
}

bool ofc_parse_stmt_cycle_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	return ofc_colstr_atomic_writef(cs, "CYCLE");
}
