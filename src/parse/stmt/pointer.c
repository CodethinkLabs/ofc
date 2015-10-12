#include "../parse.h"


unsigned ofc_parse_stmt_pointer(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = ofc_parse_keyword(
		src, ptr, debug, OFC_PARSE_KEYWORD_POINTER);
	if (i == 0) return 0;

	unsigned l;
	stmt->pointer = ofc_parse_pointer_list(
		src, &ptr[i], debug, &l);
	if (!stmt->pointer)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += l;

	stmt->type = OFC_PARSE_STMT_POINTER;
	return i;
}

bool ofc_parse_stmt_pointer_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
	return (stmt && ofc_colstr_atomic_writef(cs, "POINTER ")
		&& ofc_parse_pointer_list_print(
			cs, stmt->pointer));
}
