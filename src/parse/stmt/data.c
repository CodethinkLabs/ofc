#include "../parse.h"



unsigned ofc_parse_stmt_data(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = ofc_parse_keyword(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_DATA);
	if (i == 0) return 0;

	unsigned l;
	stmt->data = ofc_parse_data_list(
		src, &ptr[i], debug, &l);
	if (!stmt->data)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += l;

	stmt->type = OFC_PARSE_STMT_DATA;
	return i;
}

bool ofc_parse_stmt_data_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
	return (stmt && ofc_colstr_atomic_writef(cs, "DATA ")
		&& ofc_parse_data_list_print(cs, stmt->data));
}
