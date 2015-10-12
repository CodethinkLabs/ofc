#include <ofc/parse.h>


unsigned ofc_parse_stmt_assign(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = ofc_parse_keyword(
		src, ptr, debug, OFC_PARSE_KEYWORD_ASSIGN);
	if (i == 0) return 0;

	unsigned len = ofc_parse_unsigned(
		src, &ptr[i], debug, &stmt->assign.label);
	if (len == 0)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	len = ofc_parse_keyword(
		src, &ptr[i], debug, OFC_PARSE_KEYWORD_TO);
	if (len == 0)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += 2;

	len = ofc_parse_name(
		src, &ptr[i], debug,
		&stmt->assign.variable);
	if (len == 0)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	stmt->type = OFC_PARSE_STMT_ASSIGN;
	return i;
}

bool ofc_parse_stmt_assign_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	return (ofc_colstr_atomic_writef(cs, "ASSIGN %u TO ", stmt->assign.label)
		&& ofc_str_ref_print(cs, stmt->assign.variable));
}
