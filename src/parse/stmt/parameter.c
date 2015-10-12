#include "../parse.h"


unsigned ofc_parse_stmt_parameter(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = ofc_parse_keyword(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_PARAMETER);
	if (i == 0) return 0;

	bool has_brackets = (ptr[i] == '(');
	if (has_brackets) i += 1;

	unsigned l;
	stmt->parameter.list = ofc_parse_assign_list(
		src, &ptr[i], debug, &l);
	if (!stmt->parameter.list)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += l;

	if (has_brackets)
	{
		if (ptr[i++] != ')')
		{
			ofc_parse_assign_list_delete(
				stmt->parameter.list);
			ofc_parse_debug_rewind(debug, dpos);
			return 0;
		}
	}

	stmt->type = OFC_PARSE_STMT_PARAMETER;
	return i;
}

bool ofc_parse_stmt_parameter_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
    if (!stmt)
		return false;

	return (ofc_colstr_atomic_writef(cs, "PARAMETER")
		&& ofc_colstr_atomic_writef(cs, " ")
		&& ofc_colstr_atomic_writef(cs, "(")
		&& ofc_parse_assign_list_print(
			cs, stmt->parameter.list)
		&& ofc_colstr_atomic_writef(cs, ")"));
}
