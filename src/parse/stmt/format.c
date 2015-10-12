#include <ofc/parse.h>

unsigned ofc_parse_stmt_format(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = ofc_parse_keyword(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_FORMAT);
	if (i == 0) return 0;

	if (ptr[i++] != '(')
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}

	/* Empty FORMAT statements are valid as in nist/FM110.FOR */
	unsigned l;
	stmt->format = ofc_parse_format_desc_list(
		src, &ptr[i], debug, &l);
	if (stmt->format) i += l;

	if (ptr[i++] != ')')
	{
		ofc_parse_format_desc_list_delete(
			stmt->format);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}

	stmt->type = OFC_PARSE_STMT_FORMAT;
	return i;
}

bool ofc_parse_stmt_format_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	if (!ofc_colstr_atomic_writef(cs, "FORMAT("))
		return false;

	if (stmt->format
		&& !ofc_parse_format_desc_list_print(
			cs, stmt->format))
		return false;

	return ofc_colstr_atomic_writef(cs, ")");
}
