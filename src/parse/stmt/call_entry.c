#include "../parse.h"

unsigned ofc_parse_stmt_call(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = ofc_parse_keyword(
		src, ptr, debug, OFC_PARSE_KEYWORD_CALL);
	if (i == 0) return 0;

	unsigned len = ofc_parse_name(
		src, &ptr[i], debug,
		&stmt->call_entry.name);
	if (len == 0)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	stmt->call_entry.args = NULL;
	if (ptr[i] == '(')
	{
		i += 1;

		stmt->call_entry.args = ofc_parse_call_arg_list(
			src, &ptr[i], debug, &len);
		if (stmt->call_entry.args) i += len;

		if (ptr[i++] != ')')
		{
			ofc_parse_call_arg_list_delete(
				stmt->call_entry.args);
			ofc_parse_debug_rewind(debug, dpos);
			return 0;
		}
	}

	stmt->type = OFC_PARSE_STMT_CALL;
	return i;
}

unsigned ofc_parse_stmt_entry(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = ofc_parse_keyword(
		src, ptr, debug, OFC_PARSE_KEYWORD_ENTRY);
	if (i == 0) return 0;

	unsigned len = ofc_parse_name(
		src, &ptr[i], debug,
		&stmt->call_entry.name);
	if (len == 0)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	stmt->call_entry.args = NULL;
	if (ptr[i] == '(')
	{
		i += 1;

		stmt->call_entry.args = ofc_parse_call_arg_list(
			src, &ptr[i], debug, &len);
		if (stmt->call_entry.args) i += len;

		if (ptr[i++] != ')')
		{
			ofc_parse_call_arg_list_delete(
				stmt->call_entry.args);
			ofc_parse_debug_rewind(debug, dpos);
			return 0;
		}
	}

	stmt->type = OFC_PARSE_STMT_ENTRY;
	return i;
}

bool ofc_parse_stmt_call_entry_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	const char* kwstr;
	switch (stmt->type)
	{
		case OFC_PARSE_STMT_CALL:
			kwstr = "CALL";
			break;
		case OFC_PARSE_STMT_ENTRY:
			kwstr = "ENTRY";
			break;
		default:
			return false;
	}

	if (!ofc_colstr_atomic_writef(cs, "%s ", kwstr)
		|| !ofc_str_ref_print(cs, stmt->call_entry.name)
		|| !ofc_colstr_atomic_writef(cs, "("))
		return false;

	if (stmt->call_entry.args
		&& !ofc_parse_call_arg_list_print(
			cs, stmt->call_entry.args))
		return false;

	return ofc_colstr_atomic_writef(cs, ")");
}
