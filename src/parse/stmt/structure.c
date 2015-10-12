#include "../parse.h"


static unsigned ofc_parse_stmt__structure(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_keyword_e keyword,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = ofc_parse_keyword(
		src, ptr, debug, keyword);
	if (i == 0) return 0;

	stmt->structure.name = OFC_STR_REF_EMPTY;

	if (keyword == OFC_PARSE_KEYWORD_STRUCTURE)
	{
		if (ptr[i] == '/')
		{
			i += 1;

			unsigned len = ofc_parse_name(
				src, &ptr[i], debug, &stmt->structure.name);
			if (len == 0)
			{
				ofc_parse_debug_rewind(debug, dpos);
				return 0;
			}
			i += len;

			if (ptr[i++] != '/')
			{
				ofc_parse_debug_rewind(debug, dpos);
				return 0;
			}
		}

		/* TODO - Parse field list. */
	}

	unsigned len;
	if (!ofc_is_end_statement(&ptr[i], &len))
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	stmt->structure.block = ofc_parse_stmt_list(
		src, &ptr[i], debug, &len);
	if (stmt->structure.block) i += len;

	len = ofc_parse_keyword_end(
		src, &ptr[i], debug, keyword);
	if (len == 0)
	{
		ofc_parse_stmt_list_delete(
			stmt->structure.block);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	stmt->type = OFC_PARSE_STMT_STRUCTURE;
	return i;
}


unsigned ofc_parse_stmt_structure(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned i = ofc_parse_stmt__structure(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_STRUCTURE, stmt);
	if (i == 0) return 0;

	stmt->type = OFC_PARSE_STMT_STRUCTURE;
	return i;
}

unsigned ofc_parse_stmt_union(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned i = ofc_parse_stmt__structure(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_UNION, stmt);
	if (i == 0) return 0;

	stmt->type = OFC_PARSE_STMT_UNION;
	return i;
}

unsigned ofc_parse_stmt_map(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned i = ofc_parse_stmt__structure(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_MAP, stmt);
	if (i == 0) return 0;

	stmt->type = OFC_PARSE_STMT_MAP;
	return i;
}

unsigned ofc_parse_stmt_record(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = ofc_parse_keyword(
		src, ptr, debug, OFC_PARSE_KEYWORD_RECORD);
	if (i == 0) return 0;

	unsigned l;
	stmt->record = ofc_parse_record_list(
		src, &ptr[i], debug, &l);
	if (l == 0)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += l;

	stmt->type = OFC_PARSE_STMT_RECORD;
	return i;
}




bool ofc_parse_stmt_structure_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt, unsigned indent)
{
	if (!stmt)
		return false;

	const char* kwstr;
	switch (stmt->type)
	{
		case OFC_PARSE_STMT_STRUCTURE:
			kwstr = "STRUCTURE";
			break;
		case OFC_PARSE_STMT_UNION:
			kwstr = "UNION";
			break;
		case OFC_PARSE_STMT_MAP:
			kwstr = "MAP";
			break;
		default:
			return false;
	}

	if (!ofc_colstr_atomic_writef(cs, "%s", kwstr))
		return false;

	if (!ofc_str_ref_empty(stmt->structure.name))
	{
		if (!ofc_colstr_atomic_writef(cs, " /")
			|| !ofc_str_ref_print(cs, stmt->structure.name)
			|| !ofc_colstr_atomic_writef(cs, "/"))
			return false;
	}

	if (!ofc_parse_stmt_list_print(
		cs, stmt->structure.block, (indent + 1)))
		return false;

	if (!ofc_colstr_newline(cs, NULL))
		return false;

	unsigned j;
	for (j = 0; j < indent; j++)
	{
		if (!ofc_colstr_atomic_writef(cs, "  "))
			return false;
	}

	return ofc_colstr_atomic_writef(cs, "END %s", kwstr);
}

bool ofc_parse_stmt_record_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	return (ofc_colstr_atomic_writef(cs, "RECORD ")
		&& ofc_parse_record_list_print(cs, stmt->record));
}
