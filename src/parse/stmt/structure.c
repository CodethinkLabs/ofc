#include "../parse.h"


static unsigned parse_stmt__structure(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_keyword_e keyword,
	parse_stmt_t* stmt)
{
	unsigned dpos = parse_debug_position(debug);

	unsigned i = parse_keyword(
		src, ptr, debug, keyword);
	if (i == 0) return 0;

	stmt->structure.name = STR_REF_EMPTY;

	if (keyword == PARSE_KEYWORD_STRUCTURE)
	{
		if (ptr[i] == '/')
		{
			i += 1;

			unsigned len = parse_name(
				src, &ptr[i], debug, &stmt->structure.name);
			if (len == 0)
			{
				parse_debug_rewind(debug, dpos);
				return 0;
			}
			i += len;

			if (ptr[i++] != '/')
			{
				parse_debug_rewind(debug, dpos);
				return 0;
			}
		}

		/* TODO - Parse field list. */
	}

	unsigned len;
	if (!is_end_statement(&ptr[i], &len))
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	stmt->structure.block = parse_stmt_list(
		src, &ptr[i], debug, &len);
	if (stmt->structure.block) i += len;

	len = parse_keyword_end(
		src, &ptr[i], debug, keyword);
	if (len == 0)
	{
		parse_stmt_list_delete(
			stmt->structure.block);
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	stmt->type = PARSE_STMT_STRUCTURE;
	return i;
}


unsigned parse_stmt_structure(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__structure(
		src, ptr, debug,
		PARSE_KEYWORD_STRUCTURE, stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_STRUCTURE;
	return i;
}

unsigned parse_stmt_union(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__structure(
		src, ptr, debug,
		PARSE_KEYWORD_UNION, stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_UNION;
	return i;
}

unsigned parse_stmt_map(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__structure(
		src, ptr, debug,
		PARSE_KEYWORD_MAP, stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_MAP;
	return i;
}

unsigned parse_stmt_record(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned dpos = parse_debug_position(debug);

	unsigned i = parse_keyword(
		src, ptr, debug, PARSE_KEYWORD_RECORD);
	if (i == 0) return 0;

	unsigned l;
	stmt->record = parse_record_list(
		src, &ptr[i], debug, &l);
	if (l == 0)
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += l;

	stmt->type = PARSE_STMT_RECORD;
	return i;
}




bool parse_stmt_structure_print(
	string_t* tree_output, const parse_stmt_t* stmt, unsigned indent)
{
	if (!stmt)
		return false;

	const char* kwstr;
	switch (stmt->type)
	{
		case PARSE_STMT_STRUCTURE:
			kwstr = "STRUCTURE";
			break;
		case PARSE_STMT_UNION:
			kwstr = "UNION";
			break;
		case PARSE_STMT_MAP:
			kwstr = "MAP";
			break;
		default:
			return false;
	}

	if (!string_printf(tree_output, "%s", kwstr))
		return false;

	if (!str_ref_empty(stmt->structure.name))
	{
		if (!string_printf(tree_output, " /")
			|| !str_ref_print(tree_output, stmt->structure.name)
			|| !string_printf(tree_output, "/"))
			return false;
	}

	if (!string_printf(tree_output, "\n"))
		return false;

	if (!parse_stmt_list_print(
		tree_output, stmt->structure.block, (indent + 1)))
		return false;

	if (!string_printf(tree_output, "      "))
		return false;

	unsigned j;
	for (j = 0; j < indent; j++)
	{
		if (!string_printf(tree_output, "  "))
			return false;
	}

	return string_printf(tree_output, "END %s", kwstr);
}

bool parse_stmt_record_print(
	string_t* tree_output, const parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	return (string_printf(tree_output, "RECORD ")
		&& parse_record_list_print(tree_output, stmt->record));
}
