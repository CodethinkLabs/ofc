#include "../parse.h"


static unsigned parse_stmt__structure(
	const sparse_t* src, const char* ptr,
	parse_keyword_e keyword,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, keyword);
	if (i == 0) return 0;

	stmt->structure.name = STR_REF_EMPTY;

	if (keyword == PARSE_KEYWORD_STRUCTURE)
	{
		if (ptr[i] == '/')
		{
			i += 1;

			unsigned len = parse_name(
				src, &ptr[i], &stmt->structure.name);
			if (len == 0) return 0;
			i += len;

			if (ptr[i++] != '/')
				return 0;
		}

		/* TODO - Parse field list. */
	}

	unsigned len;
	if (!is_end_statement(&ptr[i], &len))
		return 0;
	i += len;

	stmt->structure.block
		= parse_stmt_list(src, &ptr[i], &len);
	if (stmt->structure.block) i += len;

	len = parse_keyword_end(
		src, &ptr[i], keyword);
	if (len == 0)
	{
		parse_stmt_list_delete(
			stmt->structure.block);
		return 0;
	}
	i += len;

	stmt->type = PARSE_STMT_STRUCTURE;
	return i;
}


unsigned parse_stmt_structure(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__structure(
		src, ptr, PARSE_KEYWORD_STRUCTURE, stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_STRUCTURE;
	return i;
}

unsigned parse_stmt_union(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__structure(
		src, ptr, PARSE_KEYWORD_UNION, stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_UNION;
	return i;
}

unsigned parse_stmt_map(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__structure(
		src, ptr, PARSE_KEYWORD_MAP, stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_MAP;
	return i;
}

unsigned parse_stmt_record(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, PARSE_KEYWORD_RECORD);
	if (i == 0) return 0;

	unsigned l;
	stmt->record = parse_record_list(
		src, &ptr[i], &l);
	if (l == 0) return 0;
	i += l;

	stmt->type = PARSE_STMT_RECORD;
	return i;
}
