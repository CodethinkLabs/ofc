#include "../parse.h"


unsigned parse_stmt_program__body(
	const sparse_t* src, const char* ptr,
	parse_keyword_e keyword,
	parse_stmt_t* stmt)
{
	unsigned i = 0;
	stmt->program.body = parse_stmt_list(src, ptr, &i);

	unsigned len = parse_keyword_end_named(
		src, &ptr[i], PARSE_KEYWORD_PROGRAM, &stmt->program.name);
	if (len == 0)
	{
		sparse_error(src, &ptr[i],
			"Invalid statement in %s body",
			parse_keyword_name(keyword));
		parse_stmt_list_delete(stmt->program.body);
		return 0;
	}
	i += len;

	if (!stmt->program.body)
	{
		sparse_warning(src, &ptr[i],
			"Empty program body");
	}

	return i;
}

unsigned parse_stmt_program(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	stmt->program.name = STR_REF_EMPTY;
	unsigned i = parse_keyword_named(
		src, ptr, PARSE_KEYWORD_PROGRAM, &stmt->program.name);
	if (i == 0) return 0;

	if (str_ref_empty(stmt->program.name))
	{
		sparse_error(src, ptr, "Expected name in PROGRAM statement");
		return 0;
	}

	unsigned len;
	if (!is_end_statement(&ptr[i], &len))
		return 0;
	i += len;

	len = parse_stmt_program__body(
		src, &ptr[i], PARSE_KEYWORD_PROGRAM, stmt);
	if (len == 0) return 0;
	i += len;

	stmt->program.type = NULL;
	stmt->program.args = NULL;

	stmt->type = PARSE_STMT_PROGRAM;
	return i;
}
