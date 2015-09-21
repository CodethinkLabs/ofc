#include "parse.h"
#include <string.h>
#include <ctype.h>


unsigned parse_program(
	const sparse_t* src, const char* ptr,
	parse_program_t* program)
{
	program->name = STR_REF_EMPTY;
	unsigned i = parse_keyword_name(
		src, ptr, PARSE_KEYWORD_PROGRAM, &program->name);
	if (i == 0) return 0;

	if (str_ref_empty(program->name))
	{
		sparse_error(src, ptr, "Expected name in PROGRAM statement");
		return 0;
	}

	if ((ptr[i] != '\n')
		&& (ptr[i] != '\r'))
	{
		sparse_error(src, &ptr[i],
			"Expected newline after PROGRAM statement");
		return 0;
	}
	i += 1;

	unsigned len = 0;
	program->body = parse_stmt_list(src, &ptr[i], &len);
	i += len;

	str_ref_t end_name = STR_REF_EMPTY;
	len = parse_keyword_name(
		src, &ptr[i], PARSE_KEYWORD_END_PROGRAM, &end_name);
	if (len == 0)
	{
		sparse_error(src, &ptr[i],
			"Expected END PROGRAM");
		parse_program_cleanup(*program);
		return 0;
	}

	if (!program->body)
	{
		sparse_warning(src, &ptr[i],
			"Empty program body");
	}

	if (!str_ref_empty(end_name)
		&& (sparse_lang_opts(src).case_sensitive
			? !str_ref_equal(program->name, end_name)
			: !str_ref_equal_ci(program->name, end_name)))
	{
		sparse_warning(src, &ptr[i],
			"END PROGRAM name '%.*s' doesn't match PROGRAM name '%.*s'",
			end_name.size, end_name.base,
			program->name.size, program->name.base);
	}
	i += len;

	if (ptr[i] != '\0')
	{
		sparse_error(src, &ptr[i],
			"Expected end of input after main program");
		parse_program_cleanup(*program);
		return 0;
	}

	return i;
}

void parse_program_cleanup(
	parse_program_t program)
{
	parse_stmt_list_delete(program.body);
}
