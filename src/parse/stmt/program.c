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

/* This is hacky, but it means we can suppress redeclarations of the same program. */
static str_ref_t parse_stmt_program__current = STR_REF_EMPTY;

unsigned parse_stmt_program(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	stmt->program.name = STR_REF_EMPTY;
	unsigned i = parse_keyword_named(
		src, ptr, PARSE_KEYWORD_PROGRAM, &stmt->program.name);
	if (i == 0) return 0;

	if (!str_ref_empty(stmt->program.name))
	{
		lang_opts_t opts = sparse_lang_opts(src);
		if (opts.case_sensitive
			? str_ref_equal(stmt->program.name, parse_stmt_program__current)
			: str_ref_equal_ci(stmt->program.name, parse_stmt_program__current))
		{
			stmt->type = PARSE_STMT_EMPTY;
			return i;
		}
	}

	unsigned len;
	if (!is_end_statement(&ptr[i], &len))
		return 0;
	i += len;

	str_ref_t prev_program_name = parse_stmt_program__current;
	parse_stmt_program__current = stmt->program.name;

	len = parse_stmt_program__body(
		src, &ptr[i], PARSE_KEYWORD_PROGRAM, stmt);
	if (len == 0) return 0;
	i += len;

	parse_stmt_program__current = prev_program_name;

	stmt->program.type = NULL;
	stmt->program.args = NULL;

	stmt->type = PARSE_STMT_PROGRAM;

	return i;
}


unsigned parse_stmt_subroutine(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	stmt->program.name = STR_REF_EMPTY;
	unsigned i = parse_keyword_named(
		src, ptr, PARSE_KEYWORD_SUBROUTINE, &stmt->program.name);
	if (i == 0) return 0;

	stmt->program.args = NULL;
	if (ptr[i] == '(')
	{
		i += 1;

		unsigned len;
		stmt->program.args = parse_call_arg_list(
			src, &ptr[i], &len);
		if (stmt->program.args) i += len;

		if (ptr[i++] != ')')
		{
			parse_call_arg_list_delete(stmt->program.args);
			return 0;
		}
	}

	unsigned len;
	if (!is_end_statement(&ptr[i], &len))
	{
		parse_call_arg_list_delete(stmt->program.args);
		return 0;
	}
	i += len;

	len = parse_stmt_program__body(
		src, &ptr[i], PARSE_KEYWORD_SUBROUTINE, stmt);
	if (len == 0)
	{
		parse_call_arg_list_delete(stmt->program.args);
		return 0;
	}
	i += len;

	stmt->program.type = NULL;

	stmt->type = PARSE_STMT_SUBROUTINE;
	return i;
}


unsigned parse_stmt_function(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = 0;
	stmt->program.type
		= parse_type(src, ptr, &i);

	stmt->program.name = STR_REF_EMPTY;
	unsigned len = parse_keyword_named(
		src, &ptr[i], PARSE_KEYWORD_FUNCTION,
		&stmt->program.name);
	if (len == 0)
	{
		parse_type_delete(stmt->program.type);
		return 0;
	}
	i += len;

	stmt->program.args = NULL;
	if (ptr[i] == '(')
	{
		i += 1;

		unsigned len;
		stmt->program.args = parse_call_arg_list(
			src, &ptr[i], &len);
		if (stmt->program.args) i += len;

		if (ptr[i++] != ')')
		{
			parse_call_arg_list_delete(stmt->program.args);
			return 0;
		}
	}

	if (!is_end_statement(&ptr[i], &len))
	{
		parse_type_delete(stmt->program.type);
		parse_call_arg_list_delete(stmt->program.args);
		return 0;
	}
	i += len;

	len = parse_stmt_program__body(
		src, &ptr[i], PARSE_KEYWORD_FUNCTION, stmt);
	if (len == 0)
	{
		parse_type_delete(stmt->program.type);
		parse_call_arg_list_delete(stmt->program.args);
		return 0;
	}
	i += len;

	stmt->type = PARSE_STMT_FUNCTION;
	return i;
}


/* This is hacky, but it means we can suppress redeclarations of the same block_data. */
static str_ref_t parse_stmt_block_data__current = STR_REF_EMPTY;

unsigned parse_stmt_block_data(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	stmt->program.name = STR_REF_EMPTY;
	unsigned i = parse_keyword_named(
		src, ptr, PARSE_KEYWORD_BLOCK_DATA, &stmt->program.name);
	if (i == 0) return 0;

	if (!str_ref_empty(stmt->program.name))
	{
		lang_opts_t opts = sparse_lang_opts(src);
		if (opts.case_sensitive
			? str_ref_equal(stmt->program.name, parse_stmt_block_data__current)
			: str_ref_equal_ci(stmt->program.name, parse_stmt_block_data__current))
		{
			stmt->type = PARSE_STMT_EMPTY;
			return i;
		}
	}

	unsigned len;
	if (!is_end_statement(&ptr[i], &len))
		return 0;
	i += len;

	str_ref_t prev_block_data_name = parse_stmt_block_data__current;
	parse_stmt_block_data__current = stmt->program.name;

	len = parse_stmt_program__body(
		src, &ptr[i], PARSE_KEYWORD_BLOCK_DATA, stmt);
	if (len == 0) return 0;
	i += len;

	parse_stmt_block_data__current = prev_block_data_name;

	stmt->program.type = NULL;
	stmt->program.args = NULL;

	stmt->type = PARSE_STMT_BLOCK_DATA;

	return i;
}



bool parse_stmt_program_print(
	int fd, const parse_stmt_t* stmt, unsigned indent)
{
	if (!stmt) return false;

	switch (stmt->type)
	{
		case PARSE_STMT_PROGRAM:
		case PARSE_STMT_SUBROUTINE:
		case PARSE_STMT_FUNCTION:
		case PARSE_STMT_BLOCK_DATA:
			break;
		default:
			return false;
	}

	if (stmt->program.type)
	{
		if (!parse_type_print(
			fd, stmt->program.type)
			|| !dprintf_bool(fd, " "))
			return false;
	}

	const char* kwstr;
	bool has_args = false;
	switch (stmt->type)
	{
		case PARSE_STMT_PROGRAM:
			kwstr = "PROGRAM";
			break;
		case PARSE_STMT_SUBROUTINE:
			kwstr = "SUBROUTINE";
			has_args = true;
			break;
		case PARSE_STMT_FUNCTION:
			kwstr = "FUNCTION";
			has_args = true;
			break;
		case PARSE_STMT_BLOCK_DATA:
			kwstr = "BLOCK DATA";
			break;
		default:
			return false;
	}

	if (!dprintf_bool(fd, "%s", kwstr))
				return false;

    if (!str_ref_empty(stmt->program.name))
	{
		if (!dprintf_bool(fd, " ")
			|| !str_ref_print(fd, stmt->program.name))
			return false;
	}

	if (has_args)
	{
		if (!dprintf_bool(fd, "("))
			return false;

		if (stmt->program.args
			&& !parse_call_arg_list_print(
				fd, stmt->program.args))
			return false;

		if (!dprintf_bool(fd, ")"))
			return false;
	}

	if (!dprintf_bool(fd, "\n"))
		return false;

	if (stmt->program.body
		&& !parse_stmt_list_print(
			fd, stmt->program.body, (indent + 1)))
		return false;

	if (!dprintf_bool(fd, "      "))
			return false;
	unsigned i;
	for (i = 0; i < indent; i++)
	{
		if (!dprintf_bool(fd, "  "))
			return false;
	}

	if (!dprintf_bool(fd, "END %s", kwstr))
				return false;

	if (!str_ref_empty(stmt->program.name))
	{
		if (!dprintf_bool(fd, " ")
			|| !str_ref_print(fd, stmt->program.name))
			return false;
	}

	return true;
}
