#include "parse.h"
#include <string.h>
#include <ctype.h>


/* TODO - Replace decl list with hash table for lookup speed. */
static bool parse_program_add_decl(
	parse_program_t* program, parse_decl_t decl)
{
	if (!program)
		return false;

	parse_decl_t* ndecl
		= (parse_decl_t*)realloc(program->decl,
			(sizeof(parse_decl_t) * (program->decl_count + 1)));
	if (!ndecl) return false;

	program->decl = ndecl;
	program->decl[program->decl_count++] = decl;
	return true;
}

static bool parse_program_add_stmt(
	parse_program_t* program, parse_stmt_t stmt)
{
	if (!program)
		return false;

	parse_stmt_t* nstmt
		= (parse_stmt_t*)realloc(program->stmt,
			(sizeof(parse_stmt_t) * (program->stmt_count + 1)));
	if (!nstmt) return false;

	program->stmt = nstmt;
	program->stmt[program->stmt_count++] = stmt;
	return true;
}

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

	program->implicit = PARSE_IMPLICIT_DEFAULT;

	program->decl_count = 0;
	program->decl       = NULL;

	program->stmt_count = 0;
	program->stmt       = NULL;

	unsigned len;
	while (true)
	{
		len = parse_implicit(
			src, &ptr[i], &program->implicit);
		i += len;
		if (len > 0) continue;

		/* TODO - Disambiguate assign statements and declarations. */

		{
			parse_stmt_t stmt;
			len = parse_stmt(
				src, &ptr[i], &stmt);
			i += len;

			if (len > 0)
			{
				if (!parse_program_add_stmt(program, stmt))
				{
					/* This should never happen, likely out of memory. */
					return 0;
				}
				continue;
			}
		}

		{
			parse_decl_t decl;
			len = parse_decl(
				src, &ptr[i],
				&program->implicit,
				&decl);
			i += len;

			if (len > 0)
			{
				if (!parse_program_add_decl(program, decl))
				{
					/* This should never happen, likely out of memory. */
					return 0;
				}
				continue;
			}
		}

		break;
	}

	str_ref_t end_name = STR_REF_EMPTY;
	len = parse_keyword_name(
		src, &ptr[i], PARSE_KEYWORD_END_PROGRAM, &end_name);
	if (len == 0)
	{
		sparse_error(src, &ptr[i],
			"Expected END PROGRAM");
		return 0;
	}

	/* TODO - Compare cases depending on lang opts. */
	if (!str_ref_empty(end_name)
		&& !str_ref_equal(program->name, end_name))
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
			"Expected end of input after main program.\n");
		return 0;
	}

	return i;
}
