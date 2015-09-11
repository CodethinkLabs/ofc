#include "parse.h"
#include <string.h>
#include <ctype.h>



unsigned parse_program(
	const sparse_t* src, const char* ptr,
	parse_program_t* program)
{
	str_ref_t name = STR_REF_EMPTY;
	unsigned i = parse_keyword_name(
		src, ptr, PARSE_KEYWORD_PROGRAM, &name);
	if (i == 0) return 0;

	if (str_ref_empty(name))
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

	/* TODO - Parse implicit/declarations */

	/* TODO - Parse execution-part */

	/* TODO - Parse internal-subprogram-part */

	str_ref_t end_name = STR_REF_EMPTY;
	unsigned len = parse_keyword_name(
		src, &ptr[i], PARSE_KEYWORD_END_PROGRAM, &end_name);
	if (len == 0)
	{
		sparse_error(src, &ptr[i],
			"Expected end of program");
		return 0;
	}

	/* TODO - Compare cases depending on lang opts. */
	if (!str_ref_empty(end_name)
		&& !str_ref_equal(name, end_name))
	{
		sparse_warning(src, &ptr[i],
			"END PROGRAM name doesn't match PROGRAM name");
	}
	i += len;

	if (ptr[i] != '\0')
	{
		sparse_error(src, &ptr[i],
			"Expected end of input after main program.\n");
		return 0;
	}

	program->name     = name;
	program->implicit = PARSE_IMPLICIT_DEFAULT;
	return i;
}
