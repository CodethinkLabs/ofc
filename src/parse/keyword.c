#include "parse.h"
#include <string.h>
#include <ctype.h>


unsigned parse_name(
	const sparse_t* src, const char* ptr)
{
	if (!isalpha(ptr[0]))
		return 0;

	unsigned i;
	for (i = 1; isalnum(ptr[i]) || (ptr[i] == '_'); i++);

	if (!sparse_sequential(src, ptr, i))
	{
		sparse_warning(src, ptr,
			"Unexpected whitespace in name");
	}

	return i;
}

unsigned parse_keyword_name(
	const sparse_t* src, const char* ptr,
	parse_keyword_e keyword,
	str_ref_t* name)
{
	unsigned expect_space = 0;
	int      match = -1;
	unsigned len = 0;

	switch (keyword)
	{
		case PARSE_KEYWORD_PROGRAM:
			len = 7;
			match = strncasecmp(ptr, "PROGRAM", len);
			break;
		case PARSE_KEYWORD_END_PROGRAM:
			len = 10;
			match = strncasecmp(ptr, "ENDPROGRAM", len);
			expect_space = 3;
			break;
		case PARSE_KEYWORD_END:
			len = 3;
			match = strncasecmp(ptr, "END", len);
			break;
		case PARSE_KEYWORD_IF:
			len = 2;
			match = strncasecmp(ptr, "IF", len);
			break;
		case PARSE_KEYWORD_GO_TO:
			len = 4;
			match = strncasecmp(ptr, "GOTO", len);
			expect_space = 2;
			break;
		case PARSE_KEYWORD_CONTINUE:
			len = 8;
			match = strncasecmp(ptr, "CONTINUE", len);
			break;
		case PARSE_KEYWORD_STOP:
			len = 4;
			match = strncasecmp(ptr, "STOP", len);
			break;

		case PARSE_KEYWORD_LOGICAL:
			len = 7;
			match = strncasecmp(ptr, "LOGICAL", len);
			break;
		case PARSE_KEYWORD_CHARACTER:
			len = 9;
			match = strncasecmp(ptr, "CHARACTER", len);
			break;
		case PARSE_KEYWORD_INTEGER:
			len = 7;
			match = strncasecmp(ptr, "INTEGER", len);
			break;
		case PARSE_KEYWORD_REAL:
			len = 4;
			match = strncasecmp(ptr, "REAL", len);
			break;
		case PARSE_KEYWORD_COMPLEX:
			len = 7;
			match = strncasecmp(ptr, "COMPLEX", len);
			break;
		case PARSE_KEYWORD_BYTE:
			len = 4;
			match = strncasecmp(ptr, "BYTE", len);
			break;
		case PARSE_KEYWORD_DOUBLE_PRECISION:
			len = 15;
			match = strncasecmp(ptr, "DOUBLEPRECISION", len);
			expect_space = 6;
			break;
		case PARSE_KEYWORD_DOUBLE_COMPLEX:
			len = 13;
			match = strncasecmp(ptr, "DOUBLECOMPLEX", len);
			expect_space = 6;
			break;

		case PARSE_KEYWORD_IMPLICIT:
			len = 8;
			match = strncasecmp(ptr, "IMPLICIT", len);
			break;
		case PARSE_KEYWORD_IMPLICIT_NONE:
			len = 12;
			match = strncasecmp(ptr, "IMPLICITNONE", len);
			expect_space = 8;
			break;

		case PARSE_KEYWORD_WRITE:
			len = 5;
			match = strncasecmp(ptr, "WRITE", len);
			break;
		case PARSE_KEYWORD_FORMAT:
			len = 6;
			match = strncasecmp(ptr, "FORMAT", len);
			break;
		default:
			/* Unknown keyword. */
			return 0;
	}

	if (match != 0)
	{
		switch (keyword)
		{
			case PARSE_KEYWORD_END_PROGRAM:
				return parse_keyword_name(
					src, ptr, PARSE_KEYWORD_END, name);
			default:
				break;
		}
		return 0;
	}

	bool entirely_sequential
		= sparse_sequential(src, ptr, len);

	bool unexpected_space = !entirely_sequential;
	if (expect_space > 0)
	{
		unsigned remain = (len - expect_space);
		unexpected_space = (!sparse_sequential(src, ptr, expect_space)
			|| !sparse_sequential(src, &ptr[expect_space], remain));

		if (entirely_sequential)
		{
			sparse_warning(src, ptr,
				"Expected a space between keywords '%.*s' and '%.*s'",
				expect_space, ptr, remain, &ptr[expect_space]);
		}
	}

	if (unexpected_space)
	{
		sparse_warning(src, ptr,
			"Unexpected a space in keyword");
	}

	if (name != NULL)
	{
		unsigned nlen = parse_name(src, &ptr[len]);
		name->base = &ptr[len];
		name->size = nlen;

		if ((nlen > 0) && sparse_sequential(
			src, &ptr[len - 1], 2))
		{
			sparse_warning(src, &ptr[len],
				"Expected whitespace between keyword and name");
		}

		len += nlen;
	}

	return len;
}



unsigned parse_keyword(
	const sparse_t* src, const char* ptr,
	parse_keyword_e keyword)
{
	return parse_keyword_name(
		src, ptr, keyword, NULL);
}
