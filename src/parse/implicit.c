#include "parse.h"


const parse_implicit_t PARSE_IMPLICIT_DEFAULT = 
{
	.c =
	{
		{ .type = PARSE_TYPE_REAL, .kind = 0, .count = 0 }, /* A */
		{ .type = PARSE_TYPE_REAL, .kind = 0, .count = 0 }, /* B */
		{ .type = PARSE_TYPE_REAL, .kind = 0, .count = 0 }, /* C */
		{ .type = PARSE_TYPE_REAL, .kind = 0, .count = 0 }, /* D */
		{ .type = PARSE_TYPE_REAL, .kind = 0, .count = 0 }, /* E */
		{ .type = PARSE_TYPE_REAL, .kind = 0, .count = 0 }, /* F */
		{ .type = PARSE_TYPE_REAL, .kind = 0, .count = 0 }, /* G */
		{ .type = PARSE_TYPE_REAL, .kind = 0, .count = 0 }, /* H */

		{ .type = PARSE_TYPE_INTEGER, .kind = 0, .count = 0 }, /* I */
		{ .type = PARSE_TYPE_INTEGER, .kind = 0, .count = 0 }, /* J */
		{ .type = PARSE_TYPE_INTEGER, .kind = 0, .count = 0 }, /* K */
		{ .type = PARSE_TYPE_INTEGER, .kind = 0, .count = 0 }, /* L */
		{ .type = PARSE_TYPE_INTEGER, .kind = 0, .count = 0 }, /* M */
		{ .type = PARSE_TYPE_INTEGER, .kind = 0, .count = 0 }, /* N */

		{ .type = PARSE_TYPE_REAL, .kind = 0, .count = 0 }, /* O */
		{ .type = PARSE_TYPE_REAL, .kind = 0, .count = 0 }, /* P */
		{ .type = PARSE_TYPE_REAL, .kind = 0, .count = 0 }, /* Q */
		{ .type = PARSE_TYPE_REAL, .kind = 0, .count = 0 }, /* R */
		{ .type = PARSE_TYPE_REAL, .kind = 0, .count = 0 }, /* S */
		{ .type = PARSE_TYPE_REAL, .kind = 0, .count = 0 }, /* T */
		{ .type = PARSE_TYPE_REAL, .kind = 0, .count = 0 }, /* U */
		{ .type = PARSE_TYPE_REAL, .kind = 0, .count = 0 }, /* V */
		{ .type = PARSE_TYPE_REAL, .kind = 0, .count = 0 }, /* W */
		{ .type = PARSE_TYPE_REAL, .kind = 0, .count = 0 }, /* X */
		{ .type = PARSE_TYPE_REAL, .kind = 0, .count = 0 }, /* Y */
		{ .type = PARSE_TYPE_REAL, .kind = 0, .count = 0 }, /* Z */
	},
};


unsigned parse_implicit(
	const sparse_t* src, const char* ptr,
	parse_implicit_t* implicit)
{
	unsigned i;

	i = parse_keyword(src, ptr,
		PARSE_KEYWORD_IMPLICIT_NONE);
	if (i > 0)
	{
		unsigned j;
		for (j = 0; j < 26; j++)
			implicit->c[j].type = PARSE_TYPE_NONE;
	}
	else
	{
		i = parse_keyword(src, ptr,
			PARSE_KEYWORD_IMPLICIT);
		if (i == 0) return 0;

		/* TODO - Implement other implicit rules. */
		return 0;
	}

	if ((ptr[i] == '\r')
		|| (ptr[i] == '\n'))
	{
		i += 1;
	}
	else
	{
		sparse_warning(src, &ptr[i],
			"Expected newline after implicit statement");
	}

	return i;
}
