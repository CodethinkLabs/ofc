#include "../parse.h"


const parse_stmt_implicit_t parse_stmt_implicit_DEFAULT =
{
	.c =
	{
		PARSE_TYPE_REAL_DEFAULT, /* A */
		PARSE_TYPE_REAL_DEFAULT, /* B */
		PARSE_TYPE_REAL_DEFAULT, /* C */
		PARSE_TYPE_REAL_DEFAULT, /* D */
		PARSE_TYPE_REAL_DEFAULT, /* E */
		PARSE_TYPE_REAL_DEFAULT, /* F */
		PARSE_TYPE_REAL_DEFAULT, /* G */
		PARSE_TYPE_REAL_DEFAULT, /* H */

		PARSE_TYPE_INTEGER_DEFAULT, /* I */
		PARSE_TYPE_INTEGER_DEFAULT, /* J */
		PARSE_TYPE_INTEGER_DEFAULT, /* K */
		PARSE_TYPE_INTEGER_DEFAULT, /* L */
		PARSE_TYPE_INTEGER_DEFAULT, /* M */
		PARSE_TYPE_INTEGER_DEFAULT, /* N */

		PARSE_TYPE_REAL_DEFAULT, /* O */
		PARSE_TYPE_REAL_DEFAULT, /* P */
		PARSE_TYPE_REAL_DEFAULT, /* Q */
		PARSE_TYPE_REAL_DEFAULT, /* R */
		PARSE_TYPE_REAL_DEFAULT, /* S */
		PARSE_TYPE_REAL_DEFAULT, /* T */
		PARSE_TYPE_REAL_DEFAULT, /* U */
		PARSE_TYPE_REAL_DEFAULT, /* V */
		PARSE_TYPE_REAL_DEFAULT, /* W */
		PARSE_TYPE_REAL_DEFAULT, /* X */
		PARSE_TYPE_REAL_DEFAULT, /* Y */
		PARSE_TYPE_REAL_DEFAULT, /* Z */
	},
};


static unsigned parse_stmt__implicit(
	const sparse_t* src, const char* ptr,
	parse_stmt_implicit_t* implicit)
{
	unsigned i = parse_keyword(src, ptr,
		PARSE_KEYWORD_IMPLICIT_NONE);
	if (i > 0)
	{
		unsigned j;
		for (j = 0; j < 26; j++)
			implicit->c[j].type = PARSE_TYPE_NONE;
		return i;
	}


	i = parse_keyword(src, ptr,
		PARSE_KEYWORD_IMPLICIT);
	if (i == 0) return 0;

	/* TODO - Implement other implicit rules. */
	return 0;
}

unsigned parse_stmt_implicit(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	parse_stmt_implicit_t implicit;
	unsigned len = parse_stmt__implicit(
		src, ptr, &implicit);
	if (len == 0) return 0;

	stmt->implicit = (parse_stmt_implicit_t*)malloc(
		sizeof(parse_stmt_implicit_t));
	if (!stmt->implicit) return 0;

	*(stmt->implicit) = implicit;
	stmt->type = PARSE_STMT_IMPLICIT;
	return len;
}
