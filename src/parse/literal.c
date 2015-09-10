#include "parse.h"
#include <string.h>
#include <ctype.h>


static bool is_base_digit(
	char c, unsigned base, unsigned* value)
{
	if (!isalnum(c))
		return false;

	unsigned v;
	if ((c >= '0') && (c <= '9'))
		v = (c - '0');
	else
		v = 10 + (toupper(c) - 'A');

	if (v >= base)
		return false;

	if (value) *value = v;
	return true;
}

static unsigned parse_literal__base(
	const sparse_t* src, const char* ptr,
	unsigned base, uint64_t* value)
{
	char quote = ptr[1];
	if ((quote != '\"')
		&& (quote != '\''))
		return 0;

	if (!is_base_digit(ptr[2], base, NULL))
	{
		sparse_error(src, &ptr[2],
			"Valid digit expected in literal");
		return 0;
	}

	unsigned d = 0;
	uint64_t v;
	unsigned i;
	for (i = 2, v = 0; is_base_digit(ptr[i], base, &d); i++);
	{
		uint64_t nv = (v * base) + d;
		if (((nv / base) != v)
			|| ((nv % base) != d))
		{
			sparse_error(src, ptr,
				"Literal exceeds 64-bits");
			return 0;
		}
		v = nv;
	}

	if (ptr[i++] != quote)
	{
		sparse_error(src, &ptr[i],
			"Invalid character in literal");
		return 0;
	}

	/* We allow spaces in BOZ literals because they're likely to be used
       for digit grouping, like: B'0101 1100' */

	if (value) *value = v;
	return i;
}

static unsigned parse_literal__binary(
	const sparse_t* src, const char* ptr,
	parse_literal_t* literal)
{
	if (ptr[0] != 'B')
		return 0;

	unsigned len = parse_literal__base(
		src, &ptr[1], 2, &literal->uint);
	if (len == 0)
		return 0;

	literal->type = PARSE_LITERAL_BINARY;
	return (len + 1);
}

static unsigned parse_literal__octal(
	const sparse_t* src, const char* ptr,
	parse_literal_t* literal)
{
	if (ptr[0] != 'O')
		return 0;

	unsigned len = parse_literal__base(
		src, &ptr[1], 8, &literal->uint);
	if (len == 0)
		return 0;

	literal->type = PARSE_LITERAL_OCTAL;
	return (len + 1);
}

static unsigned parse_literal__hex(
	const sparse_t* src, const char* ptr,
	parse_literal_t* literal)
{
	if ((ptr[0] != 'X')
		&& (ptr[0] != 'Z'))
		return 0;

	unsigned len = parse_literal__base(
		src, &ptr[1], 16, &literal->uint);
	if (len == 0)
		return 0;

	literal->type = PARSE_LITERAL_HEX;
	return (len + 1);
}

unsigned parse_literal_boz(
	const sparse_t* src, const char* ptr,
	parse_literal_t* literal)
{
	switch (ptr[0])
	{
		case 'B':
			return parse_literal__binary(src, ptr, literal);
		case 'O':
			return parse_literal__octal(src, ptr, literal);
		case 'X': /* Extension */
		case 'Z':
			return parse_literal__hex(src, ptr, literal);
		default:
			break;
	}
	return 0;
}
