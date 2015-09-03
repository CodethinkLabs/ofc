#include "parse.h"
#include <string.h>
#include <ctype.h>


static bool is_base_digit(char c, unsigned base)
{
	if (!isalnum(c))
		return false;

	unsigned value;
	if ((c >= '0') && (c <= '9'))
		value = (c - '0');
	else
		value = 10 + (c - 'A');
	return (value < base);
}

static unsigned parse_base_constant(
	const char* src, unsigned base, parse_t* entry)
{
	char quote = src[1];
	if ((quote != '\"')
		&& (quote != '\''))
		return 0;

	if (!is_base_digit(src[2], base))
		return 0;

	unsigned i;
	for (i = 3; is_base_digit(src[i], base); i++);

	if (src[i++] != quote)
		return 0;

	return i;
}

unsigned parse_binary_constant(const char* src, parse_t* entry)
{
	if (src[0] != 'B')
		return 0;

	unsigned len = parse_base_constant(&src[1], 2, entry);
	if (len == 0)
		return 0;

	entry->type = PARSE_TYPE_BINARY_CONSTANT;
	return (len + 1);
}

unsigned parse_octal_constant(const char* src, parse_t* entry)
{
	if (src[0] != 'O')
		return 0;

	unsigned len = parse_base_constant(&src[1], 8, entry);
	if (len == 0)
		return 0;

	entry->type = PARSE_TYPE_OCTAL_CONSTANT;
	return (len + 1);
}

unsigned parse_hex_constant(const char* src, parse_t* entry)
{
	if ((src[0] != 'X')
		&& (src[0] != 'Z'))
		return 0;

	unsigned len = parse_base_constant(&src[1], 16, entry);
	if (len == 0)
		return 0;

	entry->type = PARSE_TYPE_HEX_CONSTANT;
	return (len + 1);
}

unsigned parse_boz_literal_constant(const char* src, parse_t* entry)
{
	switch (src[0])
	{
		case 'B':
			return parse_binary_constant(src, entry);
		case 'O':
			return parse_octal_constant(src, entry);
		case 'X': /* Extension */
		case 'Z':
			return parse_hex_constant(src, entry);
		default:
			break;
	}
	return 0;
}
