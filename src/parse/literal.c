#include "parse.h"
#include <string.h>
#include <ctype.h>
#include <math.h>


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
	parse_debug_t* debug,
	unsigned base, bool quoted, uint64_t* value)
{
	unsigned i = 0;

	char quote = '\0';
	if (quoted)
	{
		quote = ptr[i++];
		if ((quote != '\"')
			&& (quote != '\''))
			return 0;
	}

	if (!is_base_digit(ptr[i], base, NULL))
	{
		if (quoted)
		{
			sparse_error(src, &ptr[i],
				"Valid digit expected in BOZ literal");
		}
		return 0;
	}

	unsigned d;
	uint64_t v = 0;
	for (v = 0; is_base_digit(ptr[i], base, &d); i++)
	{
		if (value)
		{
			uint64_t nv = (v * 10) + d;
			if (((nv / base) != v)
				|| ((nv % base) != d))
			{
				parse_debug_warning(debug, src, ptr,
					"Literal value exceeds 64-bit size");
				return 0;
			}
			v = nv;
		}
	}

	if (quoted && (ptr[i++] != quote))
	{
		sparse_error(src, &ptr[i],
			"Invalid character in BOZ literal");
		return 0;
	}

	/* We allow spaces in BOZ literals because they're likely to be used
       for digit grouping, like: B'0101 1100' */

	if (value) *value = v;
	return i;
}

static unsigned parse_literal__binary(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_literal_t* literal)
{
	if (toupper(ptr[0]) != 'B')
		return 0;

	unsigned len = parse_literal__base(
		src, &ptr[1], debug, 2, true, NULL);
	if (len == 0) return 0;

	literal->number = str_ref(&ptr[1], len);
	literal->type = PARSE_LITERAL_BINARY;
	return (len + 1);
}

static unsigned parse_literal__octal(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_literal_t* literal)
{
	if (toupper(ptr[0]) != 'O')
		return 0;

	unsigned len = parse_literal__base(
		src, &ptr[1], debug, 8, true, NULL);
	if (len == 0) return 0;

	literal->number = str_ref(&ptr[1], len);
	literal->type = PARSE_LITERAL_OCTAL;
	return (len + 1);
}

static unsigned parse_literal__hex(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_literal_t* literal)
{
	/* Accepting 'X' in a BOZ literal is an extension. */
	if ((toupper(ptr[0]) != 'X')
		&& (toupper(ptr[0]) != 'Z'))
		return 0;

	unsigned len = parse_literal__base(
		src, &ptr[1], debug, 16, true, NULL);
	if (len == 0) return 0;

	literal->number = str_ref(&ptr[1], len);
	literal->type = PARSE_LITERAL_HEX;
	return (len + 1);
}

unsigned parse_hollerith(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	string_t* string)
{
	unsigned holl_len;
	unsigned i = parse_unsigned(
		src, ptr, debug, &holl_len);
	if (i == 0) return 0;

	if (toupper(ptr[i]) != 'H')
		return 0;

	const char* pptr
		= sparse_parent_pointer(src, &ptr[i]);
	if (!pptr) return 0;
	i += 1;

	*string = string_create(NULL, holl_len);
	if (string_empty(*string))
		return 0;

	unsigned j, holl_pos;
	for (j = 1, holl_pos = 0; holl_pos < holl_len; j++)
	{
		if ((pptr[j] == '\r')
			|| (pptr[j] == '\n')
			|| (pptr[j] == '\0'))
			break;

		if (ptr[i] == pptr[j])
			i++;

		string->base[holl_pos++] = pptr[j];
	}

	if (holl_pos < holl_len)
	{
		while (holl_pos < holl_len)
			string->base[holl_pos++] = ' ';
	}

	return i;
}

static unsigned parse_literal__hollerith(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_literal_t* literal)
{
	unsigned len = parse_hollerith(
		src, ptr, debug, &literal->string);
	if (len == 0) return 0;

	literal->type = PARSE_LITERAL_HOLLERITH;
	return len;
}


unsigned parse_character(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	string_t* string)
{
	unsigned i = 0;

	char quote = ptr[i];
	if ((quote != '\"')
		&& (quote != '\''))
		return 0;

	const char* pptr
		= sparse_parent_pointer(src, &ptr[i]);
	if (!pptr) return 0;

	/* Skip to the end of condense string-> */
	bool is_escaped = false;
	for (i++; ptr[i] != '\0'; i++)
	{
		if (!is_escaped && (ptr[i] == quote))
		{
			if (ptr[i + 1] != quote)
				break;
			is_escaped = true;
		}
		else
		{
			is_escaped = !is_escaped && (ptr[i] == '\\');
		}
	}
	if (ptr[i++] != quote)
	{
		sparse_error(src, ptr,
			"Unterminated string");
		return 0;
	}

	unsigned str_len = 0;
	unsigned j = 1;
	is_escaped = false;
	while (true)
	{
		if ((pptr[j] == '\r')
			|| (pptr[j] == '\n')
			|| (pptr[j] == '\0'))
		{
			sparse_error(src, ptr,
				"Unexpected end of line in character constant");
			return 0;
		}

		if (!is_escaped)
		{
			if (pptr[j] == quote)
			{
				unsigned k;
				for (k = 1; is_hspace(pptr[j + k]); k++);
				if (pptr[j + k] == quote)
				{
					j += k;
					is_escaped = true;
					continue;
				}
				else
				{
					break;
				}
			}
			else if (pptr[j] == '\\')
			{
				j++;
				is_escaped = true;
				continue;
			}
		}

		j++;
		is_escaped = false;
		str_len++;
	}

	unsigned str_pos = 0;
	unsigned str_end = j;

	*string = string_create(NULL, str_len);
	if (string_empty(*string))
		return 0;

	for(j = 1, is_escaped = false; j < str_end; j++)
	{
		if (is_escaped)
		{
			char c = pptr[j];
			switch (pptr[j])
			{
				case 'n':
					c = '\n';
					break;
				case 'r':
					c = '\r';
					break;
				case 't':
					c = '\t';
					break;
				case 'b':
					c = '\b';
					break;
				case 'f':
					c = '\f';
					break;
				case 'v':
					c = '\v';
					break;
				case '0':
					c = '\0';
					break;
				case '\'':
					c = '\'';
					break;
				case '\"':
					c = '\"';
					break;
				case '\\':
					c = '\\';
					break;

				/* '\x' where x is any other character */
				default:
					parse_debug_warning(debug, src, ptr,
						"Unknown escape sequence in string"
						", ignoring escape character");
					break;
			}
			is_escaped = false;
			string->base[str_pos++] = c;
		}
		else if ((pptr[j] == '\\')
			|| (pptr[j] == quote))
		{
			is_escaped = true;
		}
		else
		{
			string->base[str_pos++] = pptr[j];
		}
	}

	return i;
}

static unsigned parse_literal__character(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_literal_t* literal)
{
	unsigned len = parse_character(
		src, ptr, debug, &literal->string);
	if (len == 0) return 0;

	literal->type = PARSE_LITERAL_CHARACTER;
	return len;
}


static unsigned parse_literal__logical(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_literal_t* literal)
{
	unsigned i = 0;

	if (ptr[i++] != '.')
		return 0;

	unsigned len = parse_keyword(
		src, &ptr[i], debug, PARSE_KEYWORD_TRUE);

	bool v = (len > 0);
	if (len == 0)
	{
		len = parse_keyword(
			src, &ptr[i], debug, PARSE_KEYWORD_FALSE);
		if (len == 0) return 0;
	}
	i += len;

	if (ptr[i++] != '.')
		return 0;

	literal->logical = v;
	literal->type = PARSE_LITERAL_LOGICAL;
	return i;
}


static unsigned parse_literal__number(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_literal_t* literal)
{
	unsigned i = 0;

	if ((ptr[i] == '-')
		|| (ptr[i] == '+'))
		i++;

	bool had_int = isdigit(ptr[i]);

	for (; isdigit(ptr[i]); i++);

	bool had_fract = false;
	if (ptr[i] == '.')
	{
		i += 1;
		had_fract = (had_int || isdigit(ptr[i]));

		for (; isdigit(ptr[i]); i++);
	}

	unsigned k = 0;

	if ((toupper(ptr[i]) == 'E')
		|| (toupper(ptr[i]) == 'D'))
	{
		unsigned j = (i + 1);

		if ((ptr[j] == '-')
			|| (ptr[j] == '+'))
			j++;

		if (isdigit(ptr[j]))
		{
			for (; isdigit(ptr[j]); j++);

			if (ptr[j] == 'D')
				k = 8;

			i = j;
		}
	}

	if (!had_fract && !had_int)
		return 0;

	bool kind_ambiguous = false;
	if (ptr[i] == '_')
	{
		i += 1;

		unsigned ok = k;
		unsigned len = parse_unsigned(
			src, &ptr[i], debug, &k);
		if (len == 0) return 0;
		i += len;

		kind_ambiguous = ((ok > 0) && (k != ok));
	}

	if (kind_ambiguous)
	{
		parse_debug_warning(debug, src, ptr,
			"Kind is ambiguous, ignoring exponent kind");
	}

	literal->type = PARSE_LITERAL_NUMBER;
	literal->kind = k;
	literal->number = str_ref(ptr, i);
	return i;
}

static unsigned parse_literal__complex(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_literal_t* literal)
{
	unsigned dpos = parse_debug_position(debug);

	unsigned i = 0;
	if (ptr[i++] != '(')
		return 0;

	parse_literal_t real;
	unsigned len = parse_literal__number(
		src, &ptr[i], debug, &real);
	if (len == 0) return 0;
	i += len;

	if (ptr[i++] != ',')
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}

	parse_literal_t imaginary;
	len = parse_literal__number(
		src,  &ptr[i], debug, &imaginary);
	if (len == 0)
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	if (ptr[i++] != ')')
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}

	literal->type = PARSE_LITERAL_COMPLEX;
	literal->complex.real      = real.number;
	literal->complex.imaginary = imaginary.number;

	return i;
}



unsigned parse_literal(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_literal_t* literal)
{
	parse_literal_t l;
	l.kind = 0;

	unsigned len = 0;
	if (len == 0) len = parse_literal__character(src, ptr, debug, &l);
	if (len == 0) len = parse_literal__hollerith(src, ptr, debug, &l);
	if (len == 0) len = parse_literal__complex(src, ptr, debug, &l);
	if (len == 0) len = parse_literal__binary(src, ptr, debug, &l);
	if (len == 0) len = parse_literal__octal(src, ptr, debug, &l);
	if (len == 0) len = parse_literal__hex(src, ptr, debug, &l);
	if (len == 0) len = parse_literal__logical(src, ptr, debug, &l);
	if (len == 0) len = parse_literal__number(src, ptr, debug, &l);

	if (len == 0)
		return 0;

	*literal = l;
	return len;
}

void parse_literal_cleanup(
	parse_literal_t literal)
{
	switch (literal.type)
	{
		case PARSE_LITERAL_CHARACTER:
		case PARSE_LITERAL_HOLLERITH:
			string_delete(literal.string);
			break;
		default:
			break;
	}
}

bool parse_literal_clone(
	parse_literal_t* dst, const parse_literal_t* src)
{
	if (!dst || !src)
		return false;

	parse_literal_t clone = *src;
	switch (src->type)
	{
		case PARSE_LITERAL_CHARACTER:
		case PARSE_LITERAL_HOLLERITH:
			clone.string = string_copy(src->string);
			if (string_empty(clone.string))
				return false;
			break;
		default:
			break;
	}

	*dst = clone;
	return true;
}


unsigned parse_unsigned(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* value)
{
	unsigned dpos = parse_debug_position(debug);

	uint64_t u;
	unsigned len = parse_literal__base(
		src, ptr, debug, 10, false, &u);
	if (len == 0) return 0;

	unsigned v = (unsigned)u;
	if ((uint64_t)v != u)
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}

	*value = v;
	return len;
}


bool parse_literal_print(
	int fd, const parse_literal_t literal)
{
	switch (literal.type)
	{
		case PARSE_LITERAL_NUMBER:
			return str_ref_print(
				fd, literal.number);
		case PARSE_LITERAL_BINARY:
			return (dprintf_bool(fd, "B\"")
				&& str_ref_print(fd, literal.number)
				&& dprintf_bool(fd, "\""));
		case PARSE_LITERAL_OCTAL:
			return (dprintf_bool(fd, "O\"")
				&& str_ref_print(fd, literal.number)
				&& dprintf_bool(fd, "\""));
		case PARSE_LITERAL_HEX:
			return (dprintf_bool(fd, "Z\"")
				&& str_ref_print(fd, literal.number)
				&& dprintf_bool(fd, "\""));
		case PARSE_LITERAL_HOLLERITH:
			return (dprintf_bool(fd, "%uH",
				string_length(literal.string))
				&& string_print(fd, literal.string));
		case PARSE_LITERAL_CHARACTER:
			return (dprintf_bool(fd, "\"")
				&& string_print_escaped(
					fd, literal.string)
				&& dprintf_bool(fd, "\""));
		case PARSE_LITERAL_COMPLEX:
			return dprintf_bool(fd, "(%.*s, %.*s)",
				literal.complex.real.size,
				literal.complex.real.base,
				literal.complex.imaginary.size,
				literal.complex.imaginary.base);
		case PARSE_LITERAL_LOGICAL:
			return dprintf_bool(fd, ".%s.",
				(literal.logical
					? "TRUE" : "FALSE"));
		default:
			break;
	}

	return false;
}
