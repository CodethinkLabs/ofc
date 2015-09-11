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
		sparse_error(src, &ptr[2],
			"Valid digit expected in literal");
		return 0;
	}

	unsigned d = 0;
	uint64_t v;
	for (v = 0; is_base_digit(ptr[i], base, &d); i++);
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

	if (quoted && (ptr[i++] != quote))
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
		src, &ptr[1], 2, true, &literal->uint);
	if (len == 0) return 0;

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
		src, &ptr[1], 8, true, &literal->uint);
	if (len == 0) return 0;

	literal->type = PARSE_LITERAL_OCTAL;
	return (len + 1);
}

static unsigned parse_literal__hex(
	const sparse_t* src, const char* ptr,
	parse_literal_t* literal)
{
	/* Accepting 'X' in a BOZ literal is an extension. */
	if ((ptr[0] != 'X')
		&& (ptr[0] != 'Z'))
		return 0;

	unsigned len = parse_literal__base(
		src, &ptr[1], 16, true, &literal->uint);
	if (len == 0) return 0;

	literal->type = PARSE_LITERAL_HEX;
	return (len + 1);
}


static unsigned parse_literal__uint(
	const sparse_t* src, const char* ptr,
	parse_literal_t* literal)
{
	uint64_t v;
	unsigned len = parse_literal__base(
		src, ptr, 10, false, &v);

	if (len != 0)
	{
		literal->type = PARSE_LITERAL_UNSIGNED_INT;
		literal->uint = v;
	}

	return len;
}

static unsigned parse_literal__sint(
	const sparse_t* src, const char* ptr,
	parse_literal_t* literal)
{
	unsigned i = 0;

	bool negate = (ptr[i] == '-');
	if (negate || (ptr[i] == '+'))
		i++;

	uint64_t v;
	unsigned len = parse_literal__base(
		src, &ptr[i], 10, false, &v);
	if (len == 0) return 0;
	i += len;

	if (negate
		? (v >> 63)
		: (v > (1ULL << 63U)))
	{
		sparse_error(src, ptr,
			"Signed integer literal exceeds 64-bits");
		return 0;
	}

	if (!sparse_sequential(src, ptr, i))
	{
		sparse_warning(src, ptr,
			"Unexpected whitespace in signed literal");
	}

	literal->type = PARSE_LITERAL_SIGNED_INT;
	literal->sint = (negate ? -((int64_t)v) : (int64_t)v);
	return i;
}

static unsigned parse_literal__real(
	const sparse_t* src, const char* ptr,
	parse_literal_t* literal)
{
	unsigned i = 0;

	bool negate = (ptr[i] == '-');
	if (negate || (ptr[i] == '+'))
		i++;

	bool had_int = isdigit(ptr[i]);

	long double v;
	for (v = 0.0; isdigit(ptr[i]); i++)
	{
		unsigned d = (ptr[i] - '0');
		v = (v * 10.0) + d;
	}

	bool had_fract = false;
	if (ptr[i] == '.')
	{
		had_fract = isdigit(ptr[++i]);

		long double f;
		for (f = 10.0; isdigit(ptr[i]); i++, f *= 10.0)
		{
			unsigned d = (ptr[i] - '0');
			v += (((long double)d) / f);
		}
	}

	bool had_exponent
		= ((ptr[i] == 'E') || (ptr[i] == 'D'));

	unsigned k;
	if (ptr[i] == 'D')
		k = 8;

	if (had_exponent)
	{
		i += 1;

		parse_literal_t l;
		unsigned len = parse_literal__sint(
			src, &ptr[i], &l);
		if (len == 0) return 0;
		i += len;

		double e = pow(10.0, (double)l.sint);
		v *= e;
	}

	/* A REAL literal must have either an exponent or fractional part. */
	bool valid = (had_fract || (had_int && had_exponent));
	if (!valid) return 0;

	bool kind_ambiguous = false;
	if (ptr[i] == '_')
	{
		i += 1;

		uint64_t nk;
		unsigned len = parse_literal__base(
			src, &ptr[i], 10, false, &nk);
		if (len == 0) return 0;
		i += len;

		unsigned ok = k;

		k = nk;
		if (k != nk)
			return 0;

		kind_ambiguous = ((ok > 0) && (k != ok));
	}

	if (!sparse_sequential(src, ptr, i))
	{
		sparse_warning(src, ptr,
			"Unexpected whitespace in REAL literal");
	}

	if (kind_ambiguous)
	{
		sparse_warning(src, ptr,
			"Kind is ambiguous, ignoring exponent kind");
	}

	literal->type = PARSE_LITERAL_REAL;
	literal->kind = k;
	literal->real = v;
	return i;
}



unsigned parse_literal(
	const sparse_t* src, const char* ptr,
	parse_literal_t* literal)
{
	parse_literal_t l;
	l.kind = 0;

	unsigned len = 0;
	if (len == 0) len = parse_literal__binary(src, ptr, &l);
	if (len == 0) len = parse_literal__octal(src, ptr, &l);
	if (len == 0) len = parse_literal__hex(src, ptr, &l);
	if (len == 0) len = parse_literal__uint(src, ptr, &l);
	if (len == 0) len = parse_literal__sint(src, ptr, &l);
	if (len == 0) len = parse_literal__real(src, ptr, &l);

	/* TODO - Implement Hollerith and string parsing. */
	/*if (len == 0) len = parse_literal__hollerith(src, ptr, &l);*/
	/*if (len == 0) len = parse_literal__string(src, ptr, &l);*/

	if (len == 0)
		return 0;

	*literal = l;
	return len;
}
