/* Copyright 2015 Codethink Ltd.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include <ctype.h>
#include <math.h>
#include <string.h>

#include "ofc/parse.h"
#include "ofc/global_opts.h"


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

static unsigned ofc_parse_literal__base(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
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
			ofc_parse_debug_warning(debug,
				ofc_sparse_ref(src, &ptr[i], 1),
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
			uint64_t nv = (v * base) + d;
			if (((nv / base) != v)
				|| ((nv % base) != d))
			{
				ofc_parse_debug_warning(debug,
					ofc_sparse_ref(src, ptr, i),
					"Literal value exceeds 64-bit size");
				return 0;
			}
			v = nv;
		}
	}

	if (quoted && (ptr[i++] != quote))
	{
		ofc_parse_debug_warning(debug,
			ofc_sparse_ref(src, &ptr[i], 1),
			"Invalid character in BOZ literal");
		return 0;
	}

	/* We allow spaces in BOZ literals because they're likely to be used
	   for digit grouping, like: B'0101 1100' */

	if (value) *value = v;
	return i;
}

static unsigned ofc_parse_literal__binary(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_literal_t* literal)
{
	unsigned i = 0;

	unsigned dpos = ofc_parse_debug_position(debug);

	bool prefix = (toupper(ptr[i]) == 'B');
	if (prefix) i += 1;

	unsigned base = i;
	unsigned len = ofc_parse_literal__base(
		src, &ptr[i], debug, 2, true, NULL);
	if (len == 0)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	if (!prefix)
	{
		if (toupper(ptr[i]) != 'B')
		{
			ofc_parse_debug_rewind(debug, dpos);
			return 0;
		}
		i += 1;
	}

	literal->number = ofc_str_ref(&ptr[base + 1], (len - 2));
	literal->type = OFC_PARSE_LITERAL_BINARY;
	return i;
}

static unsigned ofc_parse_literal__octal(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_literal_t* literal)
{
	unsigned i = 0;

	unsigned dpos = ofc_parse_debug_position(debug);

	bool prefix = (toupper(ptr[i]) == 'O');
	if (prefix) i += 1;

	unsigned base = i;
	unsigned len = ofc_parse_literal__base(
		src, &ptr[i], debug, 8, true, NULL);
	if (len == 0)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	if (!prefix)
	{
		if (toupper(ptr[i]) != 'O')
		{
			ofc_parse_debug_rewind(debug, dpos);
			return 0;
		}
		i += 1;
	}

	literal->number = ofc_str_ref(&ptr[base + 1], (len - 2));
	literal->type = OFC_PARSE_LITERAL_OCTAL;
	return i;
}

static unsigned ofc_parse_literal__hex(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_literal_t* literal)
{
	unsigned i = 0;

	unsigned dpos = ofc_parse_debug_position(debug);

	/* Accepting 'X' in a BOZ literal is an extension. */
	bool prefix = ((toupper(ptr[i]) == 'X')
		|| (toupper(ptr[i]) == 'Z'));
	if (prefix) i += 1;

	unsigned base = i;
	unsigned len = ofc_parse_literal__base(
		src, &ptr[i], debug, 16, true, NULL);
	if (len == 0)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	if (!prefix)
	{
		if ((toupper(ptr[i]) != 'X')
			&& (toupper(ptr[i]) != 'Z'))
		{
			ofc_parse_debug_rewind(debug, dpos);
			return 0;
		}
		i += 1;
	}

	literal->number = ofc_str_ref(&ptr[base + 1], (len - 2));
	literal->type = OFC_PARSE_LITERAL_HEX;
	return i;
}

ofc_string_t* ofc_parse_hollerith(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	unsigned holl_len;
	unsigned i = ofc_parse_unsigned(
		src, ptr, debug, &holl_len);
	if (i == 0) return NULL;

	if (toupper(ptr[i]) != 'H')
		return NULL;

	char s[holl_len + 1];

	const char* pptr
		= ofc_sparse_parent_pointer(src, &ptr[i]);
	if (!pptr) return NULL;
	i += 1;

	unsigned j, holl_pos;
	for (j = 1, holl_pos = 0; holl_pos < holl_len; j++)
	{
		if ((pptr[j] == '\r')
			|| (pptr[j] == '\n')
			|| (pptr[j] == '\0'))
			break;

		if (ptr[i] == pptr[j])
			i++;

		s[holl_pos++] = pptr[j];
	}

	if (holl_pos < holl_len)
	{
		while (holl_pos < holl_len)
			s[holl_pos++] = ' ';
	}

	s[holl_pos] = '\0';

	ofc_string_t* string
		= ofc_string_create(s, holl_len);
	if (!string) return NULL;

	if (len) *len = i;
	return string;
}

static unsigned ofc_parse_literal__hollerith(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_literal_t* literal)
{
	unsigned len = 0;
	literal->string = ofc_parse_hollerith(
		src, ptr, debug, &len);

	if (len == 0) return 0;

	literal->type = OFC_PARSE_LITERAL_HOLLERITH;
	return len;
}


ofc_string_t* ofc_parse__character(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len, bool quiet, bool allow_escape)
{
	unsigned i = 0;

	char quote = ptr[i];
	if ((quote != '\"')
		&& (quote != '\''))
		return NULL;

	const char* pptr
		= ofc_sparse_parent_pointer(src, &ptr[i]);
	if (!pptr) return NULL;

	/* Skip to the end of condense string-> */
	bool is_escaped = false;
	for (i++; ptr[i] != '\0'; i++)
	{
		if (!is_escaped && (ptr[i] == quote))
		{
			if (ptr[i + 1] != quote)
				break;
			is_escaped = ofc_sparse_sequential(src, &ptr[i], 2);
			is_escaped = is_escaped && allow_escape;
			if (!is_escaped)
				i += 1;
		}
		else
		{
			is_escaped = allow_escape && !is_escaped && (ptr[i] == '\\');
		}
	}
	if (ptr[i++] != quote)
	{
		if (!quiet)
			ofc_sparse_error_ptr(
				src, ptr, "Unterminated string");
		return NULL;
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
			if (!quiet)
				ofc_sparse_error_ptr(src, ptr,
					"Unexpected end of line in character constant");
			return NULL;
		}

		if (!is_escaped)
		{
			if (pptr[j] == quote)
			{
				is_escaped = (pptr[j + 1] == quote);
				is_escaped = is_escaped && allow_escape;
				if (is_escaped)
				{
					j++;
					continue;
				}

				unsigned k;
				for (k = 1; ofc_is_hspace(pptr[j + k]); k++);
				if (pptr[j + k] == quote)
				{
					j += (k + 1);
					continue;
				}
				else
				{
					break;
				}
			}
			else if (pptr[j] == '\\')
			{
				is_escaped = allow_escape;
				if (is_escaped)
				{
					j++;
					continue;
				}
			}
		}

		j++;
		is_escaped = false;
		str_len++;
	}

	unsigned str_pos = 0;
	unsigned str_end = j;

	ofc_string_t* string = ofc_string_create(NULL, str_len);
	if (!string) return NULL;

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
					/* TODO - Point to actual unknown escape sequence. */
					ofc_parse_debug_warning(debug,
						ofc_sparse_ref(src, ptr, i),
						"Unknown escape sequence in string"
						", ignoring escape character");
					break;
			}
			is_escaped = false;
			string->base[str_pos++] = c;
		}
		else if (pptr[j] == '\\')
		{
			is_escaped = allow_escape;
			if (!is_escaped) string->base[str_pos++] = '\\';
		}
		else if (pptr[j] == quote)
		{
			is_escaped = (pptr[j + 1] == quote);
			is_escaped = is_escaped && allow_escape;
			if (!is_escaped)
				for (j++; (j < str_end) && (pptr[j] != quote); j++);
		}
		else
		{
			string->base[str_pos++] = pptr[j];
		}
	}

	if (len) *len = i;
	return string;
}

ofc_string_t* ofc_parse__character_escaped(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len, bool quiet)
{
	return ofc_parse__character(
		src, ptr, debug, len, quiet, true);
}

ofc_string_t* ofc_parse__character_unescaped(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len, bool quiet)
{
	return ofc_parse__character(
		src, ptr, debug, len, quiet, false);
}

ofc_string_t* ofc_parse_character(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	if (!global_opts.no_escape)
	{
		return ofc_parse__character_escaped(
			src, ptr, debug, len, false);
	}
	else
	{
		return ofc_parse__character_unescaped(
		src, ptr, debug, len, false);
	}
}

static unsigned ofc_parse_literal__character(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_literal_t* literal)
{
	unsigned len = 0;
	if (!global_opts.no_escape)
	{
		literal->string = ofc_parse__character_escaped(
			src, ptr, debug, &len, true);
		if (!literal->string) return 0;
	}
	else
	{
		literal->string = ofc_parse__character_unescaped(
			src, ptr, debug, &len, true);
		if (!literal->string) return 0;
	}

	literal->type = OFC_PARSE_LITERAL_CHARACTER;
	return len;
}


static unsigned ofc_parse_literal__logical(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_literal_t* literal)
{
	unsigned i = 0;

	if (ptr[i++] != '.')
		return 0;

	unsigned len = ofc_parse_keyword(
		src, &ptr[i], debug, OFC_PARSE_KEYWORD_TRUE);

	bool v = (len > 0);
	if (len == 0)
	{
		len = ofc_parse_keyword(
			src, &ptr[i], debug, OFC_PARSE_KEYWORD_FALSE);
		if (len == 0) return 0;
	}
	i += len;

	if (ptr[i++] != '.')
		return 0;

	literal->logical = v;
	literal->type = OFC_PARSE_LITERAL_LOGICAL;
	return i;
}


unsigned ofc_parse_literal_number(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_literal_t* literal)
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

	unsigned k = 1;

	if ((toupper(ptr[i]) == 'E')
		|| (toupper(ptr[i]) == 'D')
		|| (toupper(ptr[i]) == 'Q'))
	{
		unsigned j = (i + 1);

		if ((ptr[j] == '-')
			|| (ptr[j] == '+'))
			j++;

		if (isdigit(ptr[j]))
		{
			for (; isdigit(ptr[j]); j++);

			if (toupper(ptr[j]) == 'D')
				k = 2;
			else if (toupper(ptr[j]) == 'Q')
				k = 4;

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
		unsigned len = ofc_parse_unsigned(
			src, &ptr[i], debug, &k);
		if (len == 0) return 0;
		i += len;

		kind_ambiguous = ((ok != 1) && (k != ok));
	}

	if (kind_ambiguous)
	{
		ofc_parse_debug_warning(debug,
			ofc_sparse_ref(src, ptr, i),
			"Kind is ambiguous, ignoring exponent kind");
	}

	literal->type   = OFC_PARSE_LITERAL_NUMBER;
	literal->kind   = k;
	literal->src    = ofc_sparse_ref(src, ptr, i);
	literal->number = ofc_str_ref(ptr, i);
	return i;
}

unsigned ofc_parse_literal_integer(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_literal_t* literal)
{
	(void)src;
	(void)debug;

	if (!isdigit(ptr[0]))
		return 0;

	unsigned i;
	for (i = 1; isdigit(ptr[i]); i++);

	literal->type = OFC_PARSE_LITERAL_NUMBER;
	literal->kind = 1;
	literal->src    = ofc_sparse_ref(src, ptr, i);
	literal->number = ofc_str_ref(ptr, i);
	return i;
}

static unsigned ofc_parse_literal__complex(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_literal_t* literal)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = 0;
	if (ptr[i++] != '(')
		return 0;

	ofc_parse_literal_t real;
	unsigned len = ofc_parse_literal_number(
		src, &ptr[i], debug, &real);
	if (len == 0) return 0;
	i += len;

	if (ptr[i++] != ',')
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}

	ofc_parse_literal_t imaginary;
	len = ofc_parse_literal_number(
		src,  &ptr[i], debug, &imaginary);
	if (len == 0)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	if (ptr[i++] != ')')
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}

	literal->type = OFC_PARSE_LITERAL_COMPLEX;
	literal->complex.real	  = real.number;
	literal->complex.imaginary = imaginary.number;

	return i;
}



unsigned ofc_parse_literal(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_literal_t* literal)
{
	ofc_parse_literal_t l;
	l.kind = 1;

	/* Order is important here. */
	unsigned len = 0;
	if (len == 0) len = ofc_parse_literal__hollerith(src, ptr, debug, &l);
	if (len == 0) len = ofc_parse_literal__complex(src, ptr, debug, &l);
	if (len == 0) len = ofc_parse_literal__binary(src, ptr, debug, &l);
	if (len == 0) len = ofc_parse_literal__octal(src, ptr, debug, &l);
	if (len == 0) len = ofc_parse_literal__hex(src, ptr, debug, &l);
	if (len == 0) len = ofc_parse_literal__character(src, ptr, debug, &l);
	if (len == 0) len = ofc_parse_literal__logical(src, ptr, debug, &l);
	if (len == 0) len = ofc_parse_literal_number(src, ptr, debug, &l);

	if (len == 0)
		return 0;

	l.src = ofc_sparse_ref(src, ptr, len);

	*literal = l;
	return len;
}

void ofc_parse_literal_cleanup(
	ofc_parse_literal_t literal)
{
	switch (literal.type)
	{
		case OFC_PARSE_LITERAL_CHARACTER:
		case OFC_PARSE_LITERAL_HOLLERITH:
			ofc_string_delete(literal.string);
			break;
		default:
			break;
	}
}

bool ofc_parse_literal_clone(
	ofc_parse_literal_t* dst, const ofc_parse_literal_t* src)
{
	if (!dst || !src)
		return false;

	ofc_parse_literal_t clone = *src;
	switch (src->type)
	{
		case OFC_PARSE_LITERAL_CHARACTER:
		case OFC_PARSE_LITERAL_HOLLERITH:
			clone.string = ofc_string_copy(src->string);
			if (ofc_string_empty(clone.string))
				return false;
			break;
		default:
			break;
	}

	*dst = clone;
	return true;
}


unsigned ofc_parse_unsigned(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* value)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	uint64_t u;
	unsigned len = ofc_parse_literal__base(
		src, ptr, debug, 10, false, &u);
	if (len == 0) return 0;

	unsigned v = (unsigned)u;
	if ((uint64_t)v != u)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}

	*value = v;
	return len;
}


bool ofc_parse_literal_print(
	ofc_colstr_t* cs, const ofc_parse_literal_t literal)
{
	switch (literal.type)
	{
		case OFC_PARSE_LITERAL_NUMBER:
			return ofc_str_ref_print(
				cs, literal.number);
		case OFC_PARSE_LITERAL_BINARY:
			return ofc_colstr_write_quoted(cs, "B", '\"',
				literal.number.base, literal.number.size);
		case OFC_PARSE_LITERAL_OCTAL:
			return ofc_colstr_write_quoted(cs, "O", '\"',
				literal.number.base, literal.number.size);
		case OFC_PARSE_LITERAL_HEX:
			return ofc_colstr_write_quoted(cs, "Z", '\"',
				literal.number.base, literal.number.size);
		case OFC_PARSE_LITERAL_HOLLERITH:
			return (ofc_colstr_atomic_writef(cs, "%uH%s",
				ofc_string_length(literal.string),
				ofc_string_strz(literal.string)));
		case OFC_PARSE_LITERAL_CHARACTER:
			return ofc_colstr_write_escaped(cs, '\"',
					ofc_string_strz(literal.string),
					ofc_string_length(literal.string));
		case OFC_PARSE_LITERAL_COMPLEX:
			return ofc_colstr_atomic_writef(cs, "(%.*s, %.*s)",
				literal.complex.real.size,
				literal.complex.real.base,
				literal.complex.imaginary.size,
				literal.complex.imaginary.base);
		case OFC_PARSE_LITERAL_LOGICAL:
			return ofc_colstr_atomic_writef(cs, ".%s.",
				(literal.logical
					? "TRUE" : "FALSE"));
		default:
			break;
	}

	return false;
}
