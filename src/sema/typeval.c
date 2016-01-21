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

#include <math.h>
#include <tgmath.h>
#ifdef complex
/* Remove macro from complex.h */
#undef complex
#endif

#include "ofc/noopt.h"
#include "ofc/sema.h"
#include "ofc/target.h"


/* TODO - Remove NO_OPT, once we find a better workaround
          for the compiler error. */
static ofc_sema_typeval_t* NO_OPT ofc_sema_typeval__alloc(
	const ofc_sema_typeval_t typeval)
{
	ofc_sema_typeval_t* alloc_typeval =
		(ofc_sema_typeval_t*)malloc(sizeof(ofc_sema_typeval_t));

	if (!alloc_typeval) return NULL;

	*alloc_typeval = typeval;

	return alloc_typeval;
}

static bool ofc_sema_typeval__in_range(
	const ofc_sema_typeval_t* typeval)
{
	if (!typeval)
		return false;

	if (!typeval->type)
		return true;

	/* We only range check integers. */
	if (typeval->type->type
		!= OFC_SEMA_TYPE_INTEGER)
		return true;

	unsigned size;
	if (!ofc_sema_type_size(
		typeval->type, &size))
		return false;

	if (size >= sizeof(typeval->integer))
		return true;

	int64_t imax = 1LL << ((size * 8) - 1);
	int64_t imin = -imax;

	return ((typeval->integer < imax)
		&& (typeval->integer >= imin));
}

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

static ofc_sema_typeval_t* ofc_sema_typeval__integer_literal(
	const ofc_parse_literal_t* literal,
	const ofc_sema_type_t* type)
{
	if (!literal)
		return NULL;

	bool is_byte = false;
	if (type)
	{
		switch (type->type)
		{
			case OFC_SEMA_TYPE_INTEGER:
				break;
			case OFC_SEMA_TYPE_BYTE:
				is_byte = true;
				break;
			default:
				return NULL;
		}
	}

	unsigned base = 10;
	switch (literal->type)
	{
		case OFC_PARSE_LITERAL_NUMBER:
			base = 10;
			break;
		case OFC_PARSE_LITERAL_BINARY:
			base = 2;
			break;
		case OFC_PARSE_LITERAL_OCTAL:
			base = 8;
			break;
		case OFC_PARSE_LITERAL_HEX:
			base = 16;
			break;
		default:
			return NULL;
	}

	const char* ptr  = literal->number.base;
	unsigned    size = literal->number.size;

	if (!ptr || (size == 0))
		return NULL;

	unsigned i = 0;
	bool negate = (ptr[i] == '-');
	if (negate || (ptr[i] == '+'))
		i += 1;

	ofc_sema_typeval_t typeval
		= { .type = type, .integer = 0 };

	bool out_of_range = false;
	uint64_t uvalue = 0;
	unsigned digit;
	for (; (i < size) && is_base_digit(ptr[i], base, &digit); i++)
	{
		uint64_t nvalue = (uvalue * base) + digit;
		if (((nvalue / base) != uvalue)
			|| ((nvalue % base) != digit))
			out_of_range = true;

		uvalue = nvalue;

	}

	uint64_t smax = (1LL << ((sizeof(typeval.integer) * 8) - 1));

	if (negate)
	{
		if (uvalue > smax)
			out_of_range = true;
		typeval.integer = -uvalue;
	}
	else
	{
		if (uvalue >= smax)
			out_of_range = true;
		typeval.integer = uvalue;
	}

	if (out_of_range)
	{
		ofc_sparse_ref_error(literal->src,
			"Out of range for compiler");
		return NULL;
	}

	unsigned kind = 0;
	if ((i < size) && (ptr[i] == '_'))
	{
		i++;

		if ((i >= size)
			|| (!isdigit(ptr[i])))
			return NULL;

		for (; (i < size) && isdigit(ptr[i]); i++)
		{
			digit = (ptr[i] - '0');
			unsigned nkind = (kind * 10) + digit;
			if ((nkind / 10) != kind)
			{
				ofc_sparse_ref_error(literal->src,
					"Kind out of range");
				return NULL;
			}

			kind = nkind;
		}

		if (kind == 0)
		{
			ofc_sparse_ref_error(literal->src,
				"Literal kind must be non-zero");
			return NULL;
		}

		if (type->kind != kind)
		{
			ofc_sparse_ref_error(literal->src,
				"Expected kind doesn't match literal kind");
			return NULL;
		}
	}
	if (kind == 0)
		kind = 1;

	if (i < size)
	{
		/* This should never happen, and represents
		   a mismatch between the parser and semantic rules. */
		return NULL;
	}

	if (is_byte && (kind != 1) && (kind != 3))
	{
		ofc_sparse_ref_error(literal->src,
			"Byte can never have a KIND larger than 1 byte");
		return NULL;
	}

	if (!typeval.type)
	{
		typeval.type = ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_INTEGER, kind);

		if (!typeval.type)
		{
			/* This should never happen. */
			return NULL;
		}
	}

	if (!ofc_sema_typeval__in_range(&typeval))
	{
		ofc_sparse_ref_error(literal->src,
			"Out of range for type");
		return NULL;
	}

	typeval.src = literal->src;

	return ofc_sema_typeval__alloc(typeval);
}


static bool ofc_sema_typeval__real(
	const ofc_parse_literal_t* literal,
	ofc_str_ref_t number, unsigned  ikind,
	long double*  value , unsigned* okind)
{
	if (!value)
		return false;

	const char* ptr  = number.base;
	unsigned    size = number.size;

	if (!ptr || (size == 0))
		return false;

	unsigned i = 0;
	bool negate = (ptr[i] == '-');
	if (negate || (ptr[i] == '+'))
		i += 1;

	*value = 0.0;
	for (; (i < size) && isdigit(ptr[i]); i++)
	{
		unsigned digit = (ptr[i] - '0');
		*value *= 10.0;
		*value += digit;
	}

	if ((i < size) && (ptr[i] == '.'))
	{
		i++;

		long double f;
		for (f = 0.1; (i < size) && isdigit(ptr[i]); i++, f /= 10.0)
		{
			unsigned digit = (ptr[i] - '0');
			*value += (digit * f);
		}
	}

	if (negate)
		*value = -*value;

	unsigned kind = 0;
	if ((i < size) && isalpha(ptr[i]))
	{
		switch (toupper(ptr[i++]))
		{
			case 'E':
				kind = 1;
				break;
			case 'D':
				kind = 2;
				break;
			case 'Q':
				kind = 4;
				break;
			default:
				return false;
		}

		negate = ((i < size) && (ptr[i] == '-'));
		if (negate || ((i < size) && (ptr[i] == '+')))
			i += 1;

		long double e = 0.0;
		for (; (i < size) && isdigit(ptr[i]); i++)
		{
			unsigned digit = (ptr[i] - '0');
			e *= 10.0;
			e += digit;
		}

		if ((i < size) && (ptr[i] == '.'))
		{
			i++;

			long double f;
			for (f = 0.1; (i < size) && isdigit(ptr[i]); i++, f /= 10.0)
			{
				unsigned digit = (ptr[i] - '0');
				e += (digit * f);
			}
		}

		if (negate)
			e = -e;

		*value *= powl(10.0, e);
	}

	if ((i < size) && (ptr[i] == '_'))
	{
		i++;

		if ((i >= size)
			|| (!isdigit(ptr[i])))
			return false;

		unsigned ukind = 0;
		for (; (i < size) && isdigit(ptr[i]); i++)
		{
			unsigned digit = (ptr[i] - '0');
			unsigned nkind = (ukind * 10) + digit;
			if ((nkind / 10) != ukind)
			{
				ofc_sparse_ref_error(literal->src,
					"Kind out of range");
				return false;
			}

			ukind = nkind;
		}

		if ((kind != 0)
			&& (kind != ukind))
		{
			ofc_sparse_ref_error(literal->src,
				"Kinds specified in exponent and F90 style don't agree");
			return false;
		}

		kind = ukind;
	}

	if ((ikind != 0) && (ikind != kind))
	{
		ofc_sparse_ref_error(literal->src,
			"Expected kind doesn't match literal kind");
		return false;
	}
	kind = (kind > 0 ? kind : ikind);

	if (kind == 0)
		kind = 1;

	unsigned ksize;
	if (!ofc_sema_type_kind_size(
		ofc_target_real_size_get(), kind, &ksize))
	{
		ofc_sparse_ref_error(literal->src,
			"Invalid REAL kind in literal");
		return false;
	}

	if (ksize > sizeof(*value))
	{
		ofc_sparse_ref_error(literal->src,
			"REAL kind too large");
		return false;
	}

	if (i < size)
	{
		/* This should never happen, and represents
		   a mismatch between the parser and semantic rules. */
		return false;
	}

	if (okind) *okind = kind;
	return true;
}

static ofc_sema_typeval_t* ofc_sema_typeval__real_literal(
	const ofc_parse_literal_t* literal,
	const ofc_sema_type_t* type)
{
	if (!literal)
		return NULL;

	if (type && (type->type
		!= OFC_SEMA_TYPE_REAL))
		return NULL;

	/* TODO - Handle BOZ literals and Holleriths. */
	if (literal->type != OFC_PARSE_LITERAL_NUMBER)
		return NULL;

	unsigned tkind = (type ? type->kind : 0);

	ofc_sema_typeval_t typeval = { .type = type };

	unsigned kind = 0;
	if (!ofc_sema_typeval__real(
		literal, literal->number, tkind,
		&typeval.real, &kind))
		return NULL;

	if (!typeval.type)
	{
		typeval.type = ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_REAL, kind);

		if (!typeval.type)
		{
			/* This should never happen. */
			return NULL;
		}
	}

	typeval.src = literal->src;

	return ofc_sema_typeval__alloc(typeval);
}

static ofc_sema_typeval_t* ofc_sema_typeval__complex_literal(
	const ofc_parse_literal_t* literal,
	const ofc_sema_type_t* type)
{
	if (!literal)
		return NULL;

	if (type && (type->type
		!= OFC_SEMA_TYPE_COMPLEX))
		return NULL;

	if (literal->type != OFC_PARSE_LITERAL_COMPLEX)
		return NULL;

	unsigned tkind = (type ? type->kind : 0);

	ofc_sema_typeval_t typeval = { .type = type };

	unsigned rkind = 0;
	unsigned ikind = 0;

	if (!ofc_sema_typeval__real(
		literal, literal->complex.real, tkind,
		&typeval.complex.real, &rkind)
		|| !ofc_sema_typeval__real(
			literal, literal->complex.imaginary, tkind,
			&typeval.complex.imaginary, &ikind))
		return NULL;

	unsigned rsize, isize;
	if (!ofc_sema_type_kind_size(
		ofc_target_real_size_get(), rkind, &rsize)
		|| !ofc_sema_type_kind_size(
			ofc_target_real_size_get(), ikind, &isize))
		return NULL;

	unsigned kind = (rsize > isize ? rkind : ikind);

	if (!typeval.type)
	{
		typeval.type = ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_COMPLEX, kind);

		if (!typeval.type)
		{
			/* This should never happen. */
			return NULL;
		}
	}

	typeval.src = literal->src;

	return ofc_sema_typeval__alloc(typeval);
}

static ofc_sema_typeval_t* ofc_sema_typeval__character_literal(
	const ofc_parse_literal_t* literal,
	const ofc_sema_type_t* type)
{
	if (!literal)
		return NULL;

	bool is_byte = false;
	if (type)
	{
		switch (type->type)
		{
			case OFC_SEMA_TYPE_CHARACTER:
				break;
			case OFC_SEMA_TYPE_BYTE:
				is_byte = true;
				break;
			default:
				return NULL;
		}
	}

	switch (literal->type)
	{
		case OFC_PARSE_LITERAL_CHARACTER:
		case OFC_PARSE_LITERAL_HOLLERITH:
			break;
		default:
			return NULL;
	}

	if (!literal->string->base)
		return NULL;

	unsigned size = literal->string->size;

	if (type)
	{
		unsigned ksize;
		if (!ofc_sema_type_base_size(type, &ksize))
			return NULL;

		if (ksize > 1)
		{
			ofc_sparse_ref_error(literal->src,
				"Wide strings not supported");
			return NULL;
		}

		if (!ofc_sema_type_size(type, &size))
		{
			ofc_sparse_ref_error(literal->src,
				"Can't create variable length constant");
			return NULL;
		}
	}

	ofc_sema_typeval_t typeval = { .type = type };

	if (!typeval.type)
	{
		typeval.type = ofc_sema_type_create_character(1, size);
		if (!typeval.type)
		{
			/* This should never happen. */
			return NULL;
		}
	}

	if (is_byte)
	{
		if (size != 1)
			return NULL;

		typeval.integer = literal->string->base[0];
	}
	else if (size == 0)
	{
		typeval.character = NULL;
	}
	else
	{
		typeval.character = (char*)malloc(sizeof(char) * size);
		if (!typeval.character)
			return NULL;

		if (literal->string->size > size)
		{
			memcpy(
				typeval.character,
				literal->string->base, size);
			ofc_sparse_ref_warning(literal->src,
				"String truncated");
		}
		else
		{
			memcpy(
				typeval.character,
				literal->string->base,
				literal->string->size);

			if (literal->string->size < size)
			{
				unsigned offset = literal->string->size;
				unsigned ssize = (size - offset);
				memset(&typeval.character[offset], ' ', ssize);

				ofc_sparse_ref_warning(literal->src,
					"String padded");
			}
		}
	}

	typeval.src = literal->src;

	ofc_sema_typeval_t* atv
		= ofc_sema_typeval__alloc(typeval);
	if (!atv && !is_byte) free(typeval.character);
	return atv;
}

static ofc_sema_typeval_t* ofc_sema_typeval__logical_literal(
	const ofc_parse_literal_t* literal,
	const ofc_sema_type_t* type)
{
	if (!literal)
		return NULL;

	bool is_byte = false;
	if (type)
	{
		switch (type->type)
		{
			case OFC_SEMA_TYPE_LOGICAL:
				break;
			case OFC_SEMA_TYPE_BYTE:
				is_byte = true;
				break;
			default:
				return NULL;
		}
	}

	if (literal->type
		!= OFC_PARSE_LITERAL_LOGICAL)
		return NULL;

	ofc_sema_typeval_t typeval = { .type = type };

	if (!typeval.type)
	{
		typeval.type = ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_LOGICAL, 1);

		if (!typeval.type)
		{
			/* This should never happen. */
			return NULL;
		}
	}

	if (is_byte)
		typeval.integer = literal->logical;
	else
		typeval.logical = literal->logical;

	typeval.src = literal->src;

	return ofc_sema_typeval__alloc(typeval);
}

static ofc_sema_typeval_t* ofc_sema_typeval__byte_literal(
	const ofc_parse_literal_t* literal,
	const ofc_sema_type_t* type)
{
	if (!literal)
		return NULL;

	if (type && (type->type
		!= OFC_SEMA_TYPE_BYTE))
		return NULL;

	switch (literal->type)
	{
		case OFC_PARSE_LITERAL_NUMBER:
		case OFC_PARSE_LITERAL_BINARY:
		case OFC_PARSE_LITERAL_OCTAL:
		case OFC_PARSE_LITERAL_HEX:
			return ofc_sema_typeval__integer_literal(
				literal, type);
		case OFC_PARSE_LITERAL_LOGICAL:
			return ofc_sema_typeval__logical_literal(
				literal, type);
		case OFC_PARSE_LITERAL_CHARACTER:
			return ofc_sema_typeval__character_literal(
				literal, type);
		default:
			break;
	}

	return NULL;
}


ofc_sema_typeval_t* ofc_sema_typeval_create_unsigned(
	unsigned value, ofc_sparse_ref_t ref)
{
	unsigned kind = 12;
	if ((value >> 31) != 0)
		kind = 24;

	const ofc_sema_type_t* type
		= ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_INTEGER, kind);
	if (!type) return NULL;

	ofc_sema_typeval_t* typeval
		= (ofc_sema_typeval_t*)malloc(
			sizeof(ofc_sema_typeval_t));
	if (!typeval) return NULL;

	typeval->type = type;
	typeval->integer = value;
	typeval->src = ref;
	return typeval;
}

ofc_sema_typeval_t* ofc_sema_typeval_create_integer(
	int value, ofc_sparse_ref_t ref)
{
	unsigned kind = 12;

	const ofc_sema_type_t* type
		= ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_INTEGER, kind);
	if (!type) return NULL;

	ofc_sema_typeval_t* typeval
		= (ofc_sema_typeval_t*)malloc(
			sizeof(ofc_sema_typeval_t));
	if (!typeval) return NULL;

	typeval->type = type;
	typeval->integer = value;
	typeval->src = ref;
	return typeval;
}


ofc_sema_typeval_t* ofc_sema_typeval_literal(
	const ofc_parse_literal_t* literal,
	const ofc_sema_type_t* type)
{
	if (!literal) return NULL;

	if (!type)
	{
		ofc_sema_typeval_t* typeval = NULL;
		if (!typeval) typeval = ofc_sema_typeval__integer_literal(literal, type);
		if (!typeval) typeval = ofc_sema_typeval__real_literal(literal, type);
		if (!typeval) typeval = ofc_sema_typeval__logical_literal(literal, type);
		if (!typeval) typeval = ofc_sema_typeval__complex_literal(literal, type);
		/* Byte can never be auto-detected. */
		if (!typeval) typeval = ofc_sema_typeval__character_literal(literal, type);

		return typeval;
	}

	switch (type->type)
	{
		case OFC_SEMA_TYPE_LOGICAL:
			return ofc_sema_typeval__logical_literal(literal, type);
		case OFC_SEMA_TYPE_INTEGER:
			return ofc_sema_typeval__integer_literal(literal, type);
		case OFC_SEMA_TYPE_REAL:
			return ofc_sema_typeval__real_literal(literal, type);
		case OFC_SEMA_TYPE_COMPLEX:
			return ofc_sema_typeval__complex_literal(literal, type);
		case OFC_SEMA_TYPE_BYTE:
			return ofc_sema_typeval__byte_literal(literal, type);
		case OFC_SEMA_TYPE_CHARACTER:
			return ofc_sema_typeval__character_literal(literal, type);

		default:
			return NULL;
	}
}

void ofc_sema_typeval_delete(
	ofc_sema_typeval_t* typeval)
{
	if (!typeval)
		return;

	if (typeval->type
		&& (typeval->type->type == OFC_SEMA_TYPE_CHARACTER))
		free(typeval->character);

	free(typeval);
}


bool ofc_sema_typeval_compare(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b)
{
	if (!a || !b)
		return false;

	if (a == b)
		return true;

	if (!ofc_sema_type_compatible(
		a->type, b->type))
		return false;

	switch (a->type->type)
	{
		case OFC_SEMA_TYPE_LOGICAL:
			return (a->logical == b->logical);
		case OFC_SEMA_TYPE_INTEGER:
			return (a->integer == b->integer);
		case OFC_SEMA_TYPE_REAL:
			return (a->real == b->real);
		case OFC_SEMA_TYPE_COMPLEX:
			return ((a->complex.real == b->complex.real)
				&& (a->complex.imaginary == b->complex.imaginary));
		case OFC_SEMA_TYPE_BYTE:
			return ((a->integer & 0xFF) == (b->integer & 0xFF));
		case OFC_SEMA_TYPE_CHARACTER:
			{
				unsigned size;
				if (!ofc_sema_type_size(
					a->type, &size))
					return false;
				return (memcmp(a->character,
					b->character, size) == 0);
			}
		default:
			break;
	}

	return false;
}



unsigned ofc_sema_typeval_size(
	const ofc_sema_typeval_t* typeval)
{
	if (!typeval)
		return 0;
	unsigned size = 0;
	ofc_sema_type_size(typeval->type, &size);
	return size;
}

ofc_sema_typeval_t* ofc_sema_typeval_copy(
	const ofc_sema_typeval_t* typeval)
{
	if (!typeval || !typeval->type)
		return NULL;

	ofc_sema_typeval_t* copy
		= (ofc_sema_typeval_t*)malloc(
			sizeof(ofc_sema_typeval_t));
	if (!copy) return NULL;

	memcpy(copy, typeval,
		sizeof(ofc_sema_typeval_t));

	if (copy->type->type == OFC_SEMA_TYPE_CHARACTER)
	{
		unsigned size = ofc_sema_typeval_size(typeval);
		copy->character = NULL;
		if (size > 0)
		{
			copy->character = malloc(size);
			if (!copy->character)
			{
				free(copy);
				return NULL;
			}

			memcpy(copy->character,
				typeval->character, size);
		}
	}

	return copy;
}

ofc_sema_typeval_t* ofc_sema_typeval_cast(
	const ofc_sema_typeval_t* typeval,
	const ofc_sema_type_t* type)
{
	if (!typeval || !typeval->type)
		return NULL;
	if (!type || ofc_sema_type_compare(type, typeval->type))
		return ofc_sema_typeval_copy(typeval);

	ofc_sema_typeval_t tv;
	tv.type = type;
	tv.src  = typeval->src;

	unsigned tsize, csize;
	if (!ofc_sema_type_base_size(typeval->type, &tsize)
		|| !ofc_sema_type_base_size(type, &csize))
		return NULL;

	if ((type->type == OFC_SEMA_TYPE_CHARACTER)
		&& (typeval->type->type == OFC_SEMA_TYPE_CHARACTER))
	{
		unsigned len_tval = typeval->type->len;
		unsigned len_type = type->len;

		if (tsize > csize)
		{
			ofc_sparse_ref_error(typeval->src,
				"Can't cast CHARACTER to a smaller kind.");
			return NULL;
		}
		else if (tsize < csize)
		{
			tv.character = (char*)malloc(len_type * csize);

			unsigned wchar;
			for (wchar = 0; wchar < len_type; wchar += csize)
			{
				memcpy(&tv.character[wchar], typeval->character, tsize);

				unsigned wchar_pad;
				for (wchar_pad = 1; wchar_pad < csize; wchar_pad++)
					tv.character[wchar + wchar_pad] = '\0';
			}

			if (len_tval < len_type)
			{
				unsigned pad_char, pad_byte;
				for (pad_char = len_tval; pad_char < len_type;
					pad_char += csize)
				{
					tv.character[pad_char] = ' ';
					for(pad_byte = 1; pad_byte < csize; pad_byte++)
					{
						tv.character[pad_char + pad_byte] = '\0';
					}
				}
			}
		}
		else
		{
			tv.character = (char*)malloc(len_type * csize);

			if (len_tval < len_type)
			{
				memcpy(tv.character, typeval->character,
					(len_tval * csize));

				unsigned pad_char, pad_byte;
				for (pad_char = (len_tval * csize); pad_char < (len_type * csize);
					pad_char += csize)
				{
					tv.character[pad_char] = ' ';
					for(pad_byte = 1; pad_byte < csize; pad_byte++)
					{
						tv.character[pad_char + pad_byte] = '\0';
					}
				}
			}
			else
			{
				memcpy(tv.character, typeval->character,
					(len_type * csize));
			}
		}

		return ofc_sema_typeval__alloc(tv);
	}

	bool invalid_cast = false;
	bool large_literal = false;
	bool lossy_cast = false;

	switch (type->type)
	{
		case OFC_SEMA_TYPE_LOGICAL:
		case OFC_SEMA_TYPE_BYTE:
			break;
		case OFC_SEMA_TYPE_INTEGER:
			large_literal = (csize > 8);
			break;
		case OFC_SEMA_TYPE_REAL:
		case OFC_SEMA_TYPE_COMPLEX:
			large_literal = (csize > 10);
			break;
		default:
			invalid_cast = true;
			break;
	}

	if (large_literal)
	{
		ofc_sparse_ref_error(typeval->src,
			"Literal too large for compiler");
		return NULL;
	}

	if (type->type == typeval->type->type)
		return ofc_sema_typeval_copy(typeval);

	switch (type->type)
	{
		case OFC_SEMA_TYPE_LOGICAL:
			switch (typeval->type->type)
			{
				case OFC_SEMA_TYPE_INTEGER:
				case OFC_SEMA_TYPE_BYTE:
					tv.logical = (typeval->integer != 0);
					break;
				default:
					invalid_cast = true;
					break;
			}
			break;

		case OFC_SEMA_TYPE_INTEGER:
			switch (typeval->type->type)
			{
				case OFC_SEMA_TYPE_LOGICAL:
					tv.integer = (typeval->logical ? 1 : 0);
					break;
				case OFC_SEMA_TYPE_REAL:
					tv.integer = (int64_t)typeval->real;
					if ((long double)tv.integer != typeval->real)
						lossy_cast = true;
					break;
				case OFC_SEMA_TYPE_COMPLEX:
					tv.integer = (int64_t)typeval->complex.real;
					if (((long double)tv.integer != typeval->complex.real)
						|| (typeval->complex.imaginary != 0.0))
						lossy_cast = true;
					break;
				case OFC_SEMA_TYPE_BYTE:
					tv.integer = typeval->integer;
					break;
				default:
					invalid_cast = true;
					break;
			}

			if (csize < 8)
			{
				int64_t imax = 1LL << ((csize * 8) - 1);
				if ((tv.integer < -imax)
					|| (tv.integer >= imax))
				{
					lossy_cast = true;

					int64_t sign_mask = -1LL;
					sign_mask ^= (imax - 1);

					tv.integer = (tv.integer < 0
						? (tv.integer | sign_mask)
						: (tv.integer & (imax - 1)));
				}
			}
			break;

		case OFC_SEMA_TYPE_REAL:
			switch (typeval->type->type)
			{
				case OFC_SEMA_TYPE_INTEGER:
				case OFC_SEMA_TYPE_BYTE:
					tv.real = (long double)typeval->integer;
					break;
				case OFC_SEMA_TYPE_COMPLEX:
					tv.real = typeval->complex.real;
					if (typeval->complex.imaginary != 0.0)
						lossy_cast = true;
					break;
				default:
					invalid_cast = true;
					break;
			}
			break;

		case OFC_SEMA_TYPE_COMPLEX:
			switch (typeval->type->type)
			{
				case OFC_SEMA_TYPE_INTEGER:
				case OFC_SEMA_TYPE_BYTE:
					tv.complex.real = (long double)typeval->integer;
					break;
				case OFC_SEMA_TYPE_REAL:
					tv.complex.real = typeval->real;
					break;
				default:
					invalid_cast = true;
					break;
			}
			tv.complex.imaginary = 0.0;
			break;

		case OFC_SEMA_TYPE_BYTE:
			switch (typeval->type->type)
			{
				case OFC_SEMA_TYPE_LOGICAL:
					tv.integer = (typeval->logical ? 1 : 0);
					break;
				case OFC_SEMA_TYPE_REAL:
					tv.integer = (int64_t)typeval->real;
					if ((long double)tv.integer != typeval->real)
						lossy_cast = true;
					break;
				case OFC_SEMA_TYPE_COMPLEX:
					tv.integer = (int64_t)typeval->complex.real;
					if (((long double)tv.integer != typeval->complex.real)
						|| (typeval->complex.imaginary != 0.0))
						lossy_cast = true;
					break;
				case OFC_SEMA_TYPE_INTEGER:
				case OFC_SEMA_TYPE_BYTE:
					tv.integer = typeval->integer;
					break;
				default:
					invalid_cast = true;
					break;
			}
			if ((tv.integer < -128)
				|| (tv.integer >= 128))
			{
				lossy_cast = true;
				tv.integer = (tv.integer < 0
					? (tv.integer | 0xFFFFFFFFFFFFFF00LL)
					: (tv.integer & 0xFF));
			}
			break;

		default:
			invalid_cast = true;
			break;
	}

	if (invalid_cast)
	{
		ofc_sparse_ref_error(typeval->src,
			"Can't cast %s to %s",
			ofc_sema_type_str_rep(typeval->type),
			ofc_sema_type_str_rep(type));
		return NULL;
	}

	if (lossy_cast)
	{
		ofc_sparse_ref_warning(typeval->src,
			"Cast from %s to %s was lossy",
			ofc_sema_type_str_rep(typeval->type),
			ofc_sema_type_str_rep(type));
	}

	return ofc_sema_typeval__alloc(tv);
}


bool ofc_sema_typeval_get_logical(
	const ofc_sema_typeval_t* typeval,
	bool* logical)
{
	if (!typeval || !typeval->type
		|| !ofc_sema_type_is_logical(typeval->type))
		return false;

	/* TODO - Casting. */

	if (logical)
	{
		if (typeval->type->type
			== OFC_SEMA_TYPE_BYTE)
			*logical = (typeval->integer != 0);
		else
			*logical = typeval->logical;
	}

	return true;
}

bool ofc_sema_typeval_get_integer(
	const ofc_sema_typeval_t* typeval,
	int64_t* integer)
{
	if (!typeval || !typeval->type)
		return false;

	if (!ofc_sema_type_is_integer(typeval->type))
	{
		const ofc_sema_type_t* ptype
			= ofc_sema_type_promote(typeval->type,
				ofc_sema_type_integer_default());

		ofc_sema_typeval_t* tv
			= ofc_sema_typeval_cast(typeval, ptype);
		if (!tv) return false;

		if (integer)
			*integer = tv->integer;
		ofc_sema_typeval_delete(tv);
		return true;
	}

	if (integer)
		*integer = typeval->integer;
	return true;
}

bool ofc_sema_typeval_get_real(
	const ofc_sema_typeval_t* typeval,
	long double* real)
{
	if (!typeval || !typeval->type
		|| (typeval->type->type != OFC_SEMA_TYPE_REAL))
		return false;

	if (real)
		*real = typeval->real;
	return true;
}

bool ofc_sema_typeval_get_complex(
	const ofc_sema_typeval_t* typeval,
	long double* real, long double* imaginary)
{
	if (!typeval || !typeval->type
		|| (typeval->type->type != OFC_SEMA_TYPE_COMPLEX))
		return false;

	/* TODO - Casting. */

	if (real)
		*real = typeval->complex.real;
	if (imaginary)
		*real = typeval->complex.imaginary;
	return true;
}

bool ofc_sema_typeval_get_character(
	const ofc_sema_typeval_t* typeval,
	const char** character)
{
	if (!typeval || !typeval->type)
		return false;

	if (typeval->type->type
		== OFC_SEMA_TYPE_BYTE)
	{
		if (character)
			*character = (const char*)&typeval->integer;
		return true;
	}

	/* TODO - Casting. */

	if (typeval->type->type
		!= OFC_SEMA_TYPE_CHARACTER)
		return false;

	if (character)
		*character = typeval->character;
	return true;
}



static bool ofc_typeval_character_equal__strz(
	const ofc_sema_typeval_t* tv, const char* strz,
	bool case_sensitive)
{
	if (!tv || !strz
		|| !ofc_sema_type_is_character(tv->type))
		return false;

	unsigned size;
	if (!ofc_sema_type_base_size(tv->type, &size)
		|| (size != 1))
		return false;

	unsigned slen = strlen(strz);
	unsigned tlen = tv->type->len;

	if (tlen != slen)
		return false;

	return ((case_sensitive
		? strncmp(tv->character, strz, slen)
		: strncasecmp(tv->character, strz, slen)) == 0);
}

bool ofc_typeval_character_equal_strz(
	const ofc_sema_typeval_t* tv, const char* strz)
{
	return ofc_typeval_character_equal__strz(tv, strz, true);
}

bool ofc_typeval_character_equal_strz_ci(
	const ofc_sema_typeval_t* tv, const char* strz)
{
	return ofc_typeval_character_equal__strz(tv, strz, false);
}



ofc_sema_typeval_t* ofc_sema_typeval_power(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b)
{
	if (!a || !a->type
		|| !b || !b->type
		|| !ofc_sema_type_compatible(
			a->type, b->type))
		return NULL;

	ofc_sema_typeval_t tv;
	tv.type = a->type;

	if (!ofc_sparse_ref_bridge(
		a->src, b->src, &tv.src))
		return NULL;

	switch (a->type->type)
	{
		case OFC_SEMA_TYPE_REAL:
			tv.real = powl(a->real, b->real);
			break;
		case OFC_SEMA_TYPE_COMPLEX:
			{
				long double abs = hypotl(a->complex.real, a->complex.imaginary);
				if (abs == 0.0)
				{
					tv.complex.real      = 0.0;
					tv.complex.imaginary = 0.0;
					break;
				}
				long double arg = atan2l(a->complex.imaginary, a->complex.real);
				long double radio = powl(abs, b->complex.real);
				long double ang = arg * b->complex.real;
				if (b->complex.imaginary)
				{
					radio = radio * exp(-b->complex.imaginary * arg);
					ang = ang + (b->complex.imaginary * log(abs));
				}
				tv.complex.real = radio * cos(ang);
				tv.complex.imaginary = radio * sin(ang);
			}
			break;
		case OFC_SEMA_TYPE_INTEGER:
		case OFC_SEMA_TYPE_BYTE:
			{
				long double r = powl(
					a->integer, b->integer);
				tv.integer = (int64_t)r;
				if ((long double)tv.integer != r)
					return NULL;
			}
			break;
		default:
			return NULL;
	}

	return ofc_sema_typeval__alloc(tv);
}

ofc_sema_typeval_t* ofc_sema_typeval_multiply(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b)
{
	if (!a || !a->type
		|| !b || !b->type
		|| !ofc_sema_type_compatible(
			a->type, b->type))
		return NULL;

	ofc_sema_typeval_t tv;
	tv.type = a->type;

	if (!ofc_sparse_ref_bridge(
		a->src, b->src, &tv.src))
		return NULL;

	switch (a->type->type)
	{
		case OFC_SEMA_TYPE_REAL:
			tv.real = a->real * b->real;
			break;
		case OFC_SEMA_TYPE_COMPLEX:
			tv.complex.real = (a->complex.real * b->complex.real)
				- (a->complex.imaginary * b->complex.imaginary);
			tv.complex.imaginary = (a->complex.real * b->complex.imaginary)
				+ (b->complex.real * a->complex.imaginary);
			break;
		case OFC_SEMA_TYPE_INTEGER:
		case OFC_SEMA_TYPE_BYTE:
			tv.integer = a->integer * b->integer;
			if ((b->integer != 0)
				&& ((tv.integer / b->integer) != a->integer))
				return NULL;
			break;
		default:
			return NULL;
	}

	return ofc_sema_typeval__alloc(tv);
}

ofc_sema_typeval_t* ofc_sema_typeval_concat(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b)
{
	if (!a || !a->type
		|| !b || !b->type
		|| (a->type->type != OFC_SEMA_TYPE_CHARACTER)
		|| (b->type->type != OFC_SEMA_TYPE_CHARACTER))
		return NULL;

	unsigned asize, bsize;
	if (!ofc_sema_type_base_size(a->type, &asize)
		|| !ofc_sema_type_base_size(b->type, &bsize)
		|| (asize != bsize))
		return NULL;

	if (a->type->len == 0)
		return ofc_sema_typeval_copy(b);
	if (b->type->len == 0)
		return ofc_sema_typeval_copy(a);

	unsigned len_a = a->type->len * asize;
	unsigned len_b = b->type->len * bsize;

	unsigned len = len_a + len_b;

	ofc_sema_typeval_t tv;
	tv.type = ofc_sema_type_create_character(
		a->type->kind, len);
	if (!tv.type) return NULL;

	if (!ofc_sparse_ref_bridge(
		a->src, b->src, &tv.src))
		return NULL;

	tv.character = (char*)malloc(sizeof(char) * len);
	if (!tv.character) return NULL;

	memcpy(tv.character, a->character, len_a);
	memcpy(&tv.character[len_a], b->character, len_b);

	ofc_sema_typeval_t* ret
		= ofc_sema_typeval__alloc(tv);
	if (!ret) free(tv.character);
	return ret;
}

ofc_sema_typeval_t* ofc_sema_typeval_divide(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b)
{
	if (!a || !a->type
		|| !b || !b->type
		|| !ofc_sema_type_compatible(
			a->type, b->type))
		return NULL;

	ofc_sema_typeval_t tv;
	tv.type = a->type;

	if (!ofc_sparse_ref_bridge(
		a->src, b->src, &tv.src))
		return NULL;

	switch (a->type->type)
	{
		case OFC_SEMA_TYPE_REAL:
			tv.real = a->real / b->real;
			break;
		case OFC_SEMA_TYPE_COMPLEX:
			{
				long double div = powl(b->complex.real, 2.0)
					+ powl(b->complex.imaginary, 2.0);
				tv.complex.real
					= ((a->complex.real * b->complex.real)
						+ (a->complex.imaginary * b->complex.imaginary)) / div;
				tv.complex.imaginary
					= ((a->complex.imaginary * b->complex.real)
						- (a->complex.real * b->complex.imaginary)) / div;
			}
			break;
		case OFC_SEMA_TYPE_INTEGER:
		case OFC_SEMA_TYPE_BYTE:
			if (b->integer == 0)
			{
				ofc_sparse_ref_error(a->src,
					"Divide by zero");
				return NULL;
			}
			tv.integer = a->integer / b->integer;
			break;
		default:
			return NULL;
	}

	return ofc_sema_typeval__alloc(tv);
}

ofc_sema_typeval_t* ofc_sema_typeval_add(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b)
{
	if (!a || !a->type
		|| !b || !b->type
		|| !ofc_sema_type_compatible(
			a->type, b->type))
		return NULL;

	ofc_sema_typeval_t tv;
	tv.type = a->type;

	if (!ofc_sparse_ref_bridge(
		a->src, b->src, &tv.src))
		return NULL;

	switch (a->type->type)
	{
		case OFC_SEMA_TYPE_REAL:
			tv.real = a->real + b->real;
			break;
		case OFC_SEMA_TYPE_COMPLEX:
			tv.complex.real = a->complex.real + b->complex.real;
			tv.complex.imaginary
				= a->complex.imaginary + b->complex.imaginary;
				break;
		case OFC_SEMA_TYPE_INTEGER:
		case OFC_SEMA_TYPE_BYTE:
			/* TODO - Detect overflow. */
			tv.integer = a->integer + b->integer;
			break;
		default:
			return NULL;
	}

	return ofc_sema_typeval__alloc(tv);
}

ofc_sema_typeval_t* ofc_sema_typeval_subtract(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b)
{
	if (!a || !a->type
		|| !b || !b->type
		|| !ofc_sema_type_compatible(
			a->type, b->type))
		return NULL;

	ofc_sema_typeval_t tv;
	tv.type = a->type;

	if (!ofc_sparse_ref_bridge(
		a->src, b->src, &tv.src))
		return NULL;

	switch (a->type->type)
	{
		case OFC_SEMA_TYPE_REAL:
			tv.real = a->real - b->real;
			break;
		case OFC_SEMA_TYPE_COMPLEX:
			tv.complex.real = a->complex.real - b->complex.real;
			tv.complex.imaginary
				= a->complex.imaginary - b->complex.imaginary;
			break;
		case OFC_SEMA_TYPE_INTEGER:
		case OFC_SEMA_TYPE_BYTE:
			/* TODO - Detect overflow. */
			tv.integer = a->integer - b->integer;
			break;
		default:
			return NULL;
	}

	return ofc_sema_typeval__alloc(tv);
}

ofc_sema_typeval_t* ofc_sema_typeval_negate(
	const ofc_sema_typeval_t* a)
{
	if (!a || !a->type)
		return NULL;

	ofc_sema_typeval_t tv;
	tv.type = a->type;
	tv.src  = a->src;

	switch (a->type->type)
	{
		case OFC_SEMA_TYPE_REAL:
			tv.real = -a->real;
			break;
		case OFC_SEMA_TYPE_COMPLEX:
			tv.complex.real = -a->complex.real;
			tv.complex.imaginary = -a->complex.imaginary;
			break;
		case OFC_SEMA_TYPE_INTEGER:
		case OFC_SEMA_TYPE_BYTE:
			tv.integer = -a->integer;
			if (-tv.integer != a->integer)
			{
				ofc_sparse_ref_error(a->src,
					"Overflow in constant negate");
				return NULL;
			}
			break;
		default:
			return NULL;
	}

	return ofc_sema_typeval__alloc(tv);
}

ofc_sema_typeval_t* ofc_sema_typeval_eq(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b)
{
	if (!a || !a->type
		|| !b || !b->type
		|| !ofc_sema_type_compatible(
			a->type, b->type))
		return NULL;

	ofc_sema_typeval_t tv;
	tv.type = ofc_sema_type_create_primitive(
		OFC_SEMA_TYPE_LOGICAL, 1);

	if (!ofc_sparse_ref_bridge(
		a->src, b->src, &tv.src))
		return NULL;

	if (a->type->type == OFC_SEMA_TYPE_CHARACTER)
	{
		if (b->type->type != OFC_SEMA_TYPE_CHARACTER)
			return NULL;

		if (a->type->len != b->type->len)
			return NULL;

		/* TODO - Support comparison of types with differing character sizes. */
		unsigned asize, bsize;
		if (!ofc_sema_type_base_size(a->type, &asize)
			|| !ofc_sema_type_base_size(b->type, &bsize)
			|| (asize != bsize))
			return NULL;

		tv.logical = (memcmp(a->character, b->character,
			(a->type->len * asize)) == 0);
	}
	else
	{
		switch (a->type->type)
		{
			case OFC_SEMA_TYPE_REAL:
				tv.logical = (a->real == b->real);
				break;
			case OFC_SEMA_TYPE_COMPLEX:
				tv.logical = ((a->complex.real == b->complex.real)
					&& (a->complex.imaginary == b->complex.imaginary));
				break;
			case OFC_SEMA_TYPE_INTEGER:
			case OFC_SEMA_TYPE_BYTE:
				tv.logical = (a->integer == b->integer);
				break;
			default:
				return NULL;
		}
	}

	return ofc_sema_typeval__alloc(tv);
}

ofc_sema_typeval_t* ofc_sema_typeval_ne(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b)
{
	if (!a || !a->type
		|| !b || !b->type
		|| !ofc_sema_type_compatible(
			a->type, b->type))
		return NULL;

	ofc_sema_typeval_t tv;
	tv.type = ofc_sema_type_create_primitive(
		OFC_SEMA_TYPE_LOGICAL, 1);

	if (!ofc_sparse_ref_bridge(
		a->src, b->src, &tv.src))
		return NULL;

	if (a->type->type == OFC_SEMA_TYPE_CHARACTER)
	{
		if (b->type->type != OFC_SEMA_TYPE_CHARACTER)
			return NULL;

		if (a->type->len != b->type->len)
			return NULL;

		/* TODO - Support comparison of types with differing character sizes. */
		unsigned asize, bsize;
		if (!ofc_sema_type_base_size(a->type, &asize)
			|| !ofc_sema_type_base_size(b->type, &bsize)
			|| (asize != bsize))
			return NULL;

		tv.logical = (memcmp(a->character, b->character,
			(a->type->len * asize)) != 0);
	}
	else
	{
		switch (a->type->type)
		{
			case OFC_SEMA_TYPE_REAL:
				tv.logical = (a->real != b->real);
				break;
			case OFC_SEMA_TYPE_COMPLEX:
				tv.logical = ((a->complex.real != b->complex.real)
								|| (a->complex.imaginary != b->complex.imaginary));
				break;
			case OFC_SEMA_TYPE_INTEGER:
			case OFC_SEMA_TYPE_BYTE:
				tv.logical = (a->integer != b->integer);
				break;
			default:
				return NULL;
		}
	}

	return ofc_sema_typeval__alloc(tv);
}

ofc_sema_typeval_t* ofc_sema_typeval_lt(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b)
{
	if (!a || !a->type
		|| !b || !b->type
		|| !ofc_sema_type_compatible(
			a->type, b->type))
		return NULL;

	ofc_sema_typeval_t tv;
	tv.type = ofc_sema_type_create_primitive(
		OFC_SEMA_TYPE_LOGICAL, 1);

	if (!ofc_sparse_ref_bridge(
		a->src, b->src, &tv.src))
		return NULL;

	if (a->type->type == OFC_SEMA_TYPE_CHARACTER)
	{
		if (b->type->type != OFC_SEMA_TYPE_CHARACTER)
			return NULL;

		if (a->type->len != b->type->len)
			return NULL;

		/* TODO - Support comparison of types with differing character sizes. */
		unsigned asize, bsize;
		if (!ofc_sema_type_base_size(a->type, &asize)
			|| !ofc_sema_type_base_size(b->type, &bsize)
			|| (asize != bsize))
			return NULL;

		/* Strings of characters larger than 8-bytes aren't suported. */
		if (asize > 8)
			return NULL;

		tv.logical = false;
		unsigned i, j;
		for (i = 0, j = 0; i < a->type->len; i++, j += asize)
		{
			uint64_t ac = 0, bc = 0;
			memcpy(&ac, &a->character[j], asize);
			memcpy(&bc, &b->character[j], asize);

			if (ac == bc)
				continue;

			tv.logical = (ac < bc);
			break;
		}
	}
	else
	{
		switch (a->type->type)
		{
			case OFC_SEMA_TYPE_REAL:
				tv.logical = (a->real < b->real);
				break;
			case OFC_SEMA_TYPE_INTEGER:
			case OFC_SEMA_TYPE_BYTE:
				tv.logical = (a->integer < b->integer);
				break;
			default:
				return NULL;
		}
	}

	return ofc_sema_typeval__alloc(tv);
}

ofc_sema_typeval_t* ofc_sema_typeval_le(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b)
{
	if (!a || !a->type
		|| !b || !b->type
		|| !ofc_sema_type_compatible(
			a->type, b->type))
		return NULL;

	ofc_sema_typeval_t tv;
	tv.type = ofc_sema_type_create_primitive(
		OFC_SEMA_TYPE_LOGICAL, 1);

	if (!ofc_sparse_ref_bridge(
		a->src, b->src, &tv.src))
		return NULL;

	if (a->type->type == OFC_SEMA_TYPE_CHARACTER)
	{
		if (b->type->type != OFC_SEMA_TYPE_CHARACTER)
			return NULL;

		if (a->type->len != b->type->len)
			return NULL;

		/* TODO - Support comparison of types with differing character sizes. */
		unsigned asize, bsize;
		if (!ofc_sema_type_base_size(a->type, &asize)
			|| !ofc_sema_type_base_size(b->type, &bsize)
			|| (asize != bsize))
			return NULL;

		/* Strings of characters larger than 8-bytes aren't suported. */
		if (asize > 8)
			return NULL;

		tv.logical = true;
		unsigned i, j;
		for (i = 0, j = 0; i < a->type->len; i++, j += asize)
		{
			uint64_t ac = 0, bc = 0;
			memcpy(&ac, &a->character[j], asize);
			memcpy(&bc, &b->character[j], asize);

			if (ac == bc)
				continue;

			tv.logical = (ac < bc);
			break;
		}
	}
	else
	{
		switch (a->type->type)
		{
			case OFC_SEMA_TYPE_REAL:
				tv.logical = (a->real <= b->real);
				break;
			case OFC_SEMA_TYPE_INTEGER:
			case OFC_SEMA_TYPE_BYTE:
				tv.logical = (a->integer <= b->integer);
				break;
			default:
				return NULL;
		}
	}

	return ofc_sema_typeval__alloc(tv);
}

ofc_sema_typeval_t* ofc_sema_typeval_gt(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b)
{
	if (!a || !a->type
		|| !b || !b->type
		|| !ofc_sema_type_compatible(
			a->type, b->type))
		return NULL;

	ofc_sema_typeval_t tv;
	tv.type = ofc_sema_type_create_primitive(
		OFC_SEMA_TYPE_LOGICAL, 1);

	if (!ofc_sparse_ref_bridge(
		a->src, b->src, &tv.src))
		return NULL;

	if (a->type->type == OFC_SEMA_TYPE_CHARACTER)
	{
		if (b->type->type != OFC_SEMA_TYPE_CHARACTER)
			return NULL;

		if (a->type->len != b->type->len)
			return NULL;

		/* TODO - Support comparison of types with differing character sizes. */
		unsigned asize, bsize;
		if (!ofc_sema_type_base_size(a->type, &asize)
			|| !ofc_sema_type_base_size(b->type, &bsize)
			|| (asize != bsize))
			return NULL;

		/* Strings of characters larger than 8-bytes aren't suported. */
		if (asize > 8)
			return NULL;

		tv.logical = false;
		unsigned i, j;
		for (i = 0, j = 0; i < a->type->len; i++, j += asize)
		{
			uint64_t ac = 0, bc = 0;
			memcpy(&ac, &a->character[j], asize);
			memcpy(&bc, &b->character[j], asize);

			if (ac == bc)
				continue;

			tv.logical = (ac > bc);
			break;
		}
	}
	else
	{
		switch (a->type->type)
		{
			case OFC_SEMA_TYPE_REAL:
				tv.logical = (a->real > b->real);
				break;
			case OFC_SEMA_TYPE_INTEGER:
			case OFC_SEMA_TYPE_BYTE:
				tv.logical = (a->integer > b->integer);
				break;
			default:
				return NULL;
		}
	}

	return ofc_sema_typeval__alloc(tv);
}

ofc_sema_typeval_t* ofc_sema_typeval_ge(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b)
{
	if (!a || !a->type
		|| !b || !b->type
		|| !ofc_sema_type_compatible(
			a->type, b->type))
		return NULL;

	ofc_sema_typeval_t tv;
	tv.type = ofc_sema_type_create_primitive(
		OFC_SEMA_TYPE_LOGICAL, 1);

	if (!ofc_sparse_ref_bridge(
		a->src, b->src, &tv.src))
		return NULL;

	if (a->type->type == OFC_SEMA_TYPE_CHARACTER)
	{
		if (b->type->type != OFC_SEMA_TYPE_CHARACTER)
			return NULL;

		if (a->type->len != b->type->len)
			return NULL;

		/* TODO - Support comparison of types with differing character sizes. */
		unsigned asize, bsize;
		if (!ofc_sema_type_base_size(a->type, &asize)
			|| !ofc_sema_type_base_size(b->type, &bsize)
			|| (asize != bsize))
			return NULL;

		/* Strings of characters larger than 8-bytes aren't suported. */
		if (asize > 8)
			return NULL;

		tv.logical = true;
		unsigned i, j;
		for (i = 0, j = 0; i < a->type->len; i++, j += asize)
		{
			uint64_t ac = 0, bc = 0;
			memcpy(&ac, &a->character[j], asize);
			memcpy(&bc, &b->character[j], asize);

			if (ac == bc)
				continue;

			tv.logical = (ac > bc);
			break;
		}
	}
	else
	{
		switch (a->type->type)
		{
			case OFC_SEMA_TYPE_REAL:
				tv.logical = (a->real >= b->real);
				break;
			case OFC_SEMA_TYPE_INTEGER:
			case OFC_SEMA_TYPE_BYTE:
				tv.logical = (a->integer >= b->integer);
				break;
			default:
				return NULL;
		}
	}

	return ofc_sema_typeval__alloc(tv);
}

ofc_sema_typeval_t* ofc_sema_typeval_not(
	const ofc_sema_typeval_t* a)
{
	if (!a || !a->type)
		return NULL;

	ofc_sema_typeval_t tv;
	tv.type = a->type;
	tv.src  = a->src;

	switch (a->type->type)
	{
		case OFC_SEMA_TYPE_LOGICAL:
			tv.logical = !a->logical;
			break;
		case OFC_SEMA_TYPE_INTEGER:
			tv.integer = ~a->integer;
			break;
		default:
			return NULL;
	}

	return ofc_sema_typeval__alloc(tv);
}

ofc_sema_typeval_t* ofc_sema_typeval_and(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b)
{
	if (!a || !a->type
		|| !b || !b->type
		|| !ofc_sema_type_compatible(
			a->type, b->type))
		return NULL;

	ofc_sema_typeval_t tv;
	tv.type = a->type;

	if (!ofc_sparse_ref_bridge(
		a->src, b->src, &tv.src))
		return NULL;

	switch (a->type->type)
	{
		case OFC_SEMA_TYPE_LOGICAL:
			tv.logical = (a->logical && b->logical);
			break;
		case OFC_SEMA_TYPE_INTEGER:
		case OFC_SEMA_TYPE_BYTE:
			tv.integer = a->integer & b->integer;
			break;
		default:
			return NULL;
	}

	return ofc_sema_typeval__alloc(tv);
}

ofc_sema_typeval_t* ofc_sema_typeval_or(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b)
{
	if (!a || !a->type
		|| !b || !b->type
		|| !ofc_sema_type_compatible(
			a->type, b->type))
		return NULL;

	ofc_sema_typeval_t tv;
	tv.type = a->type;

	if (!ofc_sparse_ref_bridge(
		a->src, b->src, &tv.src))
		return NULL;

	switch (a->type->type)
	{
		case OFC_SEMA_TYPE_LOGICAL:
			tv.logical = (a->logical || b->logical);
			break;
		case OFC_SEMA_TYPE_INTEGER:
		case OFC_SEMA_TYPE_BYTE:
			tv.integer = a->integer | b->integer;
			break;
		default:
			return NULL;
	}

	return ofc_sema_typeval__alloc(tv);
}

ofc_sema_typeval_t* ofc_sema_typeval_eqv(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b)
{
	if (!a || !a->type
		|| !b || !b->type
		|| !ofc_sema_type_compatible(
			a->type, b->type))
		return NULL;

	ofc_sema_typeval_t tv;
	tv.type = ofc_sema_type_create_primitive(
		OFC_SEMA_TYPE_LOGICAL, 1);

	if (!ofc_sparse_ref_bridge(
		a->src, b->src, &tv.src))
		return NULL;

	switch (a->type->type)
	{
		case OFC_SEMA_TYPE_LOGICAL:
			tv.logical = (a->logical == b->logical);
			break;
		case OFC_SEMA_TYPE_BYTE:
			tv.logical = (a->integer == b->integer);
			break;
		default:
			return NULL;
	}

	return ofc_sema_typeval__alloc(tv);
}

ofc_sema_typeval_t* ofc_sema_typeval_neqv(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b)
{
	if (!a || !a->type
		|| !b || !b->type
		|| !ofc_sema_type_compatible(
			a->type, b->type))
		return NULL;

	ofc_sema_typeval_t tv;
	tv.type = ofc_sema_type_create_primitive(
		OFC_SEMA_TYPE_LOGICAL, 1);

	if (!ofc_sparse_ref_bridge(
		a->src, b->src, &tv.src))
		return NULL;

	switch (a->type->type)
	{
		case OFC_SEMA_TYPE_LOGICAL:
			tv.logical = (a->logical != b->logical);
			break;
		case OFC_SEMA_TYPE_BYTE:
			tv.logical = (a->integer != b->integer);
			break;
		default:
			return NULL;
	}

	return ofc_sema_typeval__alloc(tv);
}


#include <float.h>

bool ofc_sema_typeval_print(ofc_colstr_t*cs,
	const ofc_sema_typeval_t* typeval)
{
	if (!cs || !typeval)
		return false;

	switch (typeval->type->type)
	{
		case OFC_SEMA_TYPE_LOGICAL:
			if (typeval->logical)
				return ofc_colstr_atomic_writef(cs, ".TRUE.");
			else
				return ofc_colstr_atomic_writef(cs, ".FALSE.");

		case OFC_SEMA_TYPE_BYTE:
		case OFC_SEMA_TYPE_INTEGER:
			return ofc_colstr_atomic_writef(cs, "%d",
				typeval->integer);

		case OFC_SEMA_TYPE_REAL:
			{
				if (!ofc_sparse_ref_empty(typeval->src))
					return ofc_sparse_ref_print(cs, typeval->src);

				char fmt[32];
				sprintf(fmt, "%%.%uLG", LDBL_DIG);

				char buff[64];
				sprintf(buff, fmt, typeval->real);

				unsigned i;
				for (i = 0; buff[i] != '\0'; i++)
				{
					if ((buff[i] == '.')
						|| (tolower(buff[i]) == 'e'))
						break;
				}
				if (buff[i] == '\0')
					strcat(buff, ".0");

				return ofc_colstr_atomic_writef(cs, "%s", buff);
			}

		case OFC_SEMA_TYPE_COMPLEX:
			if (!ofc_colstr_atomic_writef(cs, "(")
				|| !ofc_colstr_atomic_writef(cs, "%Lg",
					typeval->complex.real)
				|| !ofc_colstr_atomic_writef(cs, ",")
				|| !ofc_colstr_atomic_writef(cs, " ")
				|| !ofc_colstr_atomic_writef(cs, "%Lg",
					typeval->complex.imaginary)
				|| !ofc_colstr_atomic_writef(cs, ")"))
				return false;
			return true;

		case OFC_SEMA_TYPE_CHARACTER:
			return ofc_colstr_writef(cs, "\"%.*s\"",
				typeval->type->len, typeval->character);

		default:
			break;

	}

	return false;
}
