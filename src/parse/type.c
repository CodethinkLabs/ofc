#include "parse.h"
#include <ctype.h>
#include <string.h>


parse_type_t* parse_type__alloc(parse_type_t type)
{
	parse_type_t* atype
		= (parse_type_t*)malloc(
			sizeof(parse_type_t));
	if (!atype) return NULL;

	*atype = type;
	atype->ref = 0;
	return atype;
}

void parse_type__cleanup(
	parse_type_t type)
{
	parse_expr_delete(type.count_expr);
}

bool parse_type__clone(
	parse_type_t* dst, const parse_type_t* src)
{
	if (!dst || !src)
		return false;

	parse_type_t clone = *src;

	if (src->count_expr)
	{
		clone.count_expr = parse_expr_copy(
			src->count_expr);
		if (!clone.count_expr)
			return false;
	}

	*dst = clone;
	return true;
}



typedef struct
{
	parse_type_e    type;
	bool            dbl;
	parse_keyword_e keyword;
} parse_type__keyword_t;

static const parse_type__keyword_t parse_type__keyword_map[] =
{
	{ PARSE_TYPE_LOGICAL  , 0, PARSE_KEYWORD_LOGICAL          },
	{ PARSE_TYPE_CHARACTER, 0, PARSE_KEYWORD_CHARACTER        },
	{ PARSE_TYPE_INTEGER  , 0, PARSE_KEYWORD_INTEGER          },
	{ PARSE_TYPE_REAL     , 0, PARSE_KEYWORD_REAL             },
	{ PARSE_TYPE_COMPLEX  , 0, PARSE_KEYWORD_COMPLEX          },
	{ PARSE_TYPE_BYTE     , 0, PARSE_KEYWORD_BYTE             },
	{ PARSE_TYPE_REAL     , 1, PARSE_KEYWORD_DOUBLE_PRECISION },
	{ PARSE_TYPE_COMPLEX  , 1, PARSE_KEYWORD_DOUBLE_COMPLEX   },
	{ PARSE_TYPE_NONE     , 0, 0 },
};

parse_type_t* parse_type(
	const sparse_t* src, const char* ptr,
	unsigned* len)
{
	unsigned i = 0;

	parse_type_t type;
	bool dbl = false;

	unsigned j;
	for (j = 0; parse_type__keyword_map[j].type != PARSE_TYPE_NONE; j++)
	{
		i = parse_keyword(src, &ptr[i],
			parse_type__keyword_map[j].keyword);

		type.type = parse_type__keyword_map[j].type;
		dbl = parse_type__keyword_map[j].dbl;

		if (i > 0) break;
	}

	if (i == 0)
		return NULL;

	type.count_expr = NULL;

	bool implicit_kind = true;
	if (((type.type == PARSE_TYPE_CHARACTER)
			|| (type.type == PARSE_TYPE_BYTE))
		&& (ptr[i] == '*'))
	{
		i += 1;

		if (ptr[i] == '(')
		{
			i += 1;

			unsigned l;
			type.count_expr = parse_expr(
				src, &ptr[i], &l);
			if (!type.count_expr)
			{
				sparse_error(src, &ptr[i],
					"Expected count expression or value for character");
				return NULL;
			}
			i += l;

			if (ptr[i++] != ')')
			{
				parse_expr_delete(type.count_expr);
				return NULL;
			}
		}
		else
		{
			unsigned l;
			type.count_expr = parse_expr_literal(
				src, &ptr[i], &l);
			if (!type.count_expr)
			{
				sparse_error(src, &ptr[i],
					"Expected count expression or value for character");
				return NULL;
			}
			i += l;
		}

		type.kind = 1;
	}
	else if (ptr[i] == '*')
	{
		i += 1;
		unsigned l = parse_unsigned(
			src, &ptr[i], &type.kind);
		if (l == 0)
		{
			sparse_error(src, &ptr[i],
				"Expected kind value after asterisk in type specifier");
			return NULL;
		}
		i += l;

		implicit_kind = false;
	}
	else if (ptr[i] == '(')
	{
		i += 1;

		unsigned l = parse_keyword(src, ptr, PARSE_KEYWORD_KIND);
		if (l > 0)
		{
			i += l;
			if (ptr[i] != '=')
			{
				sparse_error(src, ptr,
					"Expected '=' after KIND in type specifier");
				return NULL;
			}
			i += 1;
		}

		l = parse_unsigned(
			src, &ptr[i], &type.kind);
		if (l == 0)
		{
			sparse_error(src, &ptr[i],
				"Expected kind value after 'KIND=' in type specifier");
			return NULL;
		}
		i += l;

		if (ptr[i] == ')')
		{
			sparse_error(src, &ptr[i],
				"Expected closing bracket in kind declaration");
			return NULL;
		}
		i += 1;

		implicit_kind = false;
	}
	else
	{
		type.kind = 4;
	}

	if (type.kind == 0)
	{
		sparse_warning(src, ptr,
			"Kind value must be non-zero, using default");
		type.kind = 4;
	}

	if (dbl && !implicit_kind)
	{
		sparse_warning(src, ptr,
			"DOUBLE types shouldn't have a kind, doubling kind value");
	}

	if (dbl) type.kind *= 2;

	parse_type_t* atype
		= parse_type__alloc(type);
	if (!atype)
	{
		parse_type__cleanup(type);
		return NULL;
	}

	if (len) *len = i;
	return atype;
}



bool parse_type_reference(parse_type_t* type)
{
	if (!type)
		return 0;

	unsigned nref = type->ref + 1;
	if (nref == 0) return false;

	type->ref += 1;
	return true;
}

parse_type_t* parse_type_copy(const parse_type_t* type)
{
	if (!type)
		return NULL;

	parse_type_t clone;
	if (!parse_type__clone(&clone, type))
		return NULL;

	parse_type_t* copy
		= parse_type__alloc(clone);
	if (!copy)
	{
		parse_type__cleanup(clone);
		return NULL;
	}

	return copy;
}

void parse_type_delete(parse_type_t* type)
{
	if (!type)
		return;

	if (type->ref > 0)
	{
		type->ref -= 1;
		return;
	}

	parse_type__cleanup(*type);
	free(type);
}
