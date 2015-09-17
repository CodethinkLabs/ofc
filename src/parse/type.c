#include "parse.h"
#include <ctype.h>
#include <string.h>

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


unsigned parse_type(
	const sparse_t* src, const char* ptr,
	parse_type_t* type)
{
	unsigned i = 0;

	parse_type_e t   = PARSE_TYPE_NONE;
	bool         dbl = false;

	unsigned j;
	for (j = 0; parse_type__keyword_map[j].type != PARSE_TYPE_NONE; j++)
	{
		i = parse_keyword(src, &ptr[i],
			parse_type__keyword_map[j].keyword);

		t   = parse_type__keyword_map[j].type;
		dbl = parse_type__keyword_map[j].dbl;

		if (i > 0) break;
	}

	if (i == 0)
		return 0;

	bool implicit_kind = false;
	unsigned k;
	if ((PARSE_TYPE_CHARACTER == t
			|| PARSE_TYPE_BYTE == t)
			&& (ptr[i] == '*'))
	{
		i += 1;
		parse_expr_t *count_expr;
		count_expr = (parse_expr_t*)malloc(sizeof(parse_expr_t));
		if (ptr[i] == '(')
		{
			i += 1;

			unsigned len = parse_expr(src, &ptr[i], count_expr);
			if (len == 0)
			{
				sparse_error(src, &ptr[i],
					"Expected count expression or value for character");
				return 0;
			}
			i += len;

			if (ptr[i++] != ')')
			{
				parse_expr_cleanup(*count_expr);
				return 0;
			}
		}
		else
		{
			unsigned len = parse_expr_literal(src, &ptr[i], count_expr);
			if (len == 0)
			{
				sparse_error(src, &ptr[i],
					"Expected count expression or value for character");
				return 0;
			}
			i += len;
		}

		type->type  = t;
		type->kind  = 1;
		type->count_expr = count_expr;
		return i;
	}
	else if (ptr[i] == '*')
	{
		i += 1;
		unsigned len = parse_unsigned(
			src, &ptr[i], &k);
		if (len == 0)
		{
			sparse_error(src, &ptr[i],
				"Expected kind value after asterisk in type specifier");
			return 0;
		}
		i += len;
	}
	else if (ptr[i] == '(')
	{
		i += 1;

		unsigned len = parse_keyword(src, ptr, PARSE_KEYWORD_KIND);
		if (len > 0)
		{
			i += len;
			if (ptr[i] != '=')
			{
				sparse_error(src, ptr,
					"Expected '=' after KIND in type specifier");
				return 0;
			}
			i += 1;
		}

		len = parse_unsigned(
			src, &ptr[i], &k);
		if (len == 0)
		{
			sparse_error(src, &ptr[i],
				"Expected kind value after 'KIND=' in type specifier");
			return 0;
		}
		i += len;

		if (ptr[i] == ')')
		{
			sparse_error(src, &ptr[i],
				"Epected closing bracket in kind declaration");
		}
		i += 1;
	}
	else
	{
		implicit_kind = true;
		k = 4;
	}

	if (k == 0)
	{
		sparse_warning(src, ptr,
			"Kind value must be non-zero, using default");
		k = 4;
	}

	if (dbl && !implicit_kind)
	{
		sparse_warning(src, ptr,
			"DOUBLE types shouldn't have a kind, doubling kind value");
	}

	if (dbl) k *= 2;

	type->type  = t;
	type->kind  = k;
	type->count_expr = NULL;
	return i;
}

void parse_type_cleanup(
	parse_type_t type)
{
	if (type.count_expr)
		parse_expr_cleanup(*type.count_expr);
}

bool parse_type_clone(
	parse_type_t* dst, const parse_type_t* src)
{
	if (!dst || !src)
		return false;

	parse_type_t clone = *src;

	if (src->count_expr)
	{
		clone.count_expr
			= (parse_expr_t*)malloc(
				sizeof(parse_expr_t));
		if (!clone.count_expr)
			return false;

		if (!parse_expr_clone(
			clone.count_expr,
			src->count_expr))
		{
			free(clone.count_expr);
			return false;
		}
	}

	*dst = clone;
	return true;
}
