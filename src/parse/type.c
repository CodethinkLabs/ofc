#include "parse.h"
#include <ctype.h>
#include <string.h>


static unsigned parse_decl_attr(
	const sparse_t* src, const char* ptr,
	parse_decl_attr_t* attr)
{
	attr->is_static    = false;
	attr->is_automatic = false;
	attr->is_volatile  = false;

	unsigned i = 0;
	while (true)
	{
		unsigned l;
		l = parse_keyword(src, &ptr[i],
			PARSE_KEYWORD_STATIC);
		if (l > 0)
		{
			if (attr->is_static)
			{
				sparse_warning(src, &ptr[i],
					"Duplicate definition of STATIC decl attribute");
			}

			attr->is_static = true;
			i += l;
			continue;
		}

		l = parse_keyword(src, &ptr[i],
			PARSE_KEYWORD_AUTOMATIC);
		if (l > 0)
		{
			if (attr->is_automatic)
			{
				sparse_warning(src, &ptr[i],
					"Duplicate definition of AUTOMATIC decl attribute");
			}

			attr->is_automatic = true;
			i += l;
			continue;
		}

		l = parse_keyword(src, &ptr[i],
			PARSE_KEYWORD_VOLATILE);
		if (l > 0)
		{
			if (attr->is_volatile)
			{
				sparse_warning(src, &ptr[i],
					"Duplicate definition of VOLATILE decl attribute");
			}

			attr->is_volatile = true;
			i += l;
			continue;
		}

		break;
	}

	return i;
}

parse_type_t* parse_type__alloc(parse_type_t type)
{
	parse_type_t* atype
		= (parse_type_t*)malloc(
			sizeof(parse_type_t));
	if (!atype) return NULL;

	*atype = type;
	return atype;
}

void parse_type__cleanup(
	parse_type_t type)
{
	parse_expr_delete(type.count_expr);
	parse_call_arg_list_delete(type.params);
}



typedef struct
{
	parse_type_e    type;
	parse_keyword_e keyword;
} parse_type__keyword_t;

static const parse_type__keyword_t parse_type__keyword_map[] =
{
	{ PARSE_TYPE_LOGICAL         , PARSE_KEYWORD_LOGICAL          },
	{ PARSE_TYPE_CHARACTER       , PARSE_KEYWORD_CHARACTER        },
	{ PARSE_TYPE_INTEGER         , PARSE_KEYWORD_INTEGER          },
	{ PARSE_TYPE_REAL            , PARSE_KEYWORD_REAL             },
	{ PARSE_TYPE_COMPLEX         , PARSE_KEYWORD_COMPLEX          },
	{ PARSE_TYPE_BYTE            , PARSE_KEYWORD_BYTE             },
	{ PARSE_TYPE_DOUBLE_PRECISION, PARSE_KEYWORD_DOUBLE_PRECISION },
	{ PARSE_TYPE_DOUBLE_COMPLEX  , PARSE_KEYWORD_DOUBLE_COMPLEX   },
	{ PARSE_TYPE_NONE            , 0 },
};

parse_type_t* parse_type(
	const sparse_t* src, const char* ptr,
	unsigned* len)
{
	parse_type_t type;
	unsigned i = parse_decl_attr(
		src, ptr, &type.attr);

	unsigned j;
	for (j = 0; parse_type__keyword_map[j].type != PARSE_TYPE_NONE; j++)
	{
		i = parse_keyword(src, &ptr[i],
			parse_type__keyword_map[j].keyword);

		type.type = parse_type__keyword_map[j].type;

		if (i > 0) break;
	}

	if (i == 0)
		return NULL;

	type.count_expr = NULL;
	type.count_var = false;
	type.kind = 0;

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
				if (ptr[i] == '*')
				{
					type.count_var = true;
					l = 1;
				}
				else
				{
					sparse_error(src, &ptr[i],
						"Expected count expression or value for character");
					return NULL;
				}
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

		if (type.kind == 0)
		{
			sparse_warning(src, ptr,
				"Kind value must be non-zero, using default");
		}
	}

	type.params = NULL;
	if (ptr[i] == '(')
	{
		unsigned l;
		type.params = parse_call_arg_list_force_named(
			src, &ptr[i + 1], &l);
		if (type.params && (ptr[i + 1 + l] == ')'))
		{
			i += (l + 2);
		}
		else
		{
			parse_call_arg_list_delete(
				type.params);
		}
	}

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



void parse_type_delete(parse_type_t* type)
{
	if (!type)
		return;

	parse_type__cleanup(*type);
	free(type);
}
