#include "parse.h"
#include <ctype.h>
#include <string.h>

static const char* parse_type__name[] =
{
	"NONE",
	"LOGICAL",
	"CHARACTER",
	"INTEGER",
	"REAL",
	"DOUBLE PRECISION",
	"COMPLEX",
	"DOUBLE COMPLEX",
	"BYTE",
};

static unsigned parse_decl_attr(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_decl_attr_t* attr)
{
	attr->is_static    = false;
	attr->is_automatic = false;
	attr->is_volatile  = false;

	unsigned i = 0;
	while (true)
	{
		unsigned l;
		l = parse_keyword(
			src, &ptr[i], debug,
			PARSE_KEYWORD_STATIC);
		if (l > 0)
		{
			if (attr->is_static)
			{
				parse_debug_warning(debug, src, &ptr[i],
					"Duplicate definition of STATIC decl attribute");
			}

			attr->is_static = true;
			i += l;
			continue;
		}

		l = parse_keyword(
			src, &ptr[i], debug,
			PARSE_KEYWORD_AUTOMATIC);
		if (l > 0)
		{
			if (attr->is_automatic)
			{
				parse_debug_warning(debug, src, &ptr[i],
					"Duplicate definition of AUTOMATIC decl attribute");
			}

			attr->is_automatic = true;
			i += l;
			continue;
		}

		l = parse_keyword(
			src, &ptr[i], debug,
			PARSE_KEYWORD_VOLATILE);
		if (l > 0)
		{
			if (attr->is_volatile)
			{
				parse_debug_warning(debug, src, &ptr[i],
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
	parse_debug_t* debug,
	unsigned* len)
{
	unsigned dpos = parse_debug_position(debug);

	parse_type_t type;
	unsigned i = parse_decl_attr(
		src, ptr, debug, &type.attr);

	unsigned j;
	for (j = 0; parse_type__keyword_map[j].type != PARSE_TYPE_NONE; j++)
	{
		i = parse_keyword(src, &ptr[i], debug,
			parse_type__keyword_map[j].keyword);

		type.type = parse_type__keyword_map[j].type;

		if (i > 0) break;
	}

	if (i == 0)
	{
		parse_debug_rewind(debug, dpos);
		return NULL;
	}

	type.count_expr = NULL;
	type.count_var = false;
	type.kind = 0;

	if (((type.type == PARSE_TYPE_CHARACTER)
			|| (type.type == PARSE_TYPE_BYTE))
		&& (ptr[i] == '*'))
	{
		unsigned l = parse_star_len(
			src, &ptr[i], debug,
			&type.count_expr,
			&type.count_var);
		if (l == 0)
		{
			sparse_error(src, &ptr[i],
				"Expected count expression or value for character");
			return NULL;
		}
		i += l;
	}
	else if (ptr[i] == '*')
	{
		i += 1;
		unsigned l = parse_unsigned(
			src, &ptr[i], debug, &type.kind);
		if (l == 0)
		{
			sparse_error(src, &ptr[i],
				"Expected kind value after asterisk in type specifier");
			return NULL;
		}
		i += l;

		if (type.kind == 0)
		{
			parse_debug_warning(debug, src, &ptr[i],
				"Kind value must be non-zero, using default");
		}
	}

	type.params = NULL;
	if (ptr[i] == '(')
	{
		unsigned l;
		type.params = parse_call_arg_list_force_named(
			src, &ptr[i + 1], debug, &l);
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
		parse_debug_rewind(debug, dpos);
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

bool parse_type_print(int fd, const parse_type_t* type)
{
	if (type->type >= PARSE_TYPE_COUNT)
		return false;

	if (!dprintf_bool(fd, "%s",
		parse_type__name[type->type]))
		return false;

	if ((type->kind > 0)
		|| type->count_expr
		|| type->count_var)
	{
		if (!dprintf_bool(fd, " ("))
			return false;

		if ((type->kind > 0)
			&& !dprintf_bool(fd, "KIND=%u", type->kind))
			return false;

		if (type->count_expr || type->count_var)
		{
			if ((type->kind > 0)
				&& !dprintf_bool(fd, ", "))
				return false;

			if (!dprintf_bool(fd, "LEN="))
				return false;

			if (!(type->count_var
				? dprintf_bool(fd, "*")
				: parse_expr_print(fd, type->count_expr)))
				return false;
		}

		if (!dprintf_bool(fd, ")"))
			return false;
	}

	return dprintf_bool(fd, " ::");
}
