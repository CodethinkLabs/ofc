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
#include <string.h>

#include "ofc/parse.h"

static const char* ofc_parse_type__name[] =
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
	"TYPE",
};

const char* ofc_parse_type_str_rep(
	const ofc_parse_type_e type)
{
	if (type >= OFC_PARSE_TYPE_COUNT)
		return NULL;

	return ofc_parse_type__name[type];
}

static unsigned ofc_parse_decl_attr(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_decl_attr_t* attr)
{
	attr->is_static    = false;
	attr->is_automatic = false;
	attr->is_volatile  = false;

	unsigned i = 0;
	while (true)
	{
		unsigned l;
		l = ofc_parse_keyword(
			src, &ptr[i], debug,
			OFC_PARSE_KEYWORD_STATIC);
		if (l > 0)
		{
			if (attr->is_static)
			{
				ofc_parse_debug_warning(debug,
					ofc_sparse_ref(src, &ptr[i], l),
					"Duplicate definition of STATIC decl attribute");
			}

			attr->is_static = true;
			i += l;
			continue;
		}

		l = ofc_parse_keyword(
			src, &ptr[i], debug,
			OFC_PARSE_KEYWORD_AUTOMATIC);
		if (l > 0)
		{
			if (attr->is_automatic)
			{
				ofc_parse_debug_warning(debug,
					ofc_sparse_ref(src, &ptr[i], l),
					"Duplicate definition of AUTOMATIC decl attribute");
			}

			attr->is_automatic = true;
			i += l;
			continue;
		}

		l = ofc_parse_keyword(
			src, &ptr[i], debug,
			OFC_PARSE_KEYWORD_VOLATILE);
		if (l > 0)
		{
			if (attr->is_volatile)
			{
				ofc_parse_debug_warning(debug,
					ofc_sparse_ref(src, &ptr[i], l),
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

ofc_parse_type_t* ofc_parse_type__alloc(ofc_parse_type_t type)
{
	ofc_parse_type_t* atype
		= (ofc_parse_type_t*)malloc(
			sizeof(ofc_parse_type_t));
	if (!atype) return NULL;

	*atype = type;
	return atype;
}

void ofc_parse_type__cleanup(
	ofc_parse_type_t type)
{
	ofc_parse_expr_delete(type.count_expr);
	ofc_parse_call_arg_list_delete(type.params);
}



typedef struct
{
	ofc_parse_type_e    type;
	ofc_parse_keyword_e keyword;
} ofc_parse_type__keyword_t;

static const ofc_parse_type__keyword_t ofc_parse_type__keyword_map[] =
{
	{ OFC_PARSE_TYPE_LOGICAL         , OFC_PARSE_KEYWORD_LOGICAL          },
	{ OFC_PARSE_TYPE_CHARACTER       , OFC_PARSE_KEYWORD_CHARACTER        },
	{ OFC_PARSE_TYPE_INTEGER         , OFC_PARSE_KEYWORD_INTEGER          },
	{ OFC_PARSE_TYPE_REAL            , OFC_PARSE_KEYWORD_REAL             },
	{ OFC_PARSE_TYPE_COMPLEX         , OFC_PARSE_KEYWORD_COMPLEX          },
	{ OFC_PARSE_TYPE_BYTE            , OFC_PARSE_KEYWORD_BYTE             },
	{ OFC_PARSE_TYPE_DOUBLE_PRECISION, OFC_PARSE_KEYWORD_DOUBLE_PRECISION },
	{ OFC_PARSE_TYPE_DOUBLE_COMPLEX  , OFC_PARSE_KEYWORD_DOUBLE_COMPLEX   },
	{ OFC_PARSE_TYPE_TYPE            , OFC_PARSE_KEYWORD_TYPE             },
	{ OFC_PARSE_TYPE_NONE            , 0 },
};

ofc_parse_type_t* ofc_parse_type(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	ofc_parse_type_t type;
	unsigned i = ofc_parse_decl_attr(
		src, ptr, debug, &type.attr);

	type.type = OFC_PARSE_TYPE_NONE;
	unsigned j;
	for (j = 0; ofc_parse_type__keyword_map[j].type != OFC_PARSE_TYPE_NONE; j++)
	{
		unsigned len = ofc_parse_keyword(src, &ptr[i], debug,
			ofc_parse_type__keyword_map[j].keyword);
		if (len == 0) continue;

		type.type = ofc_parse_type__keyword_map[j].type;
		i += len;
		break;
	}

	if (type.type == OFC_PARSE_TYPE_NONE)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}

	type.type_name  = OFC_SPARSE_REF_EMPTY;
	type.count_expr = NULL;
	type.count_var  = false;
	type.kind       = 0;

	if (type.type == OFC_PARSE_TYPE_TYPE)
	{
		if (ptr[i++] != '(')
		{
			ofc_parse_debug_rewind(debug, dpos);
			return 0;
		}

		unsigned l = ofc_parse_name(
			src, &ptr[i], debug,
			&type.type_name);
		if (l == 0)
		{
			ofc_parse_debug_rewind(debug, dpos);
			return 0;
		}
		i += l;

		if (ptr[i++] != ')')
		{
			ofc_parse_debug_rewind(debug, dpos);
			return 0;
		}
	}
	else if (((type.type == OFC_PARSE_TYPE_CHARACTER)
			|| (type.type == OFC_PARSE_TYPE_BYTE))
		&& (ptr[i] == '*'))
	{
		unsigned l = ofc_parse_star_len(
			src, &ptr[i], debug,
			&type.count_expr,
			&type.count_var);
		if (l == 0)
		{
			ofc_sparse_error_ptr(src, &ptr[i],
				"Expected count expression or value for character");
			return NULL;
		}
		i += l;
	}
	else if (ptr[i] == '*')
	{
		i += 1;
		unsigned l = ofc_parse_unsigned(
			src, &ptr[i], debug, &type.kind);
		if (l == 0)
		{
			ofc_sparse_error_ptr(src, &ptr[i],
				"Expected kind value after asterisk in type specifier");
			return NULL;
		}

		if (type.kind == 0)
		{
			ofc_parse_debug_warning(debug,
				ofc_sparse_ref(src, &ptr[i], l),
				"Kind value must be non-zero, using default");
		}
		i += l;
	}

	type.params = NULL;
	if (ptr[i] == '(')
	{
		unsigned l;
		type.params = ofc_parse_call_arg_list_force_named(
			src, &ptr[i + 1], debug, &l);
		if (type.params && (ptr[i + 1 + l] == ')'))
		{
			i += (l + 2);
		}
		else
		{
			ofc_parse_call_arg_list_delete(
				type.params);
		}
	}

	type.src = ofc_sparse_ref(src, ptr, i);

	ofc_parse_type_t* atype
		= ofc_parse_type__alloc(type);
	if (!atype)
	{
		ofc_parse_type__cleanup(type);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}

	if (len) *len = i;
	return atype;
}



void ofc_parse_type_delete(ofc_parse_type_t* type)
{
	if (!type)
		return;

	ofc_parse_type__cleanup(*type);
	free(type);
}

bool ofc_parse_type_print(
	ofc_colstr_t* cs, const ofc_parse_type_t* type, bool colons)
{
	if (ofc_parse_type_print_f77(cs, type))
		return true;

	if (type->type >= OFC_PARSE_TYPE_COUNT)
		return false;

	if (!ofc_colstr_atomic_writef(cs, "%s",
		ofc_parse_type_str_rep(type->type)))
		return false;

	if (type->type == OFC_PARSE_TYPE_TYPE)
	{
		if (!ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_atomic_writef(cs, "(")
			|| !ofc_sparse_ref_print(cs, type->type_name)
			|| !ofc_colstr_atomic_writef(cs, ")"))
			return false;
	}

	if ((type->kind > 0)
		|| type->count_expr
		|| type->count_var)
	{
		if (!ofc_colstr_atomic_writef(cs, " ("))
			return false;

		if ((type->kind > 0)
			&& !ofc_colstr_atomic_writef(cs, "KIND=%u", type->kind))
			return false;

		if (type->count_expr || type->count_var)
		{
			if ((type->kind > 0)
				&& !ofc_colstr_atomic_writef(cs, ", "))
				return false;

			if (!ofc_colstr_atomic_writef(cs, "LEN="))
				return false;

			if (!(type->count_var
				? ofc_colstr_atomic_writef(cs, "*")
				: ofc_parse_expr_print(cs, type->count_expr)))
				return false;
		}

		if (!ofc_colstr_atomic_writef(cs, ")"))
			return false;
	}

	return (!colons || ofc_colstr_atomic_writef(cs, " ::"));
}

bool ofc_parse_type_print_f77(
	ofc_colstr_t* cs, const ofc_parse_type_t* type)
{
	if (type->type >= OFC_PARSE_TYPE_COUNT)
		return false;

	if (!ofc_colstr_atomic_writef(cs, "%s",
		ofc_parse_type__name[type->type]))
		return false;

	if (type->type == OFC_PARSE_TYPE_TYPE)
	{
		if (!ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_atomic_writef(cs, "(")
			|| !ofc_sparse_ref_print(cs, type->type_name)
			|| !ofc_colstr_atomic_writef(cs, ")"))
			return false;
	}

	if ((type->type == OFC_PARSE_TYPE_CHARACTER)
		|| (type->type == OFC_PARSE_TYPE_BYTE))
	{
		if (type->kind > 1)
			return false;

		if (type->count_var)
		{
			if (!ofc_colstr_atomic_writef(cs, "*")
				|| !ofc_colstr_atomic_writef(cs, "(")
				|| !ofc_colstr_atomic_writef(cs, "*")
				|| !ofc_colstr_atomic_writef(cs, ")"))
				return false;
		}
		else if (type->count_expr)
		{
			if (!ofc_colstr_atomic_writef(cs, "*"))
				return false;

			bool brackets = (type->count_expr->type
				!= OFC_PARSE_EXPR_CONSTANT);
			if (brackets && !ofc_colstr_atomic_writef(cs, "("))
				return false;

			if (!ofc_parse_expr_print(cs, type->count_expr))
				return false;

			if (brackets && !ofc_colstr_atomic_writef(cs, ")"))
				return false;
		}
	}
	else
	{
		if (type->count_expr || type->count_var)
			return false;

		if (type->kind > 0)
		{
			if (!ofc_colstr_atomic_writef(cs, "*")
				|| !ofc_colstr_atomic_writef(cs, "%u", type->kind))
				return false;
		}
	}

	return true;
}
