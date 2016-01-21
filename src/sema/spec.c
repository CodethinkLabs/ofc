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

#include "ofc/sema.h"



const ofc_sema_spec_t OFC_SEMA_SPEC_DEFAULT =
{
	.name          = OFC_SPARSE_REF_EMPTY,
	.type_implicit = true,
	.kind          = 0,
	.len           = 0,
	.len_var       = false,
	.array         = NULL,
	.is_static     = false,
	.is_automatic  = false,
	.is_volatile   = false,
	.is_intrinsic  = false,
	.is_external   = false,
	.common        = NULL,
	.common_offset = 0,
};



ofc_sema_spec_t* ofc_sema_spec_create(
	ofc_sparse_ref_t name)
{
	ofc_sema_spec_t* spec
		= (ofc_sema_spec_t*)malloc(
			sizeof(ofc_sema_spec_t));
	if (!spec) return NULL;

	*spec = OFC_SEMA_SPEC_DEFAULT;
	spec->name = name;
	return spec;
}

ofc_sema_spec_t* ofc_sema_spec(
	ofc_sema_scope_t*       scope,
	const ofc_parse_type_t* ptype)
{
	if (!ptype)
		return NULL;

	ofc_sema_spec_t s
		= OFC_SEMA_SPEC_DEFAULT;

	s.type_implicit = false;
	switch (ptype->type)
	{
		case OFC_PARSE_TYPE_NONE:
			s.type_implicit = true;
			break;

		case OFC_PARSE_TYPE_LOGICAL:
			s.type = OFC_SEMA_TYPE_LOGICAL;
			break;

		case OFC_PARSE_TYPE_CHARACTER:
			s.type = OFC_SEMA_TYPE_CHARACTER;
			break;

		case OFC_PARSE_TYPE_INTEGER:
			s.type = OFC_SEMA_TYPE_INTEGER;
			break;

		case OFC_PARSE_TYPE_REAL:
		case OFC_PARSE_TYPE_DOUBLE_PRECISION:
			s.type = OFC_SEMA_TYPE_REAL;
			break;

		case OFC_PARSE_TYPE_COMPLEX:
		case OFC_PARSE_TYPE_DOUBLE_COMPLEX:
			s.type = OFC_SEMA_TYPE_COMPLEX;
			break;

		case OFC_PARSE_TYPE_BYTE:
			s.type = OFC_SEMA_TYPE_BYTE;
			break;

		default:
			/* TODO - Handle derived types? */
			return NULL;
	}

	s.len     = 0;
	s.len_var = ptype->count_var;
	if (ptype->count_expr)
	{
		if (s.len_var)
		{
			ofc_sparse_ref_error(ptype->src,
				"Type LEN specified as both fixed and variable");
			return NULL;
		}

		ofc_sema_expr_t* expr
			= ofc_sema_expr(scope, ptype->count_expr);

		bool resolved = ofc_sema_expr_resolve_uint(expr, &s.len);
		ofc_sema_expr_delete(expr);

		if (!resolved)
		{
			ofc_sparse_ref_error(ptype->count_expr->src,
				"Type LEN expression couldn't be resolved");
			return NULL;
		}

		if (s.len == 0)
		{
			ofc_sparse_ref_error(ptype->count_expr->src,
				"Type LEN must be greater than zero");
			return NULL;
		}
	}
	else if (!ptype->count_var
		&& (s.type == OFC_SEMA_TYPE_CHARACTER))
	{
		s.len = 1;
	}

	switch (ptype->type)
	{
		case OFC_PARSE_TYPE_DOUBLE_PRECISION:
		case OFC_PARSE_TYPE_DOUBLE_COMPLEX:
			if (ptype->size != 0)
			{
				ofc_sparse_ref_error(ptype->count_expr->src,
					"Can't specify size of DOUBLE type");
				return NULL;
			}
			s.kind = 2;
			break;
		default:
			s.kind = (ptype->size > 0 ? (3 * ptype->size) : 1);
			break;
	}

	if (ptype->params)
	{
		unsigned i;
		for (i = 0; i < ptype->params->count; i++)
		{
			/* TODO - Handle unnamed kind, len */
			if (ofc_str_ref_equal_strz_ci(
				ptype->params->call_arg[i]->name.string, "LEN"))
			{
				if (ptype->params->call_arg[i]->type
					== OFC_PARSE_CALL_ARG_ASTERISK)
				{
					if (s.len > 0)
					{
						ofc_sparse_ref_error(ptype->src,
							"Type LEN specified as both fixed and variable");
						return NULL;
					}

					s.len_var = true;
				}
				else
				{
					if (ptype->params->call_arg[i]->type
						!= OFC_PARSE_CALL_ARG_EXPR)
						return NULL;

					ofc_sema_expr_t* expr = ofc_sema_expr(
						scope, ptype->params->call_arg[i]->expr);
					if (!expr) return NULL;

					unsigned len;
					bool resolved = ofc_sema_expr_resolve_uint(expr, &len);
					ofc_sema_expr_delete(expr);
					if (!resolved)
					{
						ofc_sparse_ref_error(ptype->src,
							"Type LEN expression couldn't be resolved");
						return NULL;
					}

					if (len == 0)
					{
						ofc_sparse_ref_error(ptype->src,
							"Type LEN paramater must be greater than zero");
						return NULL;
					}

					if (s.len_var)
					{
						ofc_sparse_ref_error(ptype->src,
							"Type LEN specified as both fixed and variable");
						return NULL;
					}
					else if (s.len > 0)
					{
						if(s.len != len)
						{
							ofc_sparse_ref_error(ptype->src,
								"Conflicting type LEN specifications");
							return NULL;
						}

						ofc_sparse_ref_warning(ptype->src,
							"Multiple type LEN specifications");
					}

					s.len = len;
				}
			}
			else if (ofc_str_ref_equal_strz_ci(
				ptype->params->call_arg[i]->name.string, "KIND"))
			{
				if (ptype->params->call_arg[i]->type
					!= OFC_PARSE_CALL_ARG_EXPR)
					return NULL;

				ofc_sema_expr_t* expr = ofc_sema_expr(
					scope, ptype->params->call_arg[i]->expr);
				if (!expr) return NULL;

				unsigned kind;
				bool resolved = ofc_sema_expr_resolve_uint(expr, &kind);
				ofc_sema_expr_delete(expr);
				if (!resolved)
				{
					ofc_sparse_ref_error(ptype->src,
						"Type KIND expression couldn't be resolved");
					return NULL;
				}

				if (kind == 0)
				{
					ofc_sparse_ref_error(ptype->src,
						"Type KIND paramater must be greater than zero");
					return NULL;
				}

				if (s.kind != 1)
				{
					if(s.kind != kind)
					{
						ofc_sparse_ref_error(ptype->src,
							"Conflicting type KIND specifications");
						return NULL;
					}

					ofc_sparse_ref_warning(ptype->src,
						"Multiple type KIND specifications");
				}

				s.kind = kind;
			}
			else
			{
				ofc_sparse_ref_error(ptype->src,
					"Unknown parameter in type");
				return NULL;
			}
		}
	}

	if ((s.len != 0) || s.len_var)
	{
		if (s.type != OFC_SEMA_TYPE_CHARACTER)
		{
			ofc_sparse_ref_error(ptype->src,
					"LEN parameter only supported for CHARACTER type");
			return NULL;
		}
	}

	if (s.type == OFC_SEMA_TYPE_BYTE)
	{
		if ((s.kind != 1)
			&& (s.kind != 3))
		{
			ofc_sparse_ref_error(ptype->src,
				"BYTE kind must represent a size of 1");
			return NULL;
		}
	}

	s.is_static    = ptype->attr.is_static;
	s.is_automatic = ptype->attr.is_automatic;
	s.is_volatile  = ptype->attr.is_volatile;

	ofc_sema_spec_t* spec
		= (ofc_sema_spec_t*)malloc(
			sizeof(ofc_sema_spec_t));
	if (!spec) return NULL;

	*spec = s;
	return spec;
}

ofc_sema_spec_t* ofc_sema_spec_copy(
	const ofc_sema_spec_t* spec)
{
	if (!spec)
		return NULL;

	ofc_sema_spec_t* copy
		= ofc_sema_spec_create(spec->name);
	if (!copy) return NULL;

	*copy = *spec;

	if (spec->array)
	{
		copy->array = ofc_sema_array_copy(spec->array);
		if (!copy->array)
		{
			free(copy);
			return NULL;
		}
	}

	return copy;
}


bool ofc_sema_spec_mark_used(
	ofc_sema_scope_t* scope,
	ofc_sema_spec_t* spec)
{
	if (!spec)
		return false;

	if (!spec->used)
	{
		ofc_sema_decl_t* decl
			= ofc_sema_scope_decl_find_modify(
				scope, spec->name.string, true);
		if (decl) decl->has_spec = true;

		spec->used = true;
	}

	return true;
}


bool ofc_sema_spec_print(
	ofc_colstr_t* cs,
	unsigned indent,
	const ofc_sema_scope_t* scope,
	const ofc_sema_spec_t* spec)
{
	if (!spec)
		return false;

	/* Ignore specifiers that are never used. */
	if (!spec->used)
		return true;

	unsigned kind = spec->kind;
	unsigned len  = spec->len;

	ofc_sema_type_e stype = spec->type;
	if (spec->type_implicit)
	{
		ofc_sema_spec_t* final
			= ofc_sema_scope_spec_find_final(
				scope, spec->name);
		if (!final) return false;

		stype = final->type;
		kind  = final->kind;
		len   = final->len;
		ofc_sema_spec_delete(final);
	}

	if (kind == 0) kind = 1;

	const ofc_sema_type_t* type;
	switch (stype)
	{
		case OFC_SEMA_TYPE_CHARACTER:
			type = ofc_sema_type_create_character(kind, len);
			break;
		default:
			type = ofc_sema_type_create_primitive(stype, kind);
			break;
	}

	if (!ofc_colstr_newline(cs, indent, NULL)
		|| !ofc_sema_type_print(cs, type))
		return false;

	if (spec->is_external)
	{
		if (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_atomic_writef(cs, "EXTERNAL"))
			return false;
	}

	if (spec->is_intrinsic)
	{
		if (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_atomic_writef(cs, "INTRINSIC"))
			return false;
	}

	if (spec->is_volatile)
	{
		if (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_atomic_writef(cs, "VOLATILE"))
			return false;
	}

	if (spec->is_static)
	{
		if (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_atomic_writef(cs, "SAVE"))
			return false;
	}

	if (spec->is_automatic)
	{
		if (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_atomic_writef(cs, "AUTOMATIC"))
			return false;
	}

	if (spec->array)
	{
		if (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_atomic_writef(cs, "DIMENSION")
			|| !ofc_sema_array_print_brackets(cs, spec->array))
			return false;
	}

	return (ofc_colstr_atomic_writef(cs, " ")
		&& ofc_colstr_atomic_writef(cs, "::")
		&& ofc_colstr_atomic_writef(cs, " ")
		&& ofc_sparse_ref_print(cs, spec->name));
}

void ofc_sema_spec_delete(
	ofc_sema_spec_t* spec)
{
	if (!spec)
		return;

	ofc_sema_array_delete(spec->array);
	free(spec);
}


static const ofc_str_ref_t* ofc_sema_spec__name(
	const ofc_sema_spec_t* spec)
{
	return (spec ? &spec->name.string : NULL);
}

static ofc_sema_spec_list_t* ofc_sema_spec_list_create()
{
	ofc_sema_spec_list_t* list
		= (ofc_sema_spec_list_t*)malloc(
			sizeof(ofc_sema_spec_list_t));
	if (!list) return NULL;

	list->count  = 0;
	list->spec = NULL;

	return list;
}

bool ofc_sema_spec_list_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_sema_scope_t* scope,
	const ofc_sema_spec_list_t* list)
{
	if (!cs || !list)
		return false;

	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		if (!ofc_sema_spec_print(
			cs, indent, scope, list->spec[i]))
			return false;
	}

	return true;
}

static void ofc_sema_spec_list_delete(
	ofc_sema_spec_list_t* list)
{
	if (!list)
		return;

	if (list->spec)
	{
		unsigned i;
		for (i = 0; i < list->count; i++)
			ofc_sema_spec_delete(list->spec[i]);
		free(list->spec);
	}

	free(list);
}

ofc_sema_spec_map_t* ofc_sema_spec_map_create(
	bool case_sensitive)
{
	ofc_sema_spec_map_t* map
		= (ofc_sema_spec_map_t*)malloc(
			sizeof(ofc_sema_spec_map_t));
	if (!map) return NULL;

	map->map = ofc_hashmap_create(
		(void*)(case_sensitive
			? ofc_str_ref_ptr_hash
			: ofc_str_ref_ptr_hash_ci),
		(void*)(case_sensitive
			? ofc_str_ref_ptr_equal
			: ofc_str_ref_ptr_equal_ci),
		(void*)ofc_sema_spec__name,
		NULL);

	map->list = ofc_sema_spec_list_create();

	if (!map->map || !map->list)
	{
		ofc_sema_spec_map_delete(map);
		return NULL;
	}

	return map;
}

bool ofc_sema_spec_map_add(
	ofc_sema_spec_map_t* map,
	ofc_sema_spec_t* spec)
{
	if (!map || !map->list || !spec)
		return false;

	ofc_sema_spec_t** nspec
		= (ofc_sema_spec_t**)realloc(map->list->spec,
			(sizeof(ofc_sema_spec_t*) * (map->list->count + 1)));
	if (!nspec) return false;
	map->list->spec = nspec;

	if (!ofc_hashmap_add(map->map, spec))
		return false;

	map->list->spec[map->list->count++] = spec;

	return true;
}

void ofc_sema_spec_map_delete(
	ofc_sema_spec_map_t* map)
{
	if (!map)
		return;

	ofc_sema_spec_list_delete(map->list);
	ofc_hashmap_delete(map->map);

	free(map);
}
