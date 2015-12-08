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

#include <ofc/sema.h>



const ofc_sema_spec_t OFC_SEMA_SPEC_DEFAULT =
{
	.name          = OFC_STR_REF_EMPTY,
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
	ofc_str_ref_t name)
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
			ofc_sema_scope_error(scope, ptype->src,
				"Type LEN specified as both fixed and variable");
			return NULL;
		}

		ofc_sema_expr_t* expr
			= ofc_sema_expr(scope, ptype->count_expr);

		bool resolved = ofc_sema_expr_resolve_uint(expr, &s.len);
		ofc_sema_expr_delete(expr);

		if (!resolved)
		{
			ofc_sema_scope_error(scope, ptype->count_expr->src,
				"Type LEN expression couldn't be resolved");
			return NULL;
		}

		if (s.len == 0)
		{
			ofc_sema_scope_error(scope, ptype->count_expr->src,
				"Type LEN must be greater than zero");
			return NULL;
		}
	}

	s.kind = ptype->kind;
	if (ptype->params)
	{
		unsigned i;
		for (i = 0; i < ptype->params->count; i++)
		{
			/* TODO - Handle unnamed kind, len */
			if (ofc_str_ref_equal_strz_ci(
				ptype->params->call_arg[i]->name, "LEN"))
			{
				if (ptype->params->call_arg[i]->type
					== OFC_PARSE_CALL_ARG_ASTERISK)
				{
					if (s.len > 0)
					{
						ofc_sema_scope_error(scope, ptype->src,
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
						ofc_sema_scope_error(scope, ptype->src,
							"Type LEN expression couldn't be resolved.");
						return NULL;
					}

					if (len == 0)
					{
						ofc_sema_scope_error(scope, ptype->src,
							"Type LEN paramater must be greater than zero.");
						return NULL;
					}

					if (s.len_var)
					{
						ofc_sema_scope_error(scope, ptype->src,
							"Type LEN specified as both fixed and variable");
						return NULL;
					}
					else if (s.len > 0)
					{
						if(s.len != len)
						{
							ofc_sema_scope_error(scope, ptype->src,
								"Conflicting type LEN specifications");
							return NULL;
						}

						ofc_sema_scope_warning(scope, ptype->src,
							"Multiple type LEN specifications");
					}

					s.len = len;
				}
			}
			else if (ofc_str_ref_equal_strz_ci(
				ptype->params->call_arg[i]->name, "KIND"))
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
					ofc_sema_scope_error(scope, ptype->src,
						"Type KIND expression couldn't be resolved.");
					return NULL;
				}

				if (kind == 0)
				{
					ofc_sema_scope_error(scope, ptype->src,
						"Type KIND paramater must be greater than zero.");
					return NULL;
				}

				if (s.kind > 0)
				{
					if(s.kind != kind)
					{
						ofc_sema_scope_error(scope, ptype->src,
							"Conflicting type KIND specifications");
						return NULL;
					}

					ofc_sema_scope_warning(scope, ptype->src,
						"Multiple type KIND specifications");
				}

				s.kind = kind;
			}
			else
			{
				ofc_sema_scope_error(scope, ptype->src,
					"Unknown parameter in type.");
				return NULL;
			}
		}
	}

	if ((s.len != 0) || s.len_var)
	{
		if (s.type != OFC_SEMA_TYPE_CHARACTER)
		{
			ofc_sema_scope_error(scope, ptype->src,
					"LEN parameter only supported for CHARACTER type.");
			return NULL;
		}
	}

	if (s.kind == 0)
	{
		/* TODO - Calculate this properly from lang_opts. */
		switch (s.type)
		{
			case OFC_SEMA_TYPE_CHARACTER:
			case OFC_SEMA_TYPE_BYTE:
				s.kind = 1;
				break;

			case OFC_SEMA_TYPE_LOGICAL:
			case OFC_SEMA_TYPE_INTEGER:
			case OFC_SEMA_TYPE_REAL:
			case OFC_SEMA_TYPE_COMPLEX:
				s.kind = 4;
				break;

			default:
				break;
		}
	}

	switch (ptype->type)
	{
		case OFC_PARSE_TYPE_DOUBLE_PRECISION:
		case OFC_PARSE_TYPE_DOUBLE_COMPLEX:
			s.kind *= 2;
			break;
		default:
			break;
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
	return (spec ? &spec->name : NULL);
}

ofc_hashmap_t* ofc_sema_spec_map_create(
	bool case_sensitive)
{
	return ofc_hashmap_create(
		(void*)(case_sensitive
			? ofc_str_ref_ptr_hash
			: ofc_str_ref_ptr_hash_ci),
		(void*)(case_sensitive
			? ofc_str_ref_ptr_equal
			: ofc_str_ref_ptr_equal_ci),
		(void*)ofc_sema_spec__name,
		(void*)ofc_sema_spec_delete);
}
