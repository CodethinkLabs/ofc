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


static void ofc_sema_decl_init__delete(
	ofc_sema_decl_init_t init)
{
	if (init.is_substring)
	{
		free(init.substring.string);
		free(init.substring.mask);
	}
	else
	{
		ofc_sema_expr_delete(init.expr);
	}
}

ofc_sema_decl_t* ofc_sema_decl_create(
	const ofc_sema_type_t* type,
	ofc_sparse_ref_t name)
{
	ofc_sema_decl_t* decl
		= (ofc_sema_decl_t*)malloc(
			sizeof(ofc_sema_decl_t));
	if (!decl) return NULL;

	decl->type = type;
	decl->name = name;

	decl->func      = NULL;
	decl->array     = NULL;
	decl->structure = NULL;

	if (ofc_sema_decl_is_composite(decl))
	{
		decl->init_array = NULL;
	}
	else
	{
		decl->init.is_substring = false;
		decl->init.expr = NULL;
	}

	decl->is_parameter = false;

	decl->is_static    = false;
	decl->is_volatile  = false;
	decl->is_automatic = false;

	decl->is_target    = false;

	decl->is_equiv     = false;
	decl->is_common    = false;
	decl->is_argument  = false;
	decl->is_return    = false;
	decl->has_spec     = false;

	decl->used = false;

	decl->refcnt = 0;
	return decl;
}

static ofc_sema_decl_t* ofc_sema_decl__spec(
	ofc_sema_scope_t*       scope,
	ofc_sema_structure_t*   member_of,
	ofc_sparse_ref_t        name,
	ofc_sema_spec_t*        spec,
	const ofc_sema_array_t* array,
	bool                    is_procedure,
	bool                    is_function)
{
	if (!spec)
		return NULL;

	if (spec->common && member_of)
	{
		/* This should never happen. */
		return NULL;
	}

	if (spec->common && (spec->is_static
		|| spec->is_automatic || spec->is_volatile
		|| spec->is_intrinsic || spec->is_external))
	{
		/* TODO - Work out if VOLATILE should be allowed. */
		ofc_sparse_ref_error(name,
			"COMMON block entries can't have declaration attributes.");
		return NULL;
	}

	if (member_of)
	{
		if (is_procedure)
		{
			ofc_sparse_ref_error(name,
				"Structure members cannot be a procedure");
			return NULL;
		}

		if (spec->is_static)
		{
			ofc_sparse_ref_error(name,
				"Structure members cannot be STATIC");
			return NULL;
		}

		if (spec->is_volatile)
		{
			ofc_sparse_ref_error(name,
				"Structure members cannot be VOLATILE");
			return NULL;
		}

		if (spec->is_intrinsic)
		{
			ofc_sparse_ref_error(name,
				"Structure members cannot be INTRINSIC");
			return NULL;
		}

		if (spec->is_external)
		{
			ofc_sparse_ref_error(name,
				"Structure members cannot be EXTERNAL");
			return NULL;
		}
	}
	else if (is_procedure)
	{
		if (spec->is_static)
		{
			ofc_sparse_ref_error(name,
				"Procedures cannot be STATIC");
			return NULL;
		}

		if (spec->is_volatile)
		{
			ofc_sparse_ref_error(name,
				"Procedures cannot be VOLATILE");
			return NULL;
		}

		if (spec->is_intrinsic
			&& spec->is_external)
		{
			ofc_sparse_ref_error(name,
				"A procedure cannot be INTRINSIC and EXTERNAL");
			return NULL;
		}

		if (!is_function)
		{
			if (!spec->type_implicit
				|| (spec->kind > 0)
				|| (spec->len > 0)
				|| spec->len_var)
			{
				ofc_sparse_ref_error(name,
					"A SUBROUTINE cannot have a specified type");
				return NULL;
			}
		}
	}
	else
	{
		if (spec->is_intrinsic)
		{
			ofc_sparse_ref_error(name,
				"Only procedures may be INTRINSIC");
			return NULL;
		}

		if (spec->is_external)
		{
			ofc_sparse_ref_error(name,
				"Only procedures may be EXTERNAL");
			return NULL;
		}

		if (spec->is_static
			&& spec->is_automatic)
		{
			ofc_sparse_ref_error(name,
				"Declaration cannot be AUTOMATIC and STATIC");
			return NULL;
		}
	}

	if (spec->array)
	{
		if (array)
		{
			if (!ofc_sema_array_compare(
				array, spec->array))
			{
				ofc_sparse_ref_error(name,
					"Conflicting array dimensions in declaration");
				return NULL;
			}

			ofc_sparse_ref_warning(name,
				"Repetition of array dimensions in declaration");
		}

		array = spec->array;
	}

	ofc_sema_structure_t* structure = NULL;
	if ((spec->type == OFC_SEMA_TYPE_TYPE)
		|| (spec->type == OFC_SEMA_TYPE_RECORD))
	{
		if (!spec->structure)
			return NULL;
		structure = spec->structure;
	}

	const ofc_sema_type_t* type
		= ofc_sema_type_spec(spec);
	if (!type) return NULL;

	if (is_procedure)
	{
		type = (is_function
			? ofc_sema_type_create_function(type)
			: ofc_sema_type_subroutine());
		if (!type) return NULL;
	}

	ofc_sema_decl_t* decl
		= ofc_sema_decl_create(type, name);
	if (!decl) return NULL;

	if (array)
	{
		decl->array = ofc_sema_array_copy(array);
		if (!decl->array)
		{
			ofc_sema_decl_delete(decl);
			return NULL;
		}

		decl->init_array = NULL;
	}

	if (structure)
	{
		if (!ofc_sema_structure_reference(structure))
		{
			ofc_sema_decl_delete(decl);
			return NULL;
		}

		decl->structure = structure;
		decl->init_array = NULL;
	}

	decl->is_static    = spec->is_static;
	decl->is_automatic = spec->is_automatic;
	decl->is_volatile  = spec->is_volatile;
	decl->is_intrinsic = spec->is_intrinsic;
	decl->is_external  = spec->is_external;

	decl->is_equiv    = spec->is_equiv;
	decl->is_argument = spec->is_argument;
	decl->is_return   = spec->is_return;

	decl->has_spec = spec->used;

	if (member_of)
	{
		if (!ofc_sema_structure_member_add_decl(
				member_of, decl))
		{
			ofc_sema_decl_delete(decl);
			return NULL;
		}
	}
	else
	{

		if (!ofc_sema_scope_decl_add(
			scope, decl))
		{
			ofc_sema_decl_delete(decl);
			return NULL;
		}
	}

	if (spec->common)
	{
		if (!ofc_sema_common_define(
			spec->common, spec->common_offset, decl))
		{
			/* This should never happen. */
			abort();
		}

		decl->is_common = true;
	}

	return decl;
}

ofc_sema_decl_t* ofc_sema_decl_spec(
	ofc_sema_scope_t*       scope,
	ofc_sparse_ref_t        name,
	ofc_sema_spec_t*        spec,
	const ofc_sema_array_t* array)
{
	return ofc_sema_decl__spec(
		scope, NULL, name, spec, array, false, false);
}

ofc_sema_decl_t* ofc_sema_decl_implicit(
	ofc_sema_scope_t*       scope,
	ofc_sparse_ref_t        name,
	const ofc_sema_array_t* array)
{
	ofc_sema_spec_t* spec
		= ofc_sema_scope_spec_find_final(scope, name);
	if (!spec) return NULL;

	ofc_sema_decl_t* decl;
	if (spec->is_external
		|| spec->is_intrinsic)
	{
		if (spec->type_implicit)
		{
			decl = ofc_sema_decl_subroutine(
				scope, name);
		}
		else
		{
			decl = ofc_sema_decl_function(
				scope, name, spec);
		}
	}
	else
	{
		decl = ofc_sema_decl_spec(
			scope, name, spec, array);
	}

	ofc_sema_spec_delete(spec);
	return decl;
}

ofc_sema_decl_t* ofc_sema_decl_implicit_lhs(
	ofc_sema_scope_t*       scope,
	const ofc_parse_lhs_t*  lhs)
{
	if (!scope || !lhs)
		return NULL;

	ofc_sema_array_t* array = NULL;
	ofc_sparse_ref_t base_name;
	if (!ofc_parse_lhs_base_name(
		*lhs, &base_name))
		return NULL;

	switch (lhs->type)
	{
		case OFC_PARSE_LHS_VARIABLE:
			break;
		case OFC_PARSE_LHS_ARRAY:
			array = ofc_sema_array(
				scope, lhs->array.index);
			if (!array) return NULL;
			break;
		default:
			break;
	}

	ofc_sema_decl_t* decl
		= ofc_sema_decl_implicit(
			scope, base_name, array);
	ofc_sema_array_delete(array);
	return decl;
}


ofc_sema_decl_t* ofc_sema_decl_function(
	ofc_sema_scope_t*  scope,
	ofc_sparse_ref_t   name,
	ofc_sema_spec_t*   spec)
{
	return ofc_sema_decl__spec(
		scope, NULL, name, spec, NULL, true, true);
}

ofc_sema_decl_t* ofc_sema_decl_subroutine(
	ofc_sema_scope_t* scope,
	ofc_sparse_ref_t  name)
{
	ofc_sema_spec_t* spec
		= ofc_sema_scope_spec_find_final(
			scope, name);
	ofc_sema_decl_t* decl = ofc_sema_decl__spec(
		scope, NULL, name, spec, NULL, true, false);
	ofc_sema_spec_delete(spec);
	return decl;
}


static bool ofc_sema_decl__decl(
	ofc_sema_scope_t*       scope,
	ofc_sema_spec_t         spec,
	ofc_sema_structure_t*   structure,
	const ofc_parse_decl_t* pdecl)
{
	if (!pdecl || !pdecl->lhs)
		return false;

	if (!ofc_sparse_ref_empty(pdecl->record))
	{
		/* We intentionally override the existing structure. */
		spec.structure = ofc_sema_scope_structure_find(
			scope, pdecl->record.string);
		if (!spec.structure)
		{
			spec.structure = ofc_sema_scope_derived_type_find(
				scope, pdecl->record.string);
			if (!spec.structure)
			{
				ofc_sparse_ref_error(pdecl->record,
					"Referencing undefined STRUCTURE in RECORD statement");
				return false;
			}
			ofc_sparse_ref_warning(pdecl->record,
				"Referencing Fortran 90 TYPE in VAX RECORD statement");
		}
	}

	bool is_decl = ((structure != NULL)
		|| pdecl->init_clist || pdecl->init_expr);

	const ofc_parse_lhs_t* lhs = pdecl->lhs;

	ofc_sema_array_t* array = NULL;
	if (lhs->type == OFC_PARSE_LHS_ARRAY)
	{
		array = ofc_sema_array(
			scope, lhs->array.index);
		if (!array) return false;

		lhs = lhs->parent;
		if (!lhs)
		{
			ofc_sema_array_delete(array);
			return false;
		}
	}

	if (lhs->type == OFC_PARSE_LHS_STAR_LEN)
	{
		if (spec.type_implicit)
		{
			ofc_sparse_ref_error(lhs->src,
				"Can't specify a star LEN/KIND for an implied type");
			ofc_sema_array_delete(array);
			return false;
		}

		if (lhs->star_len.var)
		{
			if (spec.type != OFC_SEMA_TYPE_CHARACTER)
			{
				ofc_sparse_ref_error(lhs->src,
					"Only CHARACTER types may have an assumed length");
				ofc_sema_array_delete(array);
				return false;
			}

			if (spec.len > 0)
			{
				ofc_sparse_ref_warning(lhs->src,
					"Overriding specified star length in %s list",
					(is_decl ? "decl" : "specifier"));
			}

			spec.len     = 0;
			spec.len_var = true;
		}
		else
		{
			ofc_sema_expr_t* expr
				= ofc_sema_expr(scope, lhs->star_len.len);
			if (!expr)
			{
				ofc_sema_array_delete(array);
				return false;
			}

			unsigned star_len;
			bool resolved = ofc_sema_expr_resolve_uint(expr, &star_len);
			ofc_sema_expr_delete(expr);

			if (!resolved)
			{
				ofc_sparse_ref_error(lhs->src,
					"Star length must be a positive whole integer");
				ofc_sema_array_delete(array);
				return false;
			}

			if (spec.type == OFC_SEMA_TYPE_CHARACTER)
			{
				if (spec.len_var
					|| ((spec.len > 0) && (spec.len != star_len)))
				{
					ofc_sparse_ref_warning(lhs->src,
						"Overriding specified LEN in %s list",
						(is_decl ? "decl" : "specifier"));
				}

				spec.len     = star_len;
				spec.len_var = false;
			}
			else
			{
				if (star_len == 0)
				{
					ofc_sparse_ref_error(lhs->src,
						"Star size must be non-zero");
					ofc_sema_array_delete(array);
					return false;
				}

				if ((spec.kind != 0)
					&& ((star_len * 3) != spec.kind))
				{
					ofc_sparse_ref_warning(lhs->src,
						"Overriding specified KIND in %s list",
						(is_decl ? "decl" : "specifier"));
				}

				spec.kind = (star_len * 3);
			}
		}

		lhs = lhs->parent;
		if (!lhs)
		{
			ofc_sema_array_delete(array);
			return false;
		}
	}

	if (lhs->type == OFC_PARSE_LHS_ARRAY)
	{
		if (array)
		{
			ofc_sparse_ref_error(lhs->src,
				"Multiple definition of array dimensions");
			ofc_sema_array_delete(array);
			return false;
		}

		array = ofc_sema_array(
			scope, lhs->array.index);
		if (!array) return false;

		lhs = lhs->parent;
		if (!lhs)
		{
			ofc_sema_array_delete(array);
			return false;
		}
	}

	if (array)
	{
		if (spec.array)
		{
			/* This shouldn't ever happen since arrays
			   can't be specified on the LHS of a declaration. */
			ofc_sema_array_delete(array);
			return false;
		}
	}
	else if (spec.array)
	{
		array = ofc_sema_array_copy(spec.array);
		if (!array) return false;
	}
	spec.array = array;

	if (lhs->type != OFC_PARSE_LHS_VARIABLE)
	{
		ofc_sema_array_delete(array);
		return false;
	}

	if (!ofc_parse_lhs_base_name(
		*lhs, &spec.name))
	{
		ofc_sema_array_delete(array);
		return false;
	}

	ofc_sema_decl_t* decl = NULL;
	if (!structure)
	{
		decl = ofc_sema_scope_decl_find_modify(
			scope, spec.name.string, true);
	}

	ofc_sema_spec_t* nspec = &spec;
	if (!structure)
	{
		if (decl != NULL)
		{
			/* TODO - Handle specifications which are compatible. */

			ofc_sparse_ref_error(lhs->src,
				"Specification of declared variable");
			ofc_sema_array_delete(array);
			return false;
		}

		nspec = ofc_sema_scope_spec_modify(
			scope, spec.name);
		if (!nspec)
		{
			ofc_sema_array_delete(array);
			return false;
		}

		/* Overlay the spec on the existing one. */
		if (!spec.type_implicit)
		{
			if (!nspec->type_implicit
				&& ((nspec->type != spec.type)
					|| (nspec->structure != spec.structure)))
			{
				ofc_sema_array_delete(array);
				return false;
			}

			nspec->type_implicit = spec.type_implicit;
			nspec->type          = spec.type;

			if (spec.kind != 0)
				nspec->kind = spec.kind;

			if (spec.structure)
			{
				if (nspec->structure
					? (nspec->structure != spec.structure)
					: !ofc_sema_structure_reference(spec.structure))
				{
					ofc_sema_array_delete(array);
					return false;
				}
				nspec->structure = spec.structure;
			}
		}

		if (spec.array)
		{
			if (nspec->array)
			{
				bool conflict = !ofc_sema_array_compare(
					nspec->array, spec.array);
				ofc_sema_array_delete(array);
				if (conflict) return false;
			}
			else
			{
				nspec->array = spec.array;
			}
		}
		array = NULL;

		if ((spec.len != 0)
			|| spec.len_var)
		{
			nspec->len     = spec.len;
			nspec->len_var = spec.len_var;
		}

		nspec->is_static    |= spec.is_static;
		nspec->is_automatic |= spec.is_automatic;
		nspec->is_volatile  |= spec.is_volatile;
		nspec->is_intrinsic |= spec.is_intrinsic;
		nspec->is_external  |= spec.is_external;
	}

	if (ofc_sema_spec_is_dynamic_array(nspec)
		&& !ofc_sema_scope_is_procedure(scope))
	{
		ofc_sparse_ref_error(lhs->src,
			"Dynamically sized arrays are only valid in procedures");
		ofc_sema_array_delete(array);
		return false;
	}

	if (is_decl)
	{
		if (decl != NULL)
		{
			/* TODO - Handle redeclarations which match original. */

			ofc_sparse_ref_error(lhs->src,
				"Redeclaration of declared variable");
			ofc_sema_array_delete(array);
			return false;
		}

		ofc_sema_spec_t* fspec
			= ofc_sema_scope_spec_find_final(
				scope, spec.name);
		if (fspec)
		{
			bool overlaid
				= ofc_sema_spec_overlay(
					nspec, fspec);
			ofc_sema_spec_delete(fspec);
			if (!overlaid)
			{
				ofc_sema_array_delete(array);
				return false;
			}
		}

		decl = ofc_sema_decl__spec(
			scope, structure,
			spec.name, nspec,
			NULL, false, false);
		ofc_sema_array_delete(array);
		if (!decl) return false;

		if (pdecl->init_expr)
		{
			ofc_sema_expr_t* init_expr
				= ofc_sema_expr(scope, pdecl->init_expr);
			if (!init_expr)
				return false;

			bool initialized = ofc_sema_decl_init(
				decl, init_expr);
			ofc_sema_expr_delete(init_expr);

			if (!initialized)
				return false;
		}
		else if (pdecl->init_clist)
		{
			ofc_sema_expr_list_t* init_clist
				= ofc_sema_expr_list_clist(
					scope, pdecl->init_clist);
			if (!init_clist)
				return false;

			bool initialized = ofc_sema_decl_init_array(
				decl, NULL, init_clist->count,
				(const ofc_sema_expr_t**)init_clist->expr);
			ofc_sema_expr_list_delete(init_clist);

			if (!initialized)
				return false;
		}
	}

	return true;
}

bool ofc_sema_decl(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt || !scope
		|| !stmt->decl.type || !stmt->decl.decl
		|| (stmt->type != OFC_PARSE_STMT_DECL))
		return false;

	ofc_sema_spec_t* spec = ofc_sema_spec(
		scope, stmt->decl.type);
	if (!spec) return false;

	unsigned count = stmt->decl.decl->count;
	if (count == 0) return false;

	unsigned i;
	for (i = 0; i < count; i++)
	{
		if (!ofc_sema_decl__decl(
			scope, *spec, NULL, stmt->decl.decl->decl[i]))
		{
			ofc_sema_spec_delete(spec);
			return false;
		}
	}

	ofc_sema_spec_delete(spec);
	return true;
}

bool ofc_sema_decl_member(
	ofc_sema_scope_t* scope,
	ofc_sema_structure_t* structure,
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt || !scope
		|| !stmt->decl.type || !stmt->decl.decl
		|| (stmt->type != OFC_PARSE_STMT_DECL))
		return NULL;

	ofc_sema_spec_t* spec = ofc_sema_spec(
		scope, stmt->decl.type);
	if (!spec) return NULL;

	if (ofc_sema_spec_is_dynamic_array(spec))
	{
		ofc_sparse_ref_error(stmt->src,
			"Structure members cannot be dynamically sized arrays");
		ofc_sema_spec_delete(spec);
		return NULL;
	}

	unsigned count = stmt->decl.decl->count;
	if (count == 0) return NULL;

	unsigned i;
	for (i = 0; i < count; i++)
	{
		if (!ofc_sema_decl__decl(
			scope, *spec, structure,
			stmt->decl.decl->decl[i]))
		{
			ofc_sema_spec_delete(spec);
			return NULL;
		}
	}

	ofc_sema_spec_delete(spec);
	return true;
}



bool ofc_sema_decl_reference(
	ofc_sema_decl_t* decl)
{
	if (!decl)
		return false;

	if ((decl->refcnt + 1) == 0)
		return false;

	decl->refcnt++;
	return true;
}

void ofc_sema_decl_delete(
	ofc_sema_decl_t* decl)
{
	if (!decl)
		return;

	if (decl->refcnt > 0)
	{
		decl->refcnt--;
		return;
	}

	if (ofc_sema_decl_is_composite(decl))
	{
		if (decl->init_array)
		{
			unsigned count = 0;
			ofc_sema_decl_elem_count(decl, &count);

			unsigned i;
			for (i = 0; i < count; i++)
				ofc_sema_decl_init__delete(decl->init_array[i]);

			free(decl->init_array);
		}
	}
	else
	{
		ofc_sema_decl_init__delete(decl->init);
	}

	ofc_sema_array_delete(decl->array);
	ofc_sema_structure_delete(decl->structure);
	ofc_sema_scope_delete(decl->func);
	free(decl);
}


bool ofc_sema_decl_init(
	ofc_sema_decl_t* decl,
	const ofc_sema_expr_t* init)
{
	if (!decl || !init || !decl->type
		|| ofc_sema_decl_is_procedure(decl))
		return false;

	if (decl->used)
	{
		ofc_sparse_ref_error(init->src,
			"Can't initialize declaration after use");
		return false;
	}

	if (!ofc_sema_expr_is_constant(init))
	{
		ofc_sparse_ref_error(init->src,
			"Initializer element not constant");
		return false;
	}

	if (ofc_sema_decl_is_composite(decl))
	{
		/* TODO - Support F90 "(/ 0, 1 /)" array initializers. */
		ofc_sparse_ref_error(init->src,
			"Can't initialize non-scalar declaration with expression");
		return false;
	}

	if (decl->init.is_substring)
	{
		return ofc_sema_decl_init_substring(
			decl, init, NULL, NULL);
	}

	const ofc_sema_type_t* type = decl->type;
	if (decl->is_parameter
		&& (type->type == OFC_SEMA_TYPE_CHARACTER)
		&& type->len_var)
	{
		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(init);
		unsigned len = 1;
		if (etype && (etype->type
			== OFC_SEMA_TYPE_CHARACTER))
			len = etype->len;

		if (len == 0)
		{
			ofc_sparse_ref_error(init->src,
				"Can't initialize variable length CHARACTER PARAMETER"
				" with empty string");
			return false;
		}

		const ofc_sema_type_t* ntype
			= ofc_sema_type_create_character(
				type->kind, len, false);
		if (!ntype) return false;
		type = ntype;
	}

	ofc_sema_expr_t* expr
		= ofc_sema_expr_copy(init);
	if (!expr) return false;

	if (!ofc_sema_type_compatible(
		ofc_sema_expr_type(expr), type))
	{
		ofc_sema_expr_t* cast
			= ofc_sema_expr_cast(
				expr, type);
		if (!cast)
		{
			ofc_sparse_ref_error(init->src,
				"Incompatible types in initializer");
			ofc_sema_expr_delete(expr);
			return false;
		}
		expr = cast;
	}

	if (decl->init.is_substring)
	{
		ofc_sparse_ref_error(init->src,
			"Conflicting initializaters");
		ofc_sema_expr_delete(expr);
		return false;
	}
	else if (decl->init.expr)
	{
		bool redecl = ofc_sema_expr_compare(
			expr, decl->init.expr);
		ofc_sema_expr_delete(expr);

		if (redecl)
		{
			ofc_sparse_ref_warning(init->src,
				"Duplicate initialization");
		}
		else
		{
			ofc_sparse_ref_error(init->src,
				"Conflicting initializaters");
			return false;
		}
	}
	else
	{
		decl->type = type;
		decl->init.expr = expr;
	}

	return true;
}

bool ofc_sema_decl_init_offset(
	ofc_sema_decl_t* decl,
	unsigned offset,
	const ofc_sema_expr_t* init)
{
	if (!decl || !init || !decl->type
		|| ofc_sema_decl_is_procedure(decl))
		return false;

	if (decl->used)
	{
		ofc_sparse_ref_error(init->src,
			"Can't initialize declaration after use");
		return false;
	}

	if (!decl->type)
		return false;

	if (!ofc_sema_decl_is_composite(decl))
	{
		if (offset == 0)
			return ofc_sema_decl_init(
				decl, init);
		return false;
	}

	unsigned elem_count;
	if (!ofc_sema_decl_elem_count(
		decl, &elem_count))
	{
		ofc_sparse_ref_error(init->src,
			"Can't initialize element in array of unknown size");
		return false;
	}

	if (offset >= elem_count)
	{
		ofc_sparse_ref_warning(init->src,
			"Initializer destination out-of-bounds");
		return false;
	}

	if (!decl->init_array)
	{
		decl->init_array = (ofc_sema_decl_init_t*)malloc(
			sizeof(ofc_sema_decl_init_t) * elem_count);
		if (!decl->init_array) return false;

		unsigned i;
		for (i = 0; i < elem_count; i++)
		{
			decl->init_array[i].is_substring = false;
			decl->init_array[i].expr = NULL;
		}
	}

	if (!ofc_sema_expr_is_constant(init))
	{
		ofc_sparse_ref_error(init->src,
			"Array initializer element not constant.");
		return false;
	}

	ofc_sema_expr_t* expr
		= ofc_sema_expr_copy(init);
	if (!expr) return false;

	const ofc_sema_type_t* dtype = decl->type;
	if (decl->structure)
	{
		unsigned moffset = offset;
		if (decl->array)
		{
			unsigned mcount;
			if (!ofc_sema_structure_elem_count(
				decl->structure, &mcount)
				|| (mcount == 0))
				return false;
			moffset %= mcount;
		}

		ofc_sema_decl_t* mdecl
			= ofc_sema_structure_elem_get(
				decl->structure, moffset);
		if (!mdecl) return false;
		dtype = mdecl->type;
	}

	if (!ofc_sema_type_compatible(
		ofc_sema_expr_type(expr), dtype))
	{
		ofc_sema_expr_t* cast
			= ofc_sema_expr_cast(
				expr, dtype);
		if (!cast)
		{
			ofc_sparse_ref_error(init->src,
				"Incompatible types in initializer");
			ofc_sema_expr_delete(expr);
			return false;
		}
		expr = cast;
	}

	if (decl->init_array[offset].is_substring)
	{
		ofc_sparse_ref_error(init->src,
			"Conflicting initializer types for array element");
		ofc_sema_expr_delete(expr);
		return false;
	}
	else if (decl->init_array[offset].expr)
	{
		bool equal = ofc_sema_expr_compare(
			decl->init_array[offset].expr, expr);
		ofc_sema_expr_delete(expr);

		if (!equal)
		{
			ofc_sparse_ref_error(init->src,
				"Re-initialization of array element"
				" with different value");
			return false;
		}

		ofc_sparse_ref_warning(init->src,
			"Re-initialization of array element");
	}
	else
	{
		decl->init_array[offset].expr = expr;
	}

	return true;
}

bool ofc_sema_decl_init_array(
	ofc_sema_decl_t* decl,
	const ofc_sema_array_t* array,
	unsigned count,
	const ofc_sema_expr_t** init)
{
	if (!decl || !init
		|| ofc_sema_decl_is_procedure(decl))
		return false;

	if (count == 0)
		return true;

	if (decl->used)
	{
		ofc_sparse_ref_error(init[0]->src,
			"Can't initialize declaration after use");
		return false;
	}

	if (!decl->type)
		return false;

	if (!ofc_sema_decl_is_array(decl))
	{
		if (!array && (count == 1))
			return ofc_sema_decl_init(
				decl, init[0]);
		return false;
	}

	if (decl->init_array)
	{
		ofc_sparse_ref_warning(init[0]->src,
			"Initializing arrays in multiple statements.");
	}

	unsigned elem_count;
	if (!ofc_sema_decl_elem_count(
		decl, &elem_count))
	{
		ofc_sparse_ref_error(init[0]->src,
			"Can't initialize array of unknown size");
		return false;
	}
	if (elem_count == 0) return true;

	if (!decl->init_array)
	{
		decl->init_array = (ofc_sema_decl_init_t*)malloc(
			sizeof(ofc_sema_decl_init_t) * elem_count);
		if (!decl->init_array) return false;

		unsigned i;
		for (i = 0; i < elem_count; i++)
		{
			decl->init_array[i].is_substring = false;
			decl->init_array[i].expr = NULL;
		}
	}

	if (!array)
	{
		if (count > elem_count)
		{
			ofc_sparse_ref_warning(init[0]->src,
				"Array initializer too large, truncating.");
			count = elem_count;
		}

		unsigned i;
		for (i = 0; i < count; i++)
		{
			if (!ofc_sema_expr_is_constant(init[i]))
			{
				ofc_sparse_ref_error(init[i]->src,
					"Array initializer element not constant.");
				return false;
			}

			ofc_sema_expr_t* expr
				= ofc_sema_expr_copy(init[i]);
			if (!expr) return false;

			if (!ofc_sema_type_compatible(
				ofc_sema_expr_type(expr), decl->type))
			{
				ofc_sema_expr_t* cast
					= ofc_sema_expr_cast(
						expr, decl->type);
				if (!cast)
				{
					ofc_sparse_ref_error(init[i]->src,
						"Incompatible types in initializer");
					ofc_sema_expr_delete(expr);
					return false;
				}
				expr = cast;
			}

			if (decl->init_array[i].is_substring)
			{
				ofc_sparse_ref_error(init[i]->src,
					"Conflicting initializer types for array element");
				ofc_sema_expr_delete(expr);
				return false;
			}
			if (decl->init_array[i].expr)
			{
				bool equal = ofc_sema_expr_compare(
					decl->init_array[i].expr, expr);
				ofc_sema_expr_delete(expr);

				if (!equal)
				{
					ofc_sparse_ref_error(init[i]->src,
						"Re-initialization of array element"
						" with different value");
					return false;
				}

				ofc_sparse_ref_warning(init[i]->src,
					"Re-initialization of array element");
			}
			else
			{
				decl->init_array[i].expr = expr;
			}
		}
	}
	else
	{
		/* TODO - Initialize array slice. */
		return false;
	}

	return true;
}

bool ofc_sema_decl_init_func(
	ofc_sema_decl_t* decl,
	ofc_sema_scope_t* func)
{
	if (!ofc_sema_decl_is_procedure(decl))
		return false;

	if (decl->func)
		return (decl->func == func);

	decl->func = func;
	return true;
}


bool ofc_sema_decl_init_substring(
	ofc_sema_decl_t* decl,
	const ofc_sema_expr_t* init,
	const ofc_sema_expr_t* first,
	const ofc_sema_expr_t* last)
{
	if (!decl || !init
		|| !first || !last)
		return false;

	if (decl->used)
	{
		ofc_sparse_ref_error(init->src,
			"Can't initialize declaration after use");
		return false;
	}

	const ofc_sema_type_t* type
		= ofc_sema_decl_type(decl);
	if (!decl->init.is_substring
		&& (decl->init.expr != NULL))
	{
		/* TODO - Check if substring initializer is the same as
		          existing initializer contents and just warn. */

		ofc_sparse_ref_error(init->src,
			"Destination already has complete initializer");
		return false;
	}

	if (!ofc_sema_type_is_character(type))
	{
		ofc_sparse_ref_error(init->src,
			"Substring of non-CHARACTER type isn't supported");
		return false;
	}

	if (type->len_var)
	{
		ofc_sparse_ref_error(init->src,
			"Substring of variable length CHARACTER type isn't supported");
		return false;
	}

	if (ofc_sema_decl_is_array(decl))
	{
		/* TODO - Support substrings of arrays. */
		ofc_sparse_ref_error(init->src,
			"Substring of array type not currently supported");
		return false;
	}

    unsigned ufirst = 1;
	if (first && !ofc_sema_expr_resolve_uint(first, &ufirst))
	{
		ofc_sparse_ref_error(first->src,
			"Failed to resolve substring first index");
		return false;
	}

	unsigned ulast = type->len;
	if (last && !ofc_sema_expr_resolve_uint(last, &ulast))
	{
		ofc_sparse_ref_error(last->src,
			"Failed to resolve substring last index");
		return false;
	}

	if (!decl->init.is_substring
		&& (ufirst == 1)
		&& (ulast == type->len))
		return ofc_sema_decl_init(decl, init);

	if (ufirst > ulast)
	{
		ofc_sparse_ref_error(first->src,
			"Substring indices are reversed in initializer");
		/* TODO - Reverse the string and initialize with it? */
		return false;
	}

	if (ufirst == 0)
	{
		ofc_sparse_ref_error(first->src,
			"Substring indices are 1-based"
			", index zero is out-of-bounds");
		return false;
	}

	if (ulast > type->len)
	{
		ofc_sparse_ref_error(last->src,
			"Substring initializer out-of-bounds");
		return false;
	}

	if (ufirst == ulast)
	{
		ofc_sparse_ref_warning(first->src,
			"Initializing a zero-length substring has no effect");
		return true;
	}

	const ofc_sema_typeval_t* tv
		= ofc_sema_expr_constant(init);
	if (!tv)
	{
		ofc_sparse_ref_error(init->src,
			"Can't resolve substring initializer as constant");
		return false;
	}

	if (!ofc_sema_type_is_character(tv->type))
	{
		ofc_sparse_ref_error(init->src,
			"Substring initializer must be of type CHARACTER");
		return false;
	}

	unsigned offset = (ufirst - 1);
	unsigned len = (ulast - ufirst) + 1;

	if (tv->type->len > len)
	{
		ofc_sparse_ref_warning(init->src,
			"Substring initializer too long");
	}
	else if (tv->type->len < len)
	{
		ofc_sparse_ref_warning(init->src,
			"Substring initializer too short");
	}

	ofc_sema_typeval_t* ctv
		= ofc_sema_typeval_cast(tv, type);
	if (!ctv)
	{
		ofc_sparse_ref_error(init->src,
			"Failed to cast substring initializer to destination KIND");
		return false;
	}

	unsigned tsize;
	if (!ofc_sema_type_size(type, &tsize))
	{
		ofc_sema_typeval_delete(ctv);
		return false;
	}

	if (!decl->init.is_substring)
	{
		char* string = (char*)malloc(tsize);
		if (!string)
		{
			ofc_sema_typeval_delete(ctv);
			return false;
		}

		bool* mask = (bool*)malloc(
			sizeof(bool) * type->len);
		if (!mask)
		{
			free(string);
			ofc_sema_typeval_delete(ctv);
			return false;
		}

		unsigned i;
		for (i = 0; i < type->len; i++)
			mask[i] = false;

		decl->init.is_substring     = true;
		decl->init.substring.string = string;
		decl->init.substring.mask   = mask;
	}

	unsigned tcsize = tsize;
	if (type->len > 0) tcsize /= type->len;

	bool overlap = false;
	unsigned i, j;
	for (i = offset, j = 0; j < len; i++, j++)
	{
		if (decl->init.substring.mask[i])
		{
			if (memcmp(&decl->init.substring.string[i * tcsize],
				&ctv->character[j * tcsize], tcsize) != 0)
			{
				ofc_sparse_ref_error(init->src,
					"Re-initialization of substring,"
					" with different value at offset %u", (i + 1));
				ofc_sema_typeval_delete(ctv);
				return false;
			}

			overlap = true;
		}
		else
		{
			memcpy(&decl->init.substring.string[i * tcsize],
				&ctv->character[j * tcsize], tcsize);
			decl->init.substring.mask[i] = true;
		}
	}

	ofc_sema_typeval_delete(ctv);

	if (overlap)
	{
		ofc_sparse_ref_warning(init->src,
			"Overlapping initialization of substring");
	}

	return true;
}


bool ofc_sema_decl_size(
	const ofc_sema_decl_t* decl,
	unsigned* size)
{
	if (!decl) return false;

	unsigned acount = 1;
	if (decl->array && !ofc_sema_array_total(
		decl->array, &acount))
		return false;

	unsigned esize;
	if (!ofc_sema_type_size(
		decl->type, &esize))
		return false;

	if (size) *size = (acount * esize);
	return true;
}

bool ofc_sema_decl_elem_count(
	const ofc_sema_decl_t* decl,
	unsigned* count)
{
	if (!decl) return false;

	unsigned acount = 1;
	if (decl->array && !ofc_sema_array_total(
		decl->array, &acount))
		return false;

	unsigned scount = 1;
	if (decl->structure && !ofc_sema_structure_elem_count(
		decl->structure, &scount))
		return false;

	if (count) *count = (acount * scount);
	return true;
}

bool ofc_sema_decl_is_array(
	const ofc_sema_decl_t* decl)
{
	return (decl && decl->array);
}

bool ofc_sema_decl_is_structure(
	const ofc_sema_decl_t* decl)
{
	return (decl && decl->structure);
}

bool ofc_sema_decl_is_composite(
	const ofc_sema_decl_t* decl)
{
	if (!decl)
		return false;
	return (ofc_sema_decl_is_array(decl)
		|| ofc_sema_decl_is_structure(decl));
}


bool ofc_sema_decl_is_subroutine(
	const ofc_sema_decl_t* decl)
{
	return (decl && ofc_sema_type_is_subroutine(decl->type));
}

bool ofc_sema_decl_is_function(
	const ofc_sema_decl_t* decl)
{
	return (decl && ofc_sema_type_is_function(decl->type));
}

bool ofc_sema_decl_is_procedure(
	const ofc_sema_decl_t* decl)
{
	return (ofc_sema_decl_is_subroutine(decl)
		|| ofc_sema_decl_is_function(decl));
}


bool ofc_sema_decl_is_parameter(
	const ofc_sema_decl_t* decl)
{
	if (!decl) return false;
	return decl->is_parameter;
}

bool ofc_sema_decl_is_common(
	const ofc_sema_decl_t* decl)
{
	if (!decl) return false;
	return decl->is_common;
}


static bool ofc_sema_decl_init__used(
	ofc_sema_decl_init_t init,
	const ofc_sema_type_t* type,
	bool* complete)
{
	if (init.is_substring)
	{
		if (!type)
			return false;

		bool gap = false;
		unsigned i, s;
		for (i = 0, s = 0; i < type->len; i++)
		{
			if (!init.substring.mask[i])
				break;
			s++;
		}
		for (; i < type->len; i++)
		{
			if (init.substring.mask[i])
			{
				gap = true;
				s++;
			}
		}

		if (s == 0)
			return false;

		if (complete)
			*complete = !gap;
		return true;
	}

	if (!init.expr)
		return false;

	if (complete)
		*complete = true;
	return true;
}

bool ofc_sema_decl_has_initializer(
	const ofc_sema_decl_t* decl, bool* complete)
{
	if (!decl)
		return false;

	if (ofc_sema_decl_is_composite(decl))
	{
		if (!decl->init_array)
			return false;

		unsigned count;
		if (!ofc_sema_decl_elem_count(
			decl, &count))
			return false;

		bool partial = false;
		unsigned i, s;
		for (i = 0, s = 0; i < count; i++)
		{
			bool elem_complete;
			if (ofc_sema_decl_init__used(
				decl->init_array[i], decl->type,
				&elem_complete))
			{
				if (!elem_complete)
					partial = true;
				s++;
			}
		}

		if (s == 0)
			return false;

		if (complete)
			*complete = (!partial && (s == count));
		return true;
	}

	return ofc_sema_decl_init__used(
		decl->init, decl->type, complete);
}


const ofc_sema_type_t* ofc_sema_decl_type(
	const ofc_sema_decl_t* decl)
{
	return (decl ? decl->type : NULL);
}

const ofc_sema_type_t* ofc_sema_decl_base_type(
	const ofc_sema_decl_t* decl)
{
	return ofc_sema_type_base(
		ofc_sema_decl_type(decl));
}


bool ofc_sema_decl_foreach_expr(
	ofc_sema_decl_t* decl, void* param,
	bool (*func)(ofc_sema_expr_t* expr, void* param))
{
	if (!decl || !func)
		return false;

	if (decl->array && !ofc_sema_array_foreach_expr(
		decl->array, param, func))
		return false;

	if (ofc_sema_decl_is_composite(decl))
	{
		if (decl->init_array)
		{
			unsigned count;
			if (!ofc_sema_decl_elem_count(
				decl, &count))
				return false;

			unsigned i;
			for (i = 0; i < count; i++)
			{
				if (decl->init_array[i].is_substring
					|| !decl->init_array[i].expr)
					continue;

				if (!func(decl->init_array[i].expr, param))
					return false;
			}
		}
	}
	else
	{
		if (!decl->init.is_substring && decl->init.expr
			&& !func(decl->init.expr, param))
			return false;
	}

	return true;
}

bool ofc_sema_decl_foreach_scope(
	ofc_sema_decl_t* decl, void* param,
	bool (*func)(ofc_sema_scope_t* scope, void* param))
{
	if (!decl || !func)
		return false;

	if (decl->func && !ofc_sema_scope_foreach_scope(
		decl->func, param, func))
		return false;

	return true;
}


static const ofc_str_ref_t* ofc_sema_decl__key(
	const ofc_sema_decl_t* decl)
{
	return (decl ? &decl->name.string : NULL);
}

bool ofc_sema_decl_list__remap(
	ofc_sema_decl_list_t* list)
{
	if (!list)
		return false;

	if (list->map)
		ofc_hashmap_delete(list->map);

	return (list->map != NULL);
}

static ofc_sema_decl_list_t* ofc_sema_decl_list__create(
	bool case_sensitive, bool is_ref)
{
	ofc_sema_decl_list_t* list
		= (ofc_sema_decl_list_t*)malloc(
			sizeof(ofc_sema_decl_list_t));
	if (!list) return NULL;

	list->case_sensitive = case_sensitive;

	list->count  = 0;
	list->decl   = NULL;
	list->is_ref = is_ref;

	list->map = ofc_hashmap_create(
		(void*)(list->case_sensitive
			? ofc_str_ref_ptr_hash
			: ofc_str_ref_ptr_hash_ci),
		(void*)(list->case_sensitive
			? ofc_str_ref_ptr_equal
			: ofc_str_ref_ptr_equal_ci),
		(void*)ofc_sema_decl__key, NULL);
	if (!list->map)
	{
		free(list);
		return NULL;
	}

	return list;
}

ofc_sema_decl_list_t* ofc_sema_decl_list_create(
	bool case_sensitive)
{
	return ofc_sema_decl_list__create(
		case_sensitive, false);
}

ofc_sema_decl_list_t* ofc_sema_decl_list_create_ref(
	bool case_sensitive)
{
	return ofc_sema_decl_list__create(
		case_sensitive, true);
}

void ofc_sema_decl_list_delete(
	ofc_sema_decl_list_t* list)
{
	if (!list)
		return;

	ofc_hashmap_delete(list->map);

	if (!list->is_ref)
	{
		unsigned i;
		for (i = 0; i < list->count; i++)
			ofc_sema_decl_delete(list->decl[i]);
	}

	free(list->decl);

	free(list);
}

bool ofc_sema_decl_list_add(
	ofc_sema_decl_list_t* list,
	ofc_sema_decl_t* decl)
{
	if (!list || !decl
		|| list->is_ref)
		return false;

	/* Check for duplicate definitions. */
	if (ofc_sema_decl_list_find(
		list, decl->name.string))
		return false;

	ofc_sema_decl_t** ndecl
		= (ofc_sema_decl_t**)realloc(list->decl,
			(sizeof(ofc_sema_decl_t*) * (list->count + 1)));
	if (!ndecl) return false;
	list->decl = ndecl;

	if (!ofc_hashmap_add(
		list->map, decl))
		return false;

	list->decl[list->count++] = decl;

	return true;
}

bool ofc_sema_decl_list_add_ref(
	ofc_sema_decl_list_t* list,
	const ofc_sema_decl_t* decl)
{
	if (!list || !decl
		|| !list->is_ref)
		return false;

	/* Check for duplicate definitions. */
	if (ofc_sema_decl_list_find(
		list, decl->name.string))
		return false;

	const ofc_sema_decl_t** ndecl
		= (const ofc_sema_decl_t**)realloc(list->decl_ref,
			(sizeof(const ofc_sema_decl_t*) * (list->count + 1)));
	if (!ndecl) return false;
	list->decl_ref = ndecl;

	if (!ofc_hashmap_add(
		list->map, (void*)decl))
		return false;

	list->decl_ref[list->count++] = decl;
	return true;
}

const ofc_sema_decl_t* ofc_sema_decl_list_find(
	const ofc_sema_decl_list_t* list, ofc_str_ref_t name)
{
	if (!list)
		return NULL;

	return ofc_hashmap_find(
		list->map, &name);
}

ofc_sema_decl_t* ofc_sema_decl_list_find_modify(
	ofc_sema_decl_list_t* list, ofc_str_ref_t name)
{
	if (!list)
		return NULL;

	return ofc_hashmap_find_modify(
		list->map, &name);
}

const ofc_hashmap_t* ofc_sema_decl_list_map(
	const ofc_sema_decl_list_t* list)
{
	return (list ? list->map : NULL);
}

bool ofc_sema_decl_print_name(ofc_colstr_t* cs,
	const ofc_sema_decl_t* decl)
{
	if (!decl) return false;
	return ofc_sparse_ref_print(cs, decl->name);
}

bool ofc_sema_decl_print(ofc_colstr_t* cs,
	unsigned indent,
	const ofc_sema_decl_t* decl)
{
	if (!decl || !decl->type)
		return false;

	/* If there's a used specifier we print that instead and handle
	   initialization later using a DATA statement. */
	if (decl->has_spec)
		return true;

	if (!ofc_colstr_newline(cs, indent, NULL))
		return false;

	const ofc_sema_type_t* type = NULL;
	bool is_pointer = false;
	if (decl->type->type == OFC_SEMA_TYPE_TYPE)
	{
		if (!decl->structure)
			return false;

		if (decl->structure->type
			== OFC_SEMA_STRUCTURE_F90_TYPE)
		{
			if (!ofc_colstr_atomic_writef(cs, "TYPE")
				|| !ofc_colstr_atomic_writef(cs, "(")
				|| !ofc_sema_structure_print_name(cs, decl->structure)
				|| !ofc_colstr_atomic_writef(cs, ")"))
				return false;
		}
		else
		{
			return (ofc_colstr_atomic_writef(cs, "RECORD")
				&& ofc_colstr_atomic_writef(cs, " ")
				&& ofc_colstr_atomic_writef(cs, "/")
				&& ofc_sema_structure_print_name(cs, decl->structure)
				&& ofc_colstr_atomic_writef(cs, "/")
				&& ofc_colstr_atomic_writef(cs, " ")
				&& ofc_sema_decl_print_name(cs, decl));
		}
	}
	else
	{
		type = decl->type;
		if (!type) return false;

		is_pointer = (type->type == OFC_SEMA_TYPE_POINTER);
		if (is_pointer || ofc_sema_decl_is_function(decl))
		{
			type = type->subtype;
			if (!type) return false;
		}

		/* We can't support nested pointer types. */
		if (type->type == OFC_SEMA_TYPE_POINTER)
			return false;

		if (!ofc_sema_type_print(cs, type))
			return false;
	}

	if (decl->is_external)
	{
		if (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_atomic_writef(cs, "EXTERNAL"))
			return false;
	}

	if (decl->is_intrinsic)
	{
		if (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_atomic_writef(cs, "INTRINSIC"))
			return false;
	}

	if (decl->is_volatile)
	{
		if (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_atomic_writef(cs, "VOLATILE"))
			return false;
	}

	if (decl->is_static)
	{
		if (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_atomic_writef(cs, "SAVE"))
			return false;
	}

	if (ofc_sema_decl_is_parameter(decl))
	{
		if (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_atomic_writef(cs, "PARAMETER"))
			return false;
	}

	if (is_pointer)
	{
		if (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_atomic_writef(cs, "POINTER"))
			return false;
	}

	if (decl->is_target)
	{
		if (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_atomic_writef(cs, "TARGET"))
			return false;
	}

	if (ofc_sema_decl_is_array(decl))
	{
		if (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_atomic_writef(cs, "DIMENSION")
			|| !ofc_sema_array_print_brackets(cs, decl->array))
			return false;
	}

	if (!ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_colstr_atomic_writef(cs, "::")
		|| !ofc_colstr_atomic_writef(cs, " "))
		return false;

	if (!ofc_sema_decl_print_name(cs, decl))
		return false;

	/* TODO - Handle POINTER initializers. */

	bool init_complete = false;
	ofc_sema_decl_has_initializer(
		decl, &init_complete);

	if (init_complete)
	{
		if (!ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_atomic_writef(cs, "=")
			|| !ofc_colstr_atomic_writef(cs, " "))
			return false;

		if (ofc_sema_decl_is_composite(decl))
		{
			bool reshape = (ofc_sema_decl_is_array(decl)
				&& (decl->array->dimensions > 1));

			if (reshape)
			{
				if (!ofc_colstr_atomic_writef(cs, "RESHAPE")
					|| !ofc_colstr_atomic_writef(cs, "("))
					return false;
			}

			if (!ofc_colstr_atomic_writef(cs, "(/")
				|| !ofc_colstr_atomic_writef(cs, " "))
				return false;

			unsigned count;
			if (!ofc_sema_decl_elem_count(decl, &count))
				return false;

			unsigned i;
			for (i = 0; i < count; i++)
			{
				if (i > 0)
				{
					if (!ofc_colstr_atomic_writef(cs, ",")
						|| !ofc_colstr_atomic_writef(cs, " "))
						return false;
				}

				if (decl->init_array[i].is_substring)
				{
					unsigned len;
					for (len = 0; len < type->len; len++)
					{
						if (!decl->init_array[i].substring.mask[len])
							break;
					}

					if (!ofc_colstr_write_escaped(cs, '\"',
						decl->init_array[i].substring.string, len))
						return false;
				}
				else if (!ofc_sema_expr_print(
					cs, decl->init_array[i].expr))
				{
					return false;
				}
			}

			if (!ofc_colstr_atomic_writef(cs, " ")
				|| !ofc_colstr_atomic_writef(cs, "/)"))
				return false;

			if (reshape)
			{
				if (!ofc_colstr_atomic_writef(cs, ",")
					|| !ofc_colstr_atomic_writef(cs, " ")
					|| !ofc_colstr_atomic_writef(cs, "(/")
					|| !ofc_colstr_atomic_writef(cs, " ")
					|| !ofc_sema_array_print_size(cs, decl->array)
					|| !ofc_colstr_atomic_writef(cs, " ")
					|| !ofc_colstr_atomic_writef(cs, "/)")
					|| !ofc_colstr_atomic_writef(cs, ")"))
					return false;
			}
		}
		else
		{
			if (decl->init.is_substring)
			{
				unsigned len;
				for (len = 0; len < type->len; len++)
				{
					if (!decl->init.substring.mask[len])
						break;
				}

				if (!ofc_colstr_write_escaped(cs, '\"',
					decl->init.substring.string, len))
					return false;
			}
			else if (!ofc_sema_expr_print(cs, decl->init.expr))
				return false;
		}
	}

	return true;
}

static bool ofc_sema_decl_print_data_init__ssn(
	ofc_colstr_t* cs,
	const ofc_sema_decl_t* decl,
	bool* first,
	unsigned base, unsigned size)
{
	if (size == 0)
		return true;

	if (!*first)
	{
		if (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " "))
			return false;
	}

	*first = false;

	return (ofc_sparse_ref_print(cs, decl->name)
		&& ofc_colstr_atomic_writef(cs, "(")
		&& ofc_colstr_atomic_writef(cs, "%u", (base + 1))
		&& ofc_colstr_atomic_writef(cs, ":")
		&& ofc_colstr_atomic_writef(cs, "%u", (base + size))
		&& ofc_colstr_atomic_writef(cs, ")"));
}


static bool ofc_sema_decl_print_data_init__ssc(
	ofc_colstr_t* cs,
	char* string,
	bool* first,
	unsigned base, unsigned size)
{
	if (size == 0)
		return true;

	if (!*first)
	{
		if (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " "))
			return false;
	}

	*first = false;

	return ofc_colstr_write_escaped(
		cs, '\"', &string[base], size);
}

bool ofc_sema_decl_print_data_init(ofc_colstr_t* cs,
	unsigned indent,
	const ofc_sema_decl_t* decl)
{
	if (!decl)
		return false;

	bool complete;
	if (!ofc_sema_decl_has_initializer(
		decl, &complete))
		return true;

	if (complete && !decl->has_spec)
		return true;

	if (!ofc_colstr_newline(cs, indent, NULL)
		|| !ofc_colstr_atomic_writef(cs, "DATA")
		|| !ofc_colstr_atomic_writef(cs, " "))
		return false;

	if (complete)
	{
		/* TODO - Print complete deferred initializers. */
		return false;
	}
	else if (ofc_sema_decl_is_array(decl))
	{
		if (decl->structure)
		{
			/* TODO - Support structure arrays. */
			return false;
		}

		unsigned count;
		if (!ofc_sema_decl_elem_count(
			decl, &count))
			return false;

		/* TODO - Group by nlist in slices for a cleaner print. */

		bool first;
		unsigned i;
		for (i = 0, first = true; i < count; i++)
		{
			if (!decl->init_array[i].is_substring
				&& !decl->init_array[i].expr)
				continue;

			if (!first)
			{
				if (!ofc_colstr_atomic_writef(cs, ",")
					|| !ofc_colstr_atomic_writef(cs, " "))
					return false;
			}
			first = false;

			ofc_sema_array_index_t* index
				= ofc_sema_array_index_from_offset(decl, i);
			if (!index) return false;

			bool success = (ofc_sema_decl_print_name(cs, decl)
				&& ofc_sema_array_index_print(cs, index));
			ofc_sema_array_index_delete(index);
			if (!success) return false;
		}

		if (!ofc_colstr_atomic_writef(cs, "/"))
			return false;

		/* TODO - Compress repetitions in clist for a cleaner print. */

		for (i = 0, first = true; i < count; i++)
		{
			if (!decl->init_array[i].is_substring
				&& !decl->init_array[i].expr)
				continue;

			if (!first)
			{
				if (!ofc_colstr_atomic_writef(cs, ",")
					|| !ofc_colstr_atomic_writef(cs, " "))
					return false;
			}
			first = false;

			if (decl->init_array[i].is_substring)
			{
				bool u = false;
				unsigned j, l;
				for (j = 0, l = 0; j < decl->type->len; j++)
				{
					if (decl->init_array[i].substring.mask[j])
					{
						if (u)
						{
							/* TODO - Support arrays of substrings. */
							return false;
						}
						l++;
					}
					else
					{
						u = true;
					}
				}

				if (!ofc_colstr_write_escaped(cs, '\"',
						decl->init_array[i].substring.string, l))
					return false;
			}
			else
			{
				if (!ofc_sema_expr_print(
					cs, decl->init_array[i].expr))
					return false;
			}
		}

		if (!ofc_colstr_atomic_writef(cs, "/"))
			return false;
	}
	else if (ofc_sema_decl_is_structure(decl))
	{
		unsigned count;
		if (!ofc_sema_decl_elem_count(
			decl, &count))
			return false;

		bool first;
		unsigned i;
		for (i = 0, first = true; i < count; i++)
		{
			if (!decl->init_array[i].is_substring
				&& !decl->init_array[i].expr)
				continue;

			if (!first)
			{
				if (!ofc_colstr_atomic_writef(cs, ",")
					|| !ofc_colstr_atomic_writef(cs, " "))
					return false;
			}
			first = false;

			ofc_sema_decl_t* member
				= ofc_sema_structure_elem_get(
					decl->structure, i);
			if (!member) return false;

			if (!ofc_sema_decl_print_name(cs, decl)
				|| !ofc_sema_structure_elem_print(
					cs, decl->structure, i))
				return false;
		}

		if (!ofc_colstr_atomic_writef(cs, "/"))
			return false;

		for (i = 0, first = true; i < count; i++)
		{
			if (!decl->init_array[i].is_substring
				&& !decl->init_array[i].expr)
				continue;

			if (!first)
			{
				if (!ofc_colstr_atomic_writef(cs, ",")
					|| !ofc_colstr_atomic_writef(cs, " "))
					return false;
			}
			first = false;

			if (decl->init_array[i].is_substring)
			{
				bool u = false;
				unsigned j, l;
				for (j = 0, l = 0; j < decl->type->len; j++)
				{
					if (decl->init_array[i].substring.mask[j])
					{
						if (u)
						{
							/* TODO - Support structures containing substrings. */
							return false;
						}
						l++;
					}
					else
					{
						u = true;
					}
				}

				if (!ofc_colstr_write_escaped(cs, '\"',
						decl->init_array[i].substring.string, l))
					return false;
			}
			else
			{
				if (!ofc_sema_expr_print(
					cs, decl->init_array[i].expr))
					return false;
			}
		}

		if (!ofc_colstr_atomic_writef(cs, "/"))
			return false;
	}
	else if (ofc_sema_type_is_character(decl->type))
	{
		unsigned csize = 0;
		if (!ofc_sema_type_base_size(
			decl->type, &csize))
			return false;
		if (csize != 1)
		{
			/* TODO - Support strings with non-byte character size. */
			return false;
		}

		bool first;
		unsigned i, b, s;

		for (i = 0, b = 0, s = 0, first = true;
			i < decl->type->len; i++)
		{
			if (decl->init.substring.mask[i])
			{
				s++;
			}
			else
			{
				if (!ofc_sema_decl_print_data_init__ssn(
					cs, decl, &first, b, s))
					return false;
				b = (i + 1);
				s = 0;
			}
		}
		if (!ofc_sema_decl_print_data_init__ssn(
			cs, decl, &first, b, s))
			return false;

		if (!ofc_colstr_atomic_writef(cs, "/"))
			return false;

		for (i = 0, b = 0, s = 0, first = true;
			i < decl->type->len; i++)
		{
			if (decl->init.substring.mask[i])
			{
				s++;
			}
			else
			{
				if (!ofc_sema_decl_print_data_init__ssc(
					cs, decl->init.substring.string, &first, b, s))
					return false;
				b = (i + 1);
				s = 0;
			}
		}
		if (!ofc_sema_decl_print_data_init__ssc(
			cs, decl->init.substring.string, &first, b, s))
			return false;

		if (!ofc_colstr_atomic_writef(cs, "/"))
			return false;
	}

	return true;
}

bool ofc_sema_decl_list_stmt_func_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_sema_decl_list_t* decl_list)
{
    if (!cs || !decl_list)
		return false;

	unsigned i;
	for (i = 0; i < decl_list->count; i++)
	{
		ofc_sema_decl_t* decl = decl_list->decl[i];
		if (decl->func
			&& (decl->func->type == OFC_SEMA_SCOPE_STMT_FUNC))
		{
			if (!ofc_colstr_newline(cs, indent, NULL)
				|| !ofc_sema_decl_print_name(cs, decl)
				|| !ofc_colstr_atomic_writef(cs, "(")
				|| (decl->func->args && !ofc_sema_arg_list_print(cs,
					decl->func->args))
				|| !ofc_colstr_atomic_writef(cs, ")")
				|| !ofc_colstr_atomic_writef(cs, " ")
				|| !ofc_colstr_atomic_writef(cs, "=")
				|| !ofc_colstr_atomic_writef(cs, " ")
				|| !ofc_sema_scope_print(cs, indent, decl->func))
				return false;
		}
	}

	return true;
}

bool ofc_sema_decl_list_procedure_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_sema_decl_list_t* decl_list)
{
    if (!cs || !decl_list)
		return false;

	unsigned i;
	for (i = 0; i < decl_list->count; i++)
	{
		ofc_sema_decl_t* decl = decl_list->decl[i];
		if (decl->func)
		{
			if (decl->func->type == OFC_SEMA_SCOPE_SUBROUTINE)
			{
				if (!ofc_colstr_newline(cs, indent, NULL)
					|| !ofc_sema_scope_print(cs, indent, decl->func))
					return false;
			}
			else if (decl->func->type == OFC_SEMA_SCOPE_FUNCTION)
			{
				if (!ofc_colstr_newline(cs, indent, NULL)
					|| !ofc_colstr_newline(cs, indent, NULL)
					|| !ofc_sema_type_print(cs, decl->type->subtype)
					|| !ofc_colstr_atomic_writef(cs, " ")
					|| !ofc_sema_scope_print(cs, indent, decl->func))
					return false;
			}
		}
	}

	return true;
}

bool ofc_sema_decl_list_procedure_spec_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_sema_decl_list_t* decl_list)
{
	if (!cs || !decl_list)
		return false;

	unsigned i;
	for (i = 0; i < decl_list->count; i++)
	{
		ofc_sema_decl_t* decl = decl_list->decl[i];
		if (!ofc_sema_decl_is_procedure(decl)
			|| ofc_sema_decl_is_subroutine(decl))
			continue;

		if (!ofc_sema_decl_print(
			cs, indent, decl))
			return false;
	}

	return true;
}

bool ofc_sema_decl_list_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_sema_decl_list_t* decl_list)
{
	if (!cs || !decl_list)
		return false;

	unsigned i;
	for (i = 0; i < decl_list->count; i++)
	{
		/* Don't print specifiers for declared procedures. */
		if (ofc_sema_decl_is_procedure(decl_list->decl[i])
			|| decl_list->decl[i]->is_return)
			continue;

		if (!ofc_sema_decl_print(cs, indent,
			decl_list->decl[i]))
			return false;
	}

	for (i = 0; i < decl_list->count; i++)
	{
		if (ofc_sema_decl_is_procedure(decl_list->decl[i])
			|| decl_list->decl[i]->is_return)
			continue;

		if (!ofc_sema_decl_print_data_init(cs, indent,
			decl_list->decl[i]))
			return false;
	}

	return true;
}


bool ofc_sema_decl_list_foreach(
	ofc_sema_decl_list_t* list, void* param,
	bool (*func)(ofc_sema_decl_t* decl, void* param))
{
	if (!list || !func)
		return false;

	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		if (!func(list->decl[i], param))
			return false;
	}

	return true;
}

bool ofc_sema_decl_list_foreach_expr(
	ofc_sema_decl_list_t* list, void* param,
	bool (*func)(ofc_sema_expr_t* expr, void* param))
{
	if (!list)
		return false;

	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		if (!ofc_sema_decl_foreach_expr(
			list->decl[i], param, func))
			return false;
	}

	return true;
}

bool ofc_sema_decl_list_foreach_scope(
	ofc_sema_decl_list_t* list, void* param,
	bool (*func)(ofc_sema_scope_t* scope, void* param))
{
	if (!list)
		return false;

	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		if (!ofc_sema_decl_foreach_scope(
			list->decl[i], param, func))
			return false;
	}

	return true;
}
