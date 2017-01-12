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


bool ofc_sema_decl_is_final(
	const ofc_sema_decl_t* decl)
{
	return (decl && decl->type_final);
}

ofc_sema_decl_t* ofc_sema_decl_create(
	const ofc_sema_implicit_t* implicit,
	ofc_sparse_ref_t name)
{
	ofc_sema_decl_t* decl
		= (ofc_sema_decl_t*)malloc(
			sizeof(ofc_sema_decl_t));
	if (!decl) return NULL;

	decl->name = name;

	decl->type = NULL;
	decl->type_implicit = true;
	decl->type_final    = false;

	decl->access = OFC_SEMA_ACCESSIBILITY_DEFAULT;

	decl->is_static    = false;
	decl->is_automatic = false;
	decl->is_volatile  = false;
	decl->is_intrinsic = false;
	decl->is_external  = false;

	if (implicit && !ofc_sparse_ref_empty(name))
	{
		if (!ofc_sema_implicit_attr(
			implicit, name,
			&decl->type,
			&decl->is_static,
			&decl->is_automatic,
			&decl->is_volatile,
			&decl->is_intrinsic,
			&decl->is_external))
		{
			free(decl);
			return NULL;
		}
	}

	decl->func      = NULL;
	decl->array     = NULL;
	decl->structure = NULL;
	decl->common    = NULL;
	decl->intrinsic = NULL;

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
	decl->is_target    = false;

	decl->is_equiv     = false;
	decl->is_argument  = false;
	decl->is_return    = false;

	decl->was_read    = false;
	decl->was_written = false;
	decl->is_stmt_func_arg = false;

	decl->refcnt = 0;
	return decl;
}


bool ofc_sema_decl_type_finalize(
	ofc_sema_decl_t* decl)
{
	if (!decl || !decl->type)
		return false;

	const ofc_sema_type_t* ktype = decl->type;
	if (decl->type->kind == OFC_SEMA_KIND_NONE)
	{
		switch (decl->type->type)
		{
			case OFC_SEMA_TYPE_LOGICAL:
			case OFC_SEMA_TYPE_INTEGER:
			case OFC_SEMA_TYPE_REAL:
			case OFC_SEMA_TYPE_COMPLEX:
			case OFC_SEMA_TYPE_BYTE:
			case OFC_SEMA_TYPE_CHARACTER:
				ktype = ofc_sema_type_set_kind(
					decl->type, OFC_SEMA_KIND_DEFAULT);
				if (!ktype) return false;
				break;

			default:
				break;
		}
	}

	const ofc_sema_type_t* ltype = ktype;
	if (ofc_sema_type_is_character(ktype)
		&& (ktype->len == 0) && !ktype->len_var)
	{
		ltype = ofc_sema_type_set_len(ktype, 1, false);
		if (!ltype) return false;
	}

	/* Externals are always functions or subroutines.
	   Subroutines are only used in CALL statements,
	   so we always know its type */
	if (decl->is_external
		&& !ofc_sema_decl_is_subroutine(decl)
		&& !ofc_sema_decl_is_function(decl))
	{
		ltype = ofc_sema_type_create_function(ltype);
		if (!ltype) return false;
	}

	decl->type = ltype;
	decl->type_implicit = false;
	decl->type_final    = true;
	return true;
}


bool ofc_sema_decl_function(
	ofc_sema_decl_t* decl)
{
	if (!decl)
		return false;

	if (ofc_sema_decl_is_function(decl))
		return true;

	if (decl->is_stmt_func_arg
		|| ofc_sema_decl_is_final(decl)
		|| !ofc_sema_decl_type_finalize(decl))
		return false;

	/* Re-check in case it was finalized as a function */
	if (ofc_sema_decl_is_function(decl))
		return true;

	const ofc_sema_type_t* ftype
		= ofc_sema_type_create_function(decl->type);
	if (!ftype) return false;

	decl->type = ftype;
	return true;
}

bool ofc_sema_decl_subroutine(
	ofc_sema_decl_t* decl)
{
	if (!decl)
		return false;

	if (decl->type && !decl->type_implicit)
		return false;

	decl->type = ofc_sema_type_subroutine();
	if (!decl->type) return false;

	decl->type_implicit = false;
	return true;
}

bool ofc_sema_decl_type_set(
	ofc_sema_decl_t*       decl,
	const ofc_sema_type_t* type,
	ofc_sparse_ref_t       err_pos)
{
	if (!decl || !type)
		return false;

	if (!decl->type
		|| decl->type_implicit)
	{
		decl->type = type;
		decl->type_implicit = false;
		return true;
	}

	if (decl->type->type != type->type)
	{
		ofc_sparse_ref_error(err_pos,
			"Redefining typed declaration as a different type");
		return false;
	}

	const ofc_sema_type_t* dtype = decl->type;

	if ((dtype->kind != type->kind)
		&& (type->kind != OFC_SEMA_KIND_NONE))
	{
		if (dtype->kind != OFC_SEMA_KIND_NONE)
		{
			ofc_sparse_ref_error(err_pos,
				"Redefining typed declaration with a different KIND");
			return false;
		}

		dtype = ofc_sema_type_set_kind(
				dtype, type->kind);
		if (!dtype) return false;
	}

	if (ofc_sema_type_is_character(dtype)
		&& ((type->len != 0) || type->len_var)
		&& ((dtype->len != type->len)
			|| (dtype->len_var != type->len_var)))
	{
		if (dtype->len != 0)
		{
			ofc_sparse_ref_error(err_pos,
				"Redefining typed declaration with a different LEN");
			return false;
		}

		if (type->len != 0)
		{
			dtype = ofc_sema_type_set_len(
				dtype, type->len, false);
		}
		else if ((dtype->len == 0)
			&& type->len_var)
		{
			dtype = ofc_sema_type_set_len(
				dtype, 0, true);
		}

		if (!dtype) return false;
	}

	decl->type = dtype;
	decl->type_implicit = false;
	return true;
}

bool ofc_sema_decl_array_set(
	ofc_sema_decl_t*  decl,
	ofc_sema_array_t* array,
	ofc_sparse_ref_t  err_pos)
{
	if (!decl || !array)
		return false;

	if (decl->array)
	{
		if (!ofc_sema_array_compare(
			decl->array, array))
		{
			ofc_sparse_ref_error(err_pos,
				"Conflicting array dimensions specified");
			return false;
		}

		ofc_sparse_ref_warning(err_pos,
			"Redundant array dimension specification");
		ofc_sema_array_delete(array);
		return true;
	}

	if (ofc_sema_decl_is_final(decl))
	{
		ofc_sparse_ref_error(err_pos,
			"Can't modify dimensions of declaration after use");
		return false;
	}

	decl->array = array;
	return true;
}

bool ofc_sema_decl_mark_used(
	ofc_sema_decl_t* decl,
	bool written, bool read)
{
    if (!decl)
		return false;

	if (!written && !read)
		return true;

	if (written)
		decl->was_written = true;
	if (read)
		decl->was_read = true;
	return true;
}


static bool ofc_sema_decl__elem(
	ofc_sema_scope_t*        scope,
	const ofc_sema_type_t*   type,
	ofc_sema_structure_t**   type_struct,
	ofc_sema_structure_t*    structure,
	ofc_parse_array_index_t* dimension,
	bool                     is_static,
	bool                     is_parameter,
	bool                     is_public,
	bool                     is_private,
	bool                     type_scan,
	const ofc_parse_decl_t*  pdecl)
{
	if (!pdecl || !pdecl->lhs)
		return false;

	if (!ofc_sparse_ref_empty(pdecl->record)
		&& type_scan)
	{
		/* TODO - Handle RECORDs in type scan pass. */
		return true;
	}

	const ofc_parse_lhs_t* lhs = pdecl->lhs;

	ofc_sparse_ref_t name;
	if (!ofc_parse_lhs_base_name(*lhs, &name))
		return false;

	ofc_sema_decl_t* decl;
	if (structure)
	{
		decl = ofc_sema_structure_decl_find_create(
			structure, name);
	}
	else
	{
		decl = ofc_sema_scope_decl_find_create(
			scope, name, false);
	}
	if (!decl) return false;

	if (decl->is_intrinsic)
	{
		ofc_sparse_ref_error(lhs->src,
			"Invalid declaration of an INTRINSIC");
		return false;
	}

	ofc_sema_array_t* array = (type_scan
		? ofc_sema_array_scan(scope, dimension)
		: ofc_sema_array(scope, dimension));

	if (lhs->type == OFC_PARSE_LHS_ARRAY)
	{
		ofc_sema_array_t* lhs_array = (type_scan
			? ofc_sema_array_scan(scope, lhs->array.index)
			: ofc_sema_array(scope, lhs->array.index));
		if (!lhs_array) return false;

		if (array)
		{
			bool conflict = !ofc_sema_array_compare(array, lhs_array);
			ofc_sema_array_delete(lhs_array);
			if (conflict)
			{
				ofc_sparse_ref_error(lhs->src,
					"Multiple incompatible definitions of array dimensions");
				ofc_sema_array_delete(array);
				return false;
			}
			else
			{
				ofc_sparse_ref_warning(lhs->src,
					"Redefinition of array dimensions");
			}
		}
		else
		{
			array = lhs_array;
		}

		lhs = lhs->parent;
		if (!lhs)
		{
			ofc_sema_array_delete(array);
			return false;
		}
	}

	if (lhs->type == OFC_PARSE_LHS_STAR_LEN)
	{
		if (!type)
		{
			ofc_sparse_ref_error(lhs->src,
				"Can't specify a star LEN/KIND for an undefined type");
			ofc_sema_array_delete(array);
			return false;
		}

		if (lhs->star_len.var)
		{
			if (!ofc_sema_type_is_character(type))
			{
				ofc_sparse_ref_error(lhs->src,
					"Only CHARACTER types may have an assumed length");
				ofc_sema_array_delete(array);
				return false;
			}

			if (type && (type->len != 0))
			{
				ofc_sparse_ref_warning(lhs->src,
					"Overriding specified star length in decl list");
			}

			const ofc_sema_type_t* ntype
				= ofc_sema_type_set_len(
					type, 0, true);
			if (!ntype)
			{
				ofc_sema_array_delete(array);
				return false;
			}
			type = ntype;
		}
		else if (!type_scan || !ofc_sema_type_is_character(type))
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

			if (ofc_sema_type_is_character(type))
			{
				if (type && (type->len != 0)
					&& (type->len != star_len))
				{
					ofc_sparse_ref_warning(lhs->src,
						"Overriding specified LEN in decl list");
				}

				const ofc_sema_type_t* ntype
					= ofc_sema_type_set_len(
						type, star_len, false);
				if (!ntype)
				{
					ofc_sema_array_delete(array);
					return false;
				}
				type = ntype;
			}
			else
			{
				if (star_len == 0)
				{
					ofc_sparse_ref_error(lhs->src,
						"Star KIND must be non-zero");
					ofc_sema_array_delete(array);
					return false;
				}

				if (type && (type->kind != OFC_SEMA_KIND_NONE)
					&& (type->kind != star_len))
				{
					ofc_sparse_ref_warning(lhs->src,
						"Overriding specified KIND in decl list");
				}

				ofc_sema_kind_e kind
					= (star_len * OFC_SEMA_KIND_1_BYTE);

				const ofc_sema_type_t* ntype
					= ofc_sema_type_set_kind(
						type, kind);
				if (!ntype)
				{
					ofc_sema_array_delete(array);
					return false;
				}
				type = ntype;
			}
		}

		lhs = lhs->parent;
		if (!lhs)
		{
			ofc_sema_array_delete(array);
			return false;
		}
	}

	if (!ofc_sparse_ref_empty(pdecl->record))
	{
		if (!decl->type_implicit)
		{
			ofc_sparse_ref_error(pdecl->record,
				"Redefining typed declaration as RECORD");
			ofc_sema_array_delete(array);
			return false;
		}

		ofc_sema_structure_t* rstruct
			= ofc_sema_scope_structure_find(
				scope, pdecl->record.string);
		if (!rstruct)
		{
			rstruct = ofc_sema_scope_derived_type_find(
				scope, pdecl->record.string);
			if (!rstruct)
			{
				ofc_sparse_ref_error(pdecl->record,
					"Referencing undefined STRUCTURE in RECORD statement");
				ofc_sema_array_delete(array);
				return false;
			}
			ofc_sparse_ref_warning(pdecl->record,
				"Referencing Fortran 90 TYPE in VAX RECORD statement");
		}

		if (decl->structure)
		{
			if (decl->structure != rstruct)
			{
				ofc_sparse_ref_error(pdecl->record,
					"Redefining declaration as RECORD of a different type");
				ofc_sema_array_delete(array);
				return false;
			}
			ofc_sparse_ref_warning(pdecl->record,
				"Duplicate RECORD statement");
		}
		else
		{
			if (!ofc_sema_structure_reference(rstruct))
			{
				ofc_sema_array_delete(array);
				return false;
			}

			decl->structure = rstruct;
			if (type_struct)
				*type_struct = rstruct;
		}

		decl->type = ofc_sema_type_type();
		if (!decl->type)
		{
			ofc_sema_array_delete(array);
			return false;
		}
		decl->type_implicit = false;
	}
	else if (type_struct && *type_struct)
	{
		if (!ofc_sema_structure_reference(*type_struct))
			return false;

		decl->structure = *type_struct;
		decl->type = ofc_sema_type_type();
		if (!decl->type)
		{
			ofc_sema_array_delete(array);
			return false;
		}
		decl->type_implicit = false;
	}
	else if (type && !ofc_sema_decl_type_set(decl, type, name))
	{
		ofc_sema_array_delete(array);
		return false;
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

		array = (type_scan
			? ofc_sema_array_scan(scope, lhs->array.index)
			: ofc_sema_array(scope, lhs->array.index));
		if (!array) return false;

		lhs = lhs->parent;
		if (!lhs)
		{
			ofc_sema_array_delete(array);
			return false;
		}
	}

	if (lhs->type != OFC_PARSE_LHS_VARIABLE)
	{
		ofc_sema_array_delete(array);
		return false;
	}

	if (array)
	{
		if (!type_scan
			&& (structure || !ofc_sema_scope_is_procedure(scope))
			&& !ofc_sema_array_total(array, NULL))
		{
			ofc_sparse_ref_error(lhs->src,
				"Dynamically sized arrays are only valid in procedures");
			ofc_sema_array_delete(array);
			return false;
		}

		if (decl->array)
		{
			if (!ofc_sema_array_compare(
				decl->array, array))
			{
				ofc_sparse_ref_error(lhs->src,
					"Multiple incompatible definitions of array dimensions");
				ofc_sema_array_delete(array);
				return false;
			}
			if (!decl->array->scan)
			{
				ofc_sparse_ref_warning(lhs->src,
					"Redefinition of array dimensions");
			}
			ofc_sema_array_delete(decl->array);
		}

		decl->array = array;
	}

	if (!type_scan)
	{
		if (pdecl->init_expr)
		{
			if (!ofc_sema_decl_type_finalize(decl))
				return false;

			ofc_sema_expr_t* init_expr
				= ofc_sema_expr(scope, pdecl->init_expr);
			if (!init_expr) return false;

			bool initialized = ofc_sema_decl_init(
				decl, init_expr);
			ofc_sema_expr_delete(init_expr);
			if (!initialized) return false;
		}
		else if (pdecl->init_clist)
		{
			if (!ofc_sema_decl_type_finalize(decl))
				return false;

			ofc_sema_expr_list_t* init_clist
				= ofc_sema_expr_list_clist(
					scope, pdecl->init_clist);
			if (!init_clist) return false;

			bool initialized = ofc_sema_decl_init_array(
				decl, NULL, init_clist->count,
				(const ofc_sema_expr_t**)init_clist->expr);
			ofc_sema_expr_list_delete(init_clist);
			if (!initialized) return false;
		}
	}

	if (is_static)
		decl->is_static = true;
	if (is_parameter)
		decl->is_parameter = true;
	if (is_public)
		decl->access = OFC_SEMA_ACCESSIBILITY_PUBLIC;
	if (is_private)
		decl->access = OFC_SEMA_ACCESSIBILITY_PRIVATE;

	if (is_public && is_private)
	{
		ofc_sparse_ref_error(decl->name,
			"Declaration can't be both PUBLIC and PRIVATE");
		return false;
	}

	if ((is_public || is_private)
		&& (scope->type != OFC_SEMA_SCOPE_MODULE))
	{
		ofc_sparse_ref_error(decl->name,
			"%s declaration only allowed in MODULE",
			(is_public ? "PUBLIC" : "PRIVATE"));
		return false;
	}

	return true;
}

static bool ofc_sema__decl(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt,
	bool type_scan)
{
	if (!stmt || !scope
		|| (stmt->type != OFC_PARSE_STMT_DECL)
		|| !stmt->decl.type || !stmt->decl.decl)
		return false;

	ofc_sema_structure_t* type_struct = NULL;
	const ofc_sema_type_t* type = (type_scan
		? ofc_sema_type_scan(scope, stmt->decl.type, &type_struct)
		: ofc_sema_type(scope, stmt->decl.type, &type_struct));
	if (!type) return false;

	unsigned count = stmt->decl.decl->count;
	if (count == 0) return false;

	bool success = true;

	unsigned i;
	for (i = 0; i < count; i++)
	{
		if (!ofc_sema_decl__elem(
			scope, type,
			&type_struct, NULL,
			stmt->decl.dimension,
			stmt->decl.save,
			stmt->decl.parameter,
			stmt->decl.is_public,
			stmt->decl.is_private,
			type_scan,
			stmt->decl.decl->decl[i]))
			success = false;
	}

	return success;
}

bool ofc_sema_decl(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	return ofc_sema__decl(scope, stmt, false);
}

bool ofc_sema_decl_type_scan(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	return ofc_sema__decl(scope, stmt, true);
}

bool ofc_sema_decl_member(
	ofc_sema_scope_t* scope,
	ofc_sema_structure_t* structure,
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt || !scope
		|| (stmt->type != OFC_PARSE_STMT_DECL)
		|| !stmt->decl.type || !stmt->decl.decl
		|| stmt->decl.save)

		return false;

	ofc_sema_structure_t* type_struct = NULL;
	const ofc_sema_type_t* type = ofc_sema_type(
		scope, stmt->decl.type, &type_struct);
	if (!type) return false;

	unsigned count = stmt->decl.decl->count;
	if (count == 0) return false;

	bool success = true;

	unsigned i;
	for (i = 0; i < count; i++)
	{
		if (!ofc_sema_decl__elem(
			scope, type, &type_struct, structure,
			stmt->decl.dimension,
			false,
			stmt->decl.parameter,
			stmt->decl.is_public,
			stmt->decl.is_private,
			false,
			stmt->decl.decl->decl[i]))
			success = false;
	}

	return success;
}


ofc_sema_decl_t* ofc_sema_decl_copy(
	ofc_sema_decl_t* decl)
{
	if (!decl)
		return NULL;

	if (ofc_sema_decl_has_initializer(decl, NULL))
	{
		/* TODO - Support copying initializers. */
		return false;
	}

	if (decl->func != NULL)
	{
		/* TODO - Support copying decl's with scopes. */
		return false;
	}

	ofc_sema_decl_t* copy
		= ofc_sema_decl_create(NULL, decl->name);
	if (!copy) return NULL;

	memcpy(copy, decl, sizeof(ofc_sema_decl_t));
	copy->refcnt = 0;

	copy->array     = NULL;
	copy->structure = NULL;
	copy->func      = NULL;
	copy->common    = NULL;
	copy->intrinsic = NULL;

	if (decl->array)
	{
		copy->array = ofc_sema_array_copy(decl->array);
		if (!copy->array)
		{
			ofc_sema_decl_delete(copy);
			return NULL;
		}
	}

	if (decl->structure)
	{
		if (!ofc_sema_structure_reference(
			decl->structure))
		{
			ofc_sema_decl_delete(copy);
			return NULL;
		}
		copy->structure = decl->structure;
	}

	if (decl->is_intrinsic)
	{
		copy->is_intrinsic = true;
		copy->intrinsic = decl->intrinsic;
	}

	return copy;
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

static const ofc_str_ref_t* ofc_sema_decl_alias__key(
	const ofc_sema_decl_alias_t* alias)
{
	return (alias ? &(alias->name) : NULL);
}


ofc_sema_decl_alias_t* ofc_sema_decl_alias_create(
	ofc_str_ref_t name,
	ofc_sema_decl_t* decl)
{
	ofc_sema_decl_alias_t* alias
		= (ofc_sema_decl_alias_t*)malloc(
			sizeof(ofc_sema_decl_alias_t));
	if (!alias) return NULL;

	alias->name = name;
	alias->decl = decl;

	if (!ofc_sema_decl_reference(decl))
	{
		ofc_sema_decl_alias_delete(alias);
		return false;
	}

	return alias;
}

void ofc_sema_decl_alias_delete(
	ofc_sema_decl_alias_t* alias)
{
	if (!alias) return;

	ofc_sema_decl_delete(alias->decl);
	free(alias);
}


ofc_sema_decl_alias_map_t* ofc_sema_decl_alias_map_create(void)
{
	ofc_sema_decl_alias_map_t* map
		= (ofc_sema_decl_alias_map_t*)malloc(
			sizeof(ofc_sema_decl_alias_map_t));
	if (!map) return NULL;

	map->count = 0;
	map->list = NULL;

	map->map = ofc_hashmap_create(
		(void*)ofc_str_ref_ptr_hash_ci,
		(void*)ofc_str_ref_ptr_equal_ci,
		(void*)ofc_sema_decl_alias__key,
		(void*)ofc_sema_decl_alias_delete);

	if (!map->map)
	{
		ofc_sema_decl_alias_map_delete(map);
		return NULL;
	}

	return map;
}

bool ofc_sema_decl_alias_map_add(
	ofc_sema_decl_alias_map_t* map,
	ofc_sema_decl_alias_t* alias)
{
	if (!map || !alias)
		return false;

	ofc_sema_decl_alias_t** nlist
		= (ofc_sema_decl_alias_t**)realloc(map->list,
			(sizeof(ofc_sema_decl_alias_t*) * (map->count + 1)));
	if (!nlist) return false;
	map->list = nlist;

	if (!ofc_hashmap_add(
		map->map, alias))
		return false;

	map->list[map->count++] = alias;

	return true;
}

void ofc_sema_decl_alias_map_delete(
	ofc_sema_decl_alias_map_t* map)
{
	if (!map) return;

	ofc_hashmap_delete(map->map);
	if (map->list) free(map->list);
	free(map);
}

bool ofc_sema_decl_init(
	ofc_sema_decl_t* decl,
	const ofc_sema_expr_t* init)
{
	if (!decl || !init || !decl->type
		|| ofc_sema_decl_is_procedure(decl))
		return false;

	if (decl->was_written
		|| decl->was_read)
	{
		ofc_sparse_ref_error(init->src,
			"Can't initialize declaration after use");
		return false;
	}

	if (!ofc_sema_decl_type_finalize(decl))
		return false;

	if (init->type == OFC_SEMA_EXPR_ARRAY)
	{
		if (!init->array) return true;
		return ofc_sema_decl_init_array(
			decl, NULL, init->array->count,
			(const ofc_sema_expr_t**)init->array->expr);
	}
	else if (init->type == OFC_SEMA_EXPR_RESHAPE)
	{
		if (!init->reshape.source) return true;
		return ofc_sema_decl_init_array(
			decl, NULL, init->reshape.source->count,
			(const ofc_sema_expr_t**)init->reshape.source->expr);
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
		|| ofc_sema_decl_is_procedure(decl)
		|| !ofc_sema_decl_type_finalize(decl))
		return false;

	if (decl->was_written
		|| decl->was_read)
	{
		ofc_sparse_ref_error(init->src,
			"Can't initialize declaration after use");
		return false;
	}

	if (!ofc_sema_decl_type_finalize(decl))
		return false;

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
		|| ofc_sema_decl_is_procedure(decl)
		|| !ofc_sema_decl_type_finalize(decl))
		return false;

	if (count == 0)
		return true;

	if (decl->was_written
		|| decl->was_read)
	{
		ofc_sparse_ref_error(init[0]->src,
			"Can't initialize declaration after use");
		return false;
	}

	if (!ofc_sema_decl_type_finalize(decl))
		return false;

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


bool ofc_sema_decl_init_substring(
	ofc_sema_decl_t* decl,
	const ofc_sema_expr_t* init,
	const ofc_sema_expr_t* first,
	const ofc_sema_expr_t* last)
{
	if (!decl || !init || !last
		|| !ofc_sema_decl_type_finalize(decl))
		return false;

	if (decl->was_written
		|| decl->was_read)
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
		return ofc_sema_decl_init_substring_offset(
			decl, 0, init, first, last);
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

bool ofc_sema_decl_init_substring_offset(
	ofc_sema_decl_t* decl,
	unsigned offset,
	const ofc_sema_expr_t* init,
	const ofc_sema_expr_t* first,
	const ofc_sema_expr_t* last)
{
	if (!decl || !init || !last
		|| ofc_sema_decl_is_procedure(decl)
		|| !ofc_sema_decl_type_finalize(decl))
		return false;

	if (decl->was_written
		|| decl->was_read)
	{
		ofc_sparse_ref_error(init->src,
			"Can't initialize declaration after use");
		return false;
	}

	if (!ofc_sema_decl_type_finalize(decl))
		return false;

	if (!decl->type)
		return false;

	if (!ofc_sema_decl_is_composite(decl))
	{
		if (offset == 0)
			return ofc_sema_decl_init_substring(
				decl, init, first, last);
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

	if (!ofc_sema_expr_is_constant(init))
	{
		ofc_sparse_ref_error(init->src,
			"Array initializer element not constant.");
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
	else if (!decl->init_array[offset].is_substring
		&& (decl->init_array[offset].expr != NULL))
	{
		/* TODO - Check if substring initializer is the same as
		          existing initializer contents and just warn. */

		ofc_sparse_ref_error(init->src,
			"Destination already has complete initializer");
		return false;
	}

	const ofc_sema_type_t* type
		= ofc_sema_decl_type(decl);
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
		return ofc_sema_decl_init_offset(decl, offset, init);

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

	unsigned ss_offset = (ufirst - 1);
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

	if (!decl->init_array[offset].is_substring)
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

		decl->init_array[offset].is_substring     = true;
		decl->init_array[offset].substring.string = string;
		decl->init_array[offset].substring.mask   = mask;
	}

	unsigned tcsize = tsize;
	if (type->len > 0) tcsize /= type->len;

	bool overlap = false;
	unsigned i, j;
	for (i = ss_offset, j = 0; j < len; i++, j++)
	{
		if (decl->init_array[offset].substring.mask[i])
		{
			if (memcmp(&decl->init_array[offset].substring.string[i * tcsize],
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
			memcpy(&decl->init_array[offset].substring.string[i * tcsize],
				&ctv->character[j * tcsize], tcsize);
			decl->init_array[offset].substring.mask[i] = true;
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


bool ofc_sema_decl_is_external(
	const ofc_sema_decl_t* decl)
{
	return (decl && decl->is_external);
}

bool ofc_sema_decl_is_unknown_external(
	const ofc_sema_decl_t* decl)
{
	return (decl && decl->is_external && !decl->type);
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

bool ofc_sema_decl_is_stmt_func(
	const ofc_sema_decl_t* decl)
{
	if (!decl || !decl->func)
		return false;
	return (decl->func->type
		== OFC_SEMA_SCOPE_STMT_FUNC);
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
	return (decl->common != NULL);
}

bool ofc_sema_decl_is_intrinsic(
	const ofc_sema_decl_t* decl)
{
	return (decl && decl->is_intrinsic);
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

bool ofc_sema_decl_is_initialized(
	const ofc_sema_decl_t* decl, bool* complete)
{
	if (!decl)
		return false;

	bool c;
	if (ofc_sema_decl_has_initializer(decl, &c))
	{
		if (complete) *complete = c;
		return true;
	}

	if (!decl->structure)
		return false;

	unsigned count;
	if (!ofc_sema_decl_elem_count(
		decl, &count))
		return false;

	unsigned modulo;
	if (!ofc_sema_structure_elem_count(
		decl->structure, &modulo))
		return false;

	unsigned i, s;
	for (i = 0, s = 0; i < count; i++)
	{
		bool elem_complete = false;
		if (decl->init_array
			&& ofc_sema_decl_init__used(
				decl->init_array[i], decl->type,
				&elem_complete))
		{
			if (elem_complete)
				s++;
		}

		if (!elem_complete)
		{
			ofc_sema_decl_t* member
				= ofc_sema_structure_elem_get(
					decl->structure, (count % modulo));
			bool dc;
			if (ofc_sema_decl_is_initialized(
				member, &dc) && dc)
				s++;
		}
	}

	if (s == 0)
		return false;

	if (complete)
		*complete = (s == count);
	return true;
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
	list->size   = 0;
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
		for (i = 0; i < list->size; i++)
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

	unsigned slot;
	if (list->count >= list->size)
	{
		unsigned nsize = list->count + 1;
		ofc_sema_decl_t** ndecl
			= (ofc_sema_decl_t**)realloc(list->decl,
				(sizeof(ofc_sema_decl_t*) * nsize));
		if (!ndecl) return false;
		list->decl = ndecl;

		unsigned i;
		for (i = list->size; i < nsize; i++)
			list->decl[i] = NULL;

		slot = list->size;
		list->size = nsize;
	}
	else
	{
		unsigned i;
		for (i = 0; i < list->size; i++)
		{
			if (list->decl[i] == NULL)
			{
				slot = i;
				break;
			}
		}
		if (i == list->size)
			return false;
	}

	if (!ofc_hashmap_add(list->map, decl))
		return false;

	list->decl[slot] = decl;
	list->count++;
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

void ofc_sema_decl_list_remove(
	ofc_sema_decl_list_t* list, ofc_sema_decl_t* decl)
{
	if (!list || !decl)
		return;

	ofc_hashmap_remove(list->map, decl);

	unsigned i;
	for (i = 0; i < list->size; i++)
	{
		if (list->decl[i] && ofc_str_ref_equal(
			list->decl[i]->name.string,
			decl->name.string))
		{
			list->decl[i] = NULL;
			list->count--;
			break;
		}
	}

	ofc_sema_decl_delete(decl);
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
	if (!decl)
		return false;

	if (!ofc_colstr_newline(cs, indent, NULL))
		return false;

	if (decl->is_intrinsic)
	{
		return (ofc_colstr_keyword_atomic_writez(cs, "INTRINSIC")
			&& ofc_colstr_atomic_writef(cs, " ")
			&& ofc_sema_decl_print_name(cs, decl));
	}

	if (!decl->type)
		return false;

	const ofc_print_opts_t* opts =
		ofc_colstr_print_opts_get(cs);

	bool f77_parameter
		= (ofc_sema_decl_is_parameter(decl)
			&& opts && opts->f77_parameter);

	if (decl->is_automatic && opts && opts->automatic)
	{
		if (!ofc_colstr_keyword_atomic_writez(cs, "AUTOMATIC")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_sema_decl_print_name(cs, decl)
			|| !ofc_colstr_newline(cs, indent, NULL))
			return false;
	}

	const ofc_sema_type_t* type = NULL;
	bool is_pointer = false;
	if ((decl->type->type == OFC_SEMA_TYPE_TYPE)
		|| (decl->type->type == OFC_SEMA_TYPE_RECORD))
	{
		if (!decl->structure)
			return false;

		if (ofc_sema_structure_is_derived_type(decl->structure))
		{
			if (!ofc_colstr_keyword_atomic_writez(cs, "TYPE")
				|| !ofc_colstr_atomic_writef(cs, "(")
				|| !ofc_sema_structure_print_name(cs, decl->structure)
				|| !ofc_colstr_atomic_writef(cs, ")"))
				return false;
		}
		else
		{
			return (ofc_colstr_keyword_atomic_writez(cs, "RECORD")
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

	bool f90_style = false;
	if (decl->is_volatile)
	{
		if (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_keyword_atomic_writez(cs, "VOLATILE"))
			return false;
		f90_style = true;
	}

	if (decl->is_static)
	{
		if (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_keyword_atomic_writez(cs, "SAVE"))
			return false;
		f90_style = true;
	}

	if (!f77_parameter && ofc_sema_decl_is_parameter(decl))
	{
		if (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_keyword_atomic_writez(cs, "PARAMETER"))
			return false;
		f90_style = true;
	}

	if (is_pointer)
	{
		if (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_keyword_atomic_writez(cs, "POINTER"))
			return false;
		f90_style = true;
	}

	if (decl->access == OFC_SEMA_ACCESSIBILITY_PUBLIC)
	{
		if (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_keyword_atomic_writez(cs, "PUBLIC"))
			return false;
		f90_style = true;
	}

	if (decl->access == OFC_SEMA_ACCESSIBILITY_PRIVATE)
	{
		if (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_keyword_atomic_writez(cs, "PRIVATE"))
			return false;
		f90_style = true;
	}

	if (decl->is_target)
	{
		if (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_keyword_atomic_writez(cs, "TARGET"))
			return false;
		f90_style = true;
	}

	if (ofc_sema_decl_is_array(decl))
	{
		if (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_keyword_atomic_writez(cs, "DIMENSION")
			|| !ofc_sema_array_print_brackets(cs, decl->array))
			return false;
		f90_style = true;
	}

	if (!ofc_colstr_atomic_writef(cs, " "))
		return false;

	bool init_zero = (opts
		&& opts->init_zero && !decl->structure);

	if (init_zero)
	{
		if (decl->is_argument
			|| decl->is_return
			|| decl->is_equiv
			|| decl->is_external
			|| decl->is_parameter
			|| decl->is_stmt_func_arg
			|| decl->is_intrinsic
			|| decl->common)
			init_zero = false;

		if (ofc_sema_decl_is_array(decl)
			&& !ofc_sema_array_total(decl->array, NULL))
			init_zero = false;

		if (ofc_sema_decl_is_procedure(decl))
			init_zero = false;
	}

	bool init_complete = false;
	bool init_partial
		= ofc_sema_decl_has_initializer(
			decl, &init_complete);
	if (init_complete || init_zero)
		f90_style = true;

	if (f90_style)
	{
		if (!ofc_colstr_atomic_writef(cs, "::")
			|| !ofc_colstr_atomic_writef(cs, " "))
			return false;
	}

	if (!ofc_sema_decl_print_name(cs, decl))
		return false;

	/* TODO - Handle POINTER initializers. */

	if (f77_parameter)
	{
		if (!ofc_colstr_newline(cs, indent, NULL)
			|| !ofc_colstr_keyword_atomic_writez(cs, "PARAMETER")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_atomic_writef(cs, "(")
			|| !ofc_sema_decl_print_name(cs, decl))
			return false;
	}

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
				if (!ofc_colstr_keyword_atomic_writez(cs, "RESHAPE")
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
	else if (init_zero)
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
				if (!ofc_colstr_keyword_atomic_writez(cs, "RESHAPE")
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

				if (decl->init_array && ofc_sema_decl_init__used(
					decl->init_array[i], decl->type, NULL))
				{
					if (decl->init_array[i].is_substring)
					{
						char s[type->len];
						memset(s, ' ', type->len);

						unsigned i, len = 0;
						for (i = 0; i < type->len; i++)
						{
							if (decl->init_array[i].substring.mask[len])
							{
								s[i] = decl->init_array[i].substring.string[i];
								len = (i + 1);
							}
						}

						if (!ofc_colstr_write_escaped(cs, '\"', s, len))
							return false;
					}
					else if (!ofc_sema_expr_print(
						cs, decl->init_array[i].expr))
					{
						return false;
					}
				}
				else if (!ofc_sema_type_print_zero(cs, decl->type))
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
			if (init_partial && decl->init.is_substring)
			{
				char s[type->len];
				memset(s, ' ', type->len);

				unsigned i, len = 0;
				for (i = 0; i < type->len; i++)
				{
					if (decl->init.substring.mask[len])
					{
						s[i] = decl->init.substring.string[i];
						len = (i + 1);
					}
				}

				if (!ofc_colstr_write_escaped(cs, '\"', s, len))
					return false;
			}
			else if (!ofc_sema_type_print_zero(cs, decl->type))
				return false;
		}
	}

	if (f77_parameter
		&& !ofc_colstr_atomic_writef(cs, ")"))
		return false;

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

	if (complete)
		return true;

	const ofc_print_opts_t* opts
		= ofc_colstr_print_opts_get(cs);
	bool init_zero = (opts && opts->init_zero);

	if (init_zero)
	{
		if (decl->is_argument
			|| decl->is_return
			|| decl->is_equiv
			|| decl->is_external
			|| decl->is_parameter
			|| decl->is_stmt_func_arg
			|| decl->is_intrinsic
			|| decl->common)
			init_zero = false;

		if (ofc_sema_decl_is_array(decl)
			&& !ofc_sema_array_total(decl->array, NULL))
			init_zero = false;

		if (ofc_sema_decl_is_procedure(decl))
			init_zero = false;
	}

	/* If init_zero is set, this has already been initialized. */
	if (init_zero && !ofc_sema_decl_is_structure(decl))
		return true;

	if (!ofc_colstr_newline(cs, indent, NULL)
		|| !ofc_colstr_keyword_atomic_writez(cs, "DATA")
		|| !ofc_colstr_atomic_writef(cs, " "))
		return false;

	if (ofc_sema_decl_is_array(decl))
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
				&& !decl->init_array[i].expr
				&& !init_zero)
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
				&& !decl->init_array[i].expr
				&& !init_zero)
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
			else if (decl->init_array[i].expr)
			{
				if (!ofc_sema_expr_print(
					cs, decl->init_array[i].expr))
					return false;
			}
			else
			{
				if (!ofc_sema_type_print_zero(
					cs, decl->type))
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
				&& !decl->init_array[i].expr
				&& !init_zero)
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
				&& !decl->init_array[i].expr
				&& !init_zero)
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
			else if (decl->init_array[i].expr)
			{
				if (!ofc_sema_expr_print(
					cs, decl->init_array[i].expr))
					return false;
			}
			else
			{
				ofc_sema_decl_t* member
					= ofc_sema_structure_elem_get(
						decl->structure, i);
				if (!member) return false;

				if (!ofc_sema_decl_is_initialized(member, NULL))
				{
					if (!ofc_sema_type_print_zero(
						cs, member->type))
						return false;
				}
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
	for (i = 0; i < decl_list->size; i++)
	{
		ofc_sema_decl_t* decl = decl_list->decl[i];
		if (ofc_sema_decl_is_stmt_func(decl))
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
	for (i = 0; i < decl_list->size; i++)
	{
		ofc_sema_decl_t* decl = decl_list->decl[i];
		if (decl && decl->func)
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
	for (i = 0; i < decl_list->size; i++)
	{
		ofc_sema_decl_t* decl = decl_list->decl[i];

		if (!decl) continue;

		if (!ofc_sema_decl_is_procedure(decl)
			|| ofc_sema_decl_is_subroutine(decl)
			|| ofc_sema_decl_is_stmt_func(decl))
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

	/* Print PARAMETERs before other declarations. */
	for (i = 0; i < decl_list->size; i++)
	{
		ofc_sema_decl_t* decl = decl_list->decl[i];

		if (!decl) continue;

		if (!ofc_sema_decl_is_parameter(decl))
			continue;

		if (!ofc_sema_decl_print(cs, indent, decl))
			return false;
	}

	for (i = 0; i < decl_list->size; i++)
	{
		ofc_sema_decl_t* decl = decl_list->decl[i];

		if (!decl) continue;

		/* Skip PARAMETERs since we printed those first. */
		if (ofc_sema_decl_is_parameter(decl))
			continue;

		/* Don't print prototypes for declared procedures. */
		if (!ofc_sema_decl_is_stmt_func(decl)
			&& (ofc_sema_decl_is_procedure(decl) || decl->is_return))
			continue;

		if (!ofc_sema_decl_print(cs, indent, decl))
			return false;
	}

	for (i = 0; i < decl_list->size; i++)
	{
		ofc_sema_decl_t* decl = decl_list->decl[i];

		if (!decl) continue;

		if (ofc_sema_decl_is_procedure(decl)
			|| decl->is_return)
			continue;

		if (!ofc_sema_decl_print_data_init(cs, indent, decl))
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
	if (!list || !func)
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
