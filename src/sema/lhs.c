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
#include <math.h>


static ofc_sema_lhs_t* ofc_sema_lhs_index(
	ofc_sema_lhs_t* lhs,
	ofc_sema_array_index_t* index)
{
	if (!index)
		return NULL;

	if (!ofc_sema_lhs_reference(lhs))
		return NULL;

	ofc_sema_lhs_t* alhs
		= (ofc_sema_lhs_t*)malloc(
			sizeof(ofc_sema_lhs_t));
	if (!alhs)
	{
		ofc_sema_lhs_delete(lhs);
		return NULL;
	}

	alhs->type      = OFC_SEMA_LHS_ARRAY_INDEX;
	alhs->src       = lhs->src;
	alhs->parent    = lhs;
	alhs->index     = index;
	alhs->data_type = ofc_sema_type_base(lhs->data_type);
	alhs->refcnt    = 0;

	return alhs;
}

static ofc_sema_lhs_t* ofc_sema_lhs_slice(
	ofc_sema_lhs_t* lhs,
	ofc_sema_array_slice_t* slice)
{
	if (!slice)
		return NULL;

	if (!ofc_sema_lhs_reference(lhs))
		return NULL;

	ofc_sema_array_t* array
		= ofc_sema_array_slice_dims(slice);
	if (!array)
	{
		ofc_sema_lhs_delete(lhs);
		return NULL;
	}

	ofc_sema_lhs_t* alhs
		= (ofc_sema_lhs_t*)malloc(
			sizeof(ofc_sema_lhs_t));
	if (!alhs)
	{
		ofc_sema_lhs_delete(lhs);
		return NULL;
	}

	alhs->type        = OFC_SEMA_LHS_ARRAY_SLICE;
	alhs->src         = lhs->src;
	alhs->parent      = lhs;
	alhs->data_type   = lhs->data_type;
	alhs->refcnt      = 0;

	alhs->slice.slice = slice;
	alhs->slice.dims  = ofc_sema_array_slice_dims(slice);
	if (!alhs->slice.dims)
	{
		ofc_sema_lhs_delete(alhs);
		return NULL;
	}

	return alhs;
}

static ofc_sema_lhs_t* ofc_sema_lhs_substring(
	ofc_sema_scope_t* scope,
	ofc_sema_lhs_t* lhs,
	const ofc_parse_array_index_t* index)
{
	if (!ofc_sema_lhs_reference(lhs))
		return NULL;

	if (!index
		|| (index->count != 1)
		|| !lhs->data_type)
	{
		ofc_sema_lhs_delete(lhs);
		return NULL;
	}

	const ofc_parse_array_range_t* range
		= index->range[0];
	if (!range || range->stride)
	{
		ofc_sema_lhs_delete(lhs);
		return NULL;
	}

	ofc_sema_expr_t* first
		= ofc_sema_expr(scope, range->first);
	if (range->first && !first)
	{
		ofc_sema_lhs_delete(lhs);
		return NULL;
	}

	ofc_sema_expr_t* last = NULL;
	if (range->last)
	{
		last = ofc_sema_expr(scope, range->last);
		if (!last)
		{
			ofc_sema_lhs_delete(lhs);
			ofc_sema_expr_delete(first);
			return NULL;
		}
	}
	else if (!range->is_slice)
	{
		last = first;
	}

	unsigned len = 0;
	if ((!first || ofc_sema_expr_is_constant(first))
		&& ofc_sema_expr_is_constant(last))
	{
		const ofc_sema_typeval_t* first_ctv
			= ofc_sema_expr_constant(first);
		const ofc_sema_typeval_t* last_ctv
			= ofc_sema_expr_constant(last);

		if ((first && !first_ctv) || !last_ctv)
		{
			ofc_sema_lhs_delete(lhs);
			if (last != first)
				ofc_sema_expr_delete(last);
			ofc_sema_expr_delete(first);
			return NULL;
		}

		int64_t ifirst = 1;
		if (first && !ofc_sema_typeval_get_integer(
			first_ctv, &ifirst))
		{
			ofc_sema_lhs_delete(lhs);
			if (last != first)
				ofc_sema_expr_delete(last);
			ofc_sema_expr_delete(first);
			return NULL;
		}

		int64_t ilast;
		if (!ofc_sema_typeval_get_integer(
			last_ctv, &ilast))
		{
			ofc_sema_lhs_delete(lhs);
			if (last != first)
				ofc_sema_expr_delete(last);
			ofc_sema_expr_delete(first);
			return NULL;
		}

		if (ifirst < 0)
		{
			ofc_sparse_ref_error(lhs->src,
				"First index in character substring must be 1 or greater");

			ofc_sema_lhs_delete(lhs);
			if (last != first)
				ofc_sema_expr_delete(last);
			ofc_sema_expr_delete(first);
			return NULL;
		}

		if (ilast < ifirst)
		{
			ofc_sparse_ref_error(lhs->src,
				"Last index in character substring must be greater than first");

			ofc_sema_lhs_delete(lhs);
			if (last != first)
				ofc_sema_expr_delete(last);
			ofc_sema_expr_delete(first);
			return NULL;
		}

		if ((lhs->data_type->len > 0)
			&& (ilast > lhs->data_type->len))
		{
			ofc_sparse_ref_warning(lhs->src,
				"Last index in character substring out-of-bounds");
		}

		int64_t ilen = (ilast - ifirst) + 1;
		len = ilen;
		if ((int64_t)len != ilen)
		{
			ofc_sema_lhs_delete(lhs);
			if (last != first)
				ofc_sema_expr_delete(last);
			ofc_sema_expr_delete(first);
			return NULL;
		}
	}

	const ofc_sema_type_t* type
		= ofc_sema_type_create_character(
			lhs->data_type->kind, len);
	if (!type)
	{
		ofc_sema_lhs_delete(lhs);
		if (last != first)
			ofc_sema_expr_delete(last);
		ofc_sema_expr_delete(first);
		return NULL;
	}

	ofc_sema_lhs_t* alhs
		= (ofc_sema_lhs_t*)malloc(
			sizeof(ofc_sema_lhs_t));
	if (!alhs)
	{
		if (last != first)
			ofc_sema_expr_delete(last);
		ofc_sema_expr_delete(first);
		ofc_sema_lhs_delete(lhs);
		return NULL;
	}

	alhs->type            = OFC_SEMA_LHS_SUBSTRING;
	alhs->src             = lhs->src;
	alhs->parent          = lhs;
	alhs->substring.first = first;
	alhs->substring.last  = last;
	alhs->data_type       = type;
	alhs->refcnt          = 0;

	return alhs;
}


static ofc_sema_lhs_t* ofc_sema_lhs_member(
	ofc_sema_lhs_t* lhs,
	ofc_sparse_ref_t member)
{
	if (!ofc_sema_lhs_reference(lhs))
		return NULL;

	if (ofc_sparse_ref_empty(member))
	{
		ofc_sema_lhs_delete(lhs);
		return NULL;
	}

	/* TODO - Implement. */

	ofc_sema_lhs_delete(lhs);
	return NULL;
}


static ofc_sema_lhs_t* ofc_sema__lhs(
	ofc_sema_scope_t* scope,
	const ofc_parse_lhs_t* lhs,
	bool is_expr, bool force_local)
{
	if (!scope || !lhs)
		return NULL;

	switch (lhs->type)
	{
		case OFC_PARSE_LHS_IMPLICIT_DO:
			ofc_sparse_ref_error(lhs->src,
				"Can't resolve implicit do to single %s.",
				(is_expr ? "primary expression": "LHS"));
			return NULL;

		case OFC_PARSE_LHS_STAR_LEN:
			ofc_sparse_ref_error(lhs->src,
				"Can't resolve star length to %s.",
				(is_expr ? "primary expression": "LHS"));
			return NULL;

		case OFC_PARSE_LHS_MEMBER_TYPE:
		case OFC_PARSE_LHS_MEMBER_STRUCTURE:
			{
				ofc_sema_lhs_t* parent = ofc_sema__lhs(
					scope, lhs->parent, is_expr, force_local);
				if (!parent) return NULL;

				/* TODO - Check dereference type against structure type. */

				if (!parent->data_type
					|| (parent->data_type->type
						!= OFC_SEMA_TYPE_STRUCTURE))
				{
					ofc_sparse_ref_error(lhs->src,
						"Attempting to dereference member of a variable"
						" that's not a structure.");
					ofc_sema_lhs_delete(parent);
					return NULL;
				}

				ofc_sema_lhs_t* slhs
					= ofc_sema_lhs_member(
						parent, lhs->member.name);
				if (!slhs)
				{
					ofc_sema_lhs_delete(parent);
					return NULL;
				}

				return slhs;
			}

		case OFC_PARSE_LHS_ARRAY:
			{
				ofc_sema_lhs_t* parent = ofc_sema__lhs(
					scope, lhs->parent, is_expr, force_local);
				if (!parent) return NULL;

				if (!ofc_sema_lhs_is_array(parent))
				{
					if (!ofc_sema_type_is_character(
						parent->data_type))
					{
						ofc_sparse_ref_error(lhs->src,
							"Attempting to index a variable that's not an array");
						ofc_sema_lhs_delete(parent);
						return NULL;
					}

					ofc_sema_lhs_t* slhs
						= ofc_sema_lhs_substring(
							scope, parent, lhs->array.index);
					ofc_sema_lhs_delete(parent);
					return slhs;
				}

				ofc_sema_array_index_t* index
					= ofc_sema_array_index(scope,
						ofc_sema_lhs_array(parent),
						lhs->array.index);
				if (index)
				{
					ofc_sema_lhs_t* slhs
						= ofc_sema_lhs_index(parent, index);
					ofc_sema_lhs_delete(parent);
					if (!slhs)
					{
						ofc_sema_array_index_delete(index);
						return NULL;
					}
					return slhs;
				}

				/* TODO - Don't double-error when an index is out-of-bounds. */

				ofc_sema_array_slice_t* slice
					= ofc_sema_array_slice(scope,
						ofc_sema_lhs_array(parent),
						lhs->array.index);
				if (!slice)
				{
					ofc_sema_lhs_delete(parent);
					return NULL;
				}

				ofc_sema_lhs_t* slhs
					= ofc_sema_lhs_slice(parent, slice);
				ofc_sema_lhs_delete(parent);
				if (!slhs)
				{
					ofc_sema_array_slice_delete(slice);
					return NULL;
				}
				return slhs;
			}

		case OFC_PARSE_LHS_VARIABLE:
			break;

		default:
			return NULL;
	}

	ofc_sema_scope_t* root
		= ofc_sema_scope_root(scope);

	ofc_sema_decl_t* decl;
	if ((root->type == OFC_SEMA_SCOPE_FUNCTION)
		&& (ofc_sema_scope_get_lang_opts(root).case_sensitive
			? ofc_str_ref_equal(lhs->variable.string, root->name)
			: ofc_str_ref_equal_ci(lhs->variable.string, root->name)))
	{
		/* Special case for FUNCTION return value. */

		decl = ofc_sema_scope_decl_find_modify(
			scope, lhs->variable.string, true);
		if (!decl)
		{
			decl = ofc_sema_scope_decl_find_modify(
				root, lhs->variable.string, true);
		}

		if (!decl)
		{
			ofc_sema_decl_t* fdecl
				= ofc_sema_scope_decl_find_modify(
					root, lhs->variable.string, false);
			if (!fdecl)
			{
				/* This should never happen. */
				return NULL;
			}

			const ofc_sema_type_t* rtype
				= ofc_sema_decl_base_type(fdecl);
			decl = ofc_sema_decl_create(rtype, lhs->variable);
			if (!decl) return NULL;
			decl->is_return = true;

			if (!ofc_sema_scope_decl_add(
				root, decl))
			{
				ofc_sema_decl_delete(decl);
				return NULL;
			}
		}
	}
	else
	{
		decl = ofc_sema_scope_decl_find_modify(
			scope, lhs->variable.string, force_local);
		if (!decl)
		{
			decl = ofc_sema_decl_implicit_lhs(scope, lhs);
			if (!decl)
			{
				ofc_sparse_ref_error(lhs->src,
					"No declaration for '%.*s' and no valid IMPLICIT rule.",
					lhs->variable.string.size, lhs->variable.string.base);
				return NULL;
			}

			bool is_argument = false;
			if (scope->args != NULL)
			{
				/* TODO - Store args in hashmap. */
				unsigned i;
				for (i = 0; !is_argument && (i < scope->args->count); i++)
				{
					ofc_sema_arg_t arg = scope->args->arg[i];

					if (arg.alt_return)
						continue;

					ofc_lang_opts_t opts
						= ofc_sema_scope_get_lang_opts(scope);

					is_argument = (opts.case_sensitive
						? ofc_str_ref_equal(arg.name.string, lhs->variable.string)
						: ofc_str_ref_equal_ci(arg.name.string, lhs->variable.string));
				}
			}

			if (is_expr
				&& !ofc_sema_decl_is_procedure(decl)
				&& !is_argument)
			{
				ofc_sparse_ref_warning(lhs->src,
					"Referencing uninitialized variable '%.*s' in expression.",
					lhs->variable.string.size, lhs->variable.string.base);
			}
		}
	}

	if (!is_expr && ofc_sema_decl_is_parameter(decl))
	{
		/* TODO - Throw this error for PARAMETER arrays, etc. too. */
		ofc_sparse_ref_error(lhs->src,
			"Assignment to PARAMETER declaration");
		return NULL;
	}

	ofc_sema_lhs_t* slhs
		= (ofc_sema_lhs_t*)malloc(
			sizeof(ofc_sema_lhs_t));
	if (!slhs) return NULL;

	if (!ofc_sema_decl_reference(decl))
	{
		free(slhs);
		return NULL;
	}

	slhs->type      = OFC_SEMA_LHS_DECL;
	slhs->src       = lhs->src;
	slhs->decl      = decl;
	slhs->data_type = decl->type;
	slhs->refcnt    = 0;

	if (is_expr)
		decl->used = true;
	return slhs;
}

ofc_sema_lhs_t* ofc_sema_lhs(
	ofc_sema_scope_t* scope,
	const ofc_parse_lhs_t* lhs)
{
	return ofc_sema__lhs(
		scope, lhs, false, false);
}

ofc_sema_lhs_t* ofc_sema_lhs_from_expr(
	ofc_sema_scope_t* scope,
	ofc_parse_expr_t* expr)
{
	if (!scope || !expr) return NULL;

	if (expr->type != OFC_PARSE_EXPR_VARIABLE)
	{
		ofc_sparse_ref_error(expr->src,
			"Attempting to convert to lhs and expression that is not an lhs");
		return NULL;
	}
	return ofc_sema_lhs(
		scope, expr->variable);
}

ofc_sema_lhs_t* ofc_sema_lhs_in_expr(
	ofc_sema_scope_t* scope,
	const ofc_parse_lhs_t* lhs)
{
	return ofc_sema__lhs(
		scope, lhs, true, false);
}

ofc_sema_lhs_t* ofc_sema_lhs_local(
	ofc_sema_scope_t* scope,
	const ofc_parse_lhs_t* lhs)
{
	return ofc_sema__lhs(
		scope, lhs, false, true);
}


static ofc_sema_lhs_t* ofc_sema_lhs__implicit_do(
	ofc_sema_scope_t* scope,
	ofc_sparse_ref_t src,
	const ofc_parse_implicit_do_t* id)
{
	if (!id || !id->init)
		return NULL;

	ofc_sema_lhs_t* lhs
		= (ofc_sema_lhs_t*)malloc(
			sizeof(ofc_sema_lhs_t));
	if (!lhs) return NULL;

	lhs->type = OFC_SEMA_LHS_IMPLICIT_DO;
	lhs->src  = src;

	lhs->implicit_do.lhs  = NULL;
	lhs->implicit_do.iter = NULL;
	lhs->implicit_do.init = NULL;
	lhs->implicit_do.last = NULL;
	lhs->implicit_do.step = NULL;

	lhs->implicit_do.count_var = true;
	lhs->implicit_do.count = 0;

	lhs->data_type = NULL;
	lhs->refcnt = 0;

	ofc_sema_lhs_t* iter_lhs = ofc_sema_lhs(
		scope, id->init->name);
	if (!iter_lhs)
	{
		ofc_sema_lhs_delete(lhs);
		return NULL;
	}

	if (iter_lhs->type != OFC_SEMA_LHS_DECL)
	{
		ofc_sparse_ref_error(id->init->name->src,
			"Implicit do loop iterator must be a variable");
		ofc_sema_lhs_delete(iter_lhs);
		ofc_sema_lhs_delete(lhs);
		return NULL;
	}

	if (!ofc_sema_decl_reference(iter_lhs->decl))
	{
		ofc_sema_lhs_delete(iter_lhs);
		ofc_sema_lhs_delete(lhs);
		return NULL;
	}

	lhs->implicit_do.iter = iter_lhs->decl;
	ofc_sema_lhs_delete(iter_lhs);

	const ofc_sema_type_t* iter_type
		= ofc_sema_decl_type(lhs->implicit_do.iter);
	if (!iter_type)
	{
		ofc_sema_lhs_delete(lhs);
		return NULL;
	}

	if (!ofc_sema_type_is_scalar(iter_type))
	{
		ofc_sparse_ref_error(id->init->name->src,
			"Implicit do loop iterator must be a scalar type");
		ofc_sema_lhs_delete(lhs);
		return NULL;
	}

	if (!ofc_sema_type_is_integer(iter_type))
	{
		ofc_sparse_ref_warning(id->init->name->src,
			"Using REAL in implicit do loop iterator");
	}

	lhs->implicit_do.init = ofc_sema_expr(
		scope, id->init->init);
	if (!lhs->implicit_do.init)
	{
		ofc_sema_lhs_delete(lhs);
		return NULL;
	}

	const ofc_sema_type_t* init_type
		= ofc_sema_expr_type(lhs->implicit_do.init);
	if (!init_type)
	{
		ofc_sema_lhs_delete(lhs);
		return NULL;
	}
	else if (!ofc_sema_type_compatible(
		iter_type, init_type))
	{
		ofc_sema_expr_t* cast
			= ofc_sema_expr_cast(
				lhs->implicit_do.init, iter_type);
		if (!cast)
		{
			ofc_sema_lhs_delete(lhs);
			return NULL;
		}

		lhs->implicit_do.init = cast;
	}

	lhs->implicit_do.last = ofc_sema_expr(
		scope, id->limit);
	if (!lhs->implicit_do.last)
	{
		ofc_sema_lhs_delete(lhs);
		return NULL;
	}

	const ofc_sema_type_t* last_type
		= ofc_sema_expr_type(lhs->implicit_do.last);
	if (!last_type)
	{
		ofc_sema_lhs_delete(lhs);
		return NULL;
	}
	else if (!ofc_sema_type_compatible(
		iter_type, last_type))
	{
		ofc_sema_expr_t* cast
			= ofc_sema_expr_cast(
				lhs->implicit_do.last, iter_type);
		if (!cast)
		{
			ofc_sparse_ref_error(id->limit->src,
				"Expression type '%s' doesn't match iterator type '%s'",
				ofc_sema_type_str_rep(last_type),
				ofc_sema_type_str_rep(iter_type));
			ofc_sema_lhs_delete(lhs);
			return NULL;
		}

		lhs->implicit_do.last = cast;
	}

	if (id->step)
	{
		lhs->implicit_do.step
			= ofc_sema_expr(scope, id->step);
		if (!lhs->implicit_do.step)
		{
			ofc_sema_lhs_delete(lhs);
			return NULL;
		}

		const ofc_sema_type_t* step_type
			= ofc_sema_expr_type(lhs->implicit_do.step);
		if (!step_type)
		{
			ofc_sema_lhs_delete(lhs);
			return NULL;
		}
		else if (!ofc_sema_type_compatible(
			iter_type, step_type))
		{
			ofc_sema_expr_t* cast
				= ofc_sema_expr_cast(
					lhs->implicit_do.step, iter_type);
			if (!cast)
			{
				ofc_sparse_ref_error(id->step->src,
					"Expression type '%s' doesn't match iterator type '%s'",
					ofc_sema_type_str_rep(step_type),
					ofc_sema_type_str_rep(iter_type));
				ofc_sema_lhs_delete(lhs);
				return NULL;
			}

			lhs->implicit_do.step = cast;
		}
	}

	if (id->dlist && (id->dlist->type == OFC_PARSE_LHS_IMPLICIT_DO))
	{
		lhs->implicit_do.lhs
			= ofc_sema_lhs__implicit_do(
				scope, id->dlist->src, id->dlist->implicit_do);
	}
	else
	{
		lhs->implicit_do.lhs = ofc_sema_lhs(
			scope, id->dlist);
	}

	lhs->data_type = ofc_sema_lhs_type(
		lhs->implicit_do.lhs);
	if (!lhs->data_type)
	{
		ofc_sema_lhs_delete(lhs);
		return NULL;
	}

	const ofc_sema_typeval_t* ctv[3];
	ctv[0] = ofc_sema_expr_constant(
			lhs->implicit_do.init);
	ctv[1] = ofc_sema_expr_constant(
			lhs->implicit_do.last);
	ctv[2] = ofc_sema_expr_constant(
			lhs->implicit_do.step);

	long double first, last, step = 1.0;
	if (ofc_sema_typeval_get_real(ctv[0], &first)
		&& ofc_sema_typeval_get_real(ctv[1], &last)
		&& (!ctv[2] || ofc_sema_typeval_get_real(ctv[2], &step)))
	{
		long double dcount = floor((last - first) / step);
		if (dcount < 0.0)
		{
			ofc_sparse_ref_error(src,
				"Loop iterates away from limit");
			ofc_sema_lhs_delete(lhs);
			return NULL;
		}
		dcount += 1.0;

		lhs->implicit_do.count = (unsigned)dcount;
		if ((long double)lhs->implicit_do.count != dcount)
		{
			ofc_sema_lhs_delete(lhs);
			return NULL;
		}
		lhs->implicit_do.count_var = false;
	}
	else
	{
		lhs->implicit_do.count_var = true;
		lhs->implicit_do.count     = 0;
	}

	return lhs;
}


ofc_sema_lhs_t* ofc_sema_lhs_elem_get(
	ofc_sema_lhs_t* lhs, unsigned offset)
{
	if (!lhs)
		return NULL;

	switch (lhs->type)
	{
		case OFC_SEMA_LHS_DECL:
		case OFC_SEMA_LHS_ARRAY_INDEX:
		case OFC_SEMA_LHS_STRUCTURE_MEMBER:
			if (ofc_sema_type_is_procedure(lhs->data_type))
				return NULL;

			if (ofc_sema_lhs_is_array(lhs))
			{
				const ofc_sema_type_t* base_type
					= lhs->data_type;

				unsigned base_count;
				if (!ofc_sema_type_elem_count(
					base_type, &base_count))
					return NULL;

				if (base_count == 0)
					return NULL;

				unsigned base_offset = (offset % base_count);

				ofc_sema_array_index_t* index
					= ofc_sema_array_index_from_offset(
						lhs->decl, (offset / base_count));
				if (!index) return NULL;

				ofc_sema_lhs_t* nlhs = ofc_sema_lhs_index(lhs, index);
				if (!nlhs)
				{
					ofc_sema_array_index_delete(index);
					return NULL;
				}

				ofc_sema_lhs_t* rlhs
					= ofc_sema_lhs_elem_get(
						nlhs, base_offset);
				ofc_sema_lhs_delete(nlhs);
				return rlhs;
			}

			if (!ofc_sema_type_is_composite(lhs->data_type))
			{
				if (offset != 0)
					return NULL;
				if (!ofc_sema_lhs_reference(lhs))
					return NULL;
				return lhs;
			}

			/* TODO - Handle structures. */
			break;

		case OFC_SEMA_LHS_ARRAY_SLICE:
			/* TODO - Convert offset to index and treat as index. */
			break;

		case OFC_SEMA_LHS_SUBSTRING:
			if (offset != 0)
				return NULL;
			if (!ofc_sema_lhs_reference(lhs))
					return NULL;
			return lhs;

		case OFC_SEMA_LHS_IMPLICIT_DO:
		{
			if (!lhs->implicit_do.iter)
				return NULL;

			unsigned sub_elem_count;
			if (!ofc_sema_lhs_elem_count(
				lhs->implicit_do.lhs, &sub_elem_count))
				return NULL;

			if (sub_elem_count == 0)
				return NULL;

			unsigned sub_offset
				= (offset % sub_elem_count);
			offset /= sub_elem_count;

			const ofc_sema_typeval_t* ctv[2];
			ctv[0] = ofc_sema_expr_constant(
					lhs->implicit_do.init);
			ctv[1] = ofc_sema_expr_constant(
					lhs->implicit_do.step);

			long double first, step = 1.0;
			if (!ofc_sema_typeval_get_real(ctv[0], &first))
				return NULL;
			if (ctv[1] && !ofc_sema_typeval_get_real(ctv[1], &step))
				return NULL;

			long double doffset
				= first + ((long double)offset * step);

			ofc_sema_typeval_t* dinit
				= ofc_sema_typeval_create_real(
					doffset, OFC_SPARSE_REF_EMPTY);
			if (!dinit) return NULL;

			ofc_sema_typeval_t* init
				= ofc_sema_typeval_cast(
					dinit, lhs->implicit_do.iter->type);
			ofc_sema_typeval_delete(dinit);
			if (!init) return NULL;

			ofc_sema_expr_t* iter_expr
				= ofc_sema_expr_typeval(init);
			if (!iter_expr)
			{
				ofc_sema_typeval_delete(init);
				return NULL;
			}

			ofc_sema_lhs_t* body
				= ofc_sema_lhs_copy_replace(
					lhs->implicit_do.lhs,
					lhs->implicit_do.iter, iter_expr);
			ofc_sema_expr_delete(iter_expr);
			if (!body) return NULL;

			ofc_sema_lhs_t* rval
				= ofc_sema_lhs_elem_get(
					body, sub_offset);
			ofc_sema_lhs_delete(body);
			return rval;
		}

		default:
			break;
	}

	return NULL;
}



ofc_sema_lhs_t* ofc_sema_lhs_copy_replace(
	const ofc_sema_lhs_t*  lhs,
	const ofc_sema_decl_t* replace,
	const ofc_sema_expr_t* with)
{
	if (!lhs)
		return NULL;

	ofc_sema_lhs_t* copy
		= (ofc_sema_lhs_t*)malloc(
			sizeof(ofc_sema_lhs_t));
	if (!copy) return NULL;

	*copy = *lhs;

	if (lhs->type == OFC_SEMA_LHS_DECL)
	{
		if (!ofc_sema_decl_reference(lhs->decl))
		{
			free(copy);
			return NULL;
		}
	}
	else if (lhs->type == OFC_SEMA_LHS_IMPLICIT_DO)
	{
		copy->implicit_do.lhs
			= ofc_sema_lhs_copy_replace(
				lhs->implicit_do.lhs, replace, with);
		copy->implicit_do.iter
			= (ofc_sema_decl_reference(lhs->implicit_do.iter)
				? lhs->implicit_do.iter : NULL);
		copy->implicit_do.init
			= ofc_sema_expr_copy_replace(
				lhs->implicit_do.init, replace, with);
		copy->implicit_do.last
			= ofc_sema_expr_copy_replace(
				lhs->implicit_do.last, replace, with);
		copy->implicit_do.step
			= ofc_sema_expr_copy_replace(
				lhs->implicit_do.step, replace, with);
		if (!copy->implicit_do.lhs
			|| !copy->implicit_do.iter
			|| !copy->implicit_do.init
			|| !copy->implicit_do.last
			|| (lhs->implicit_do.step
				&& !copy->implicit_do.step))
		{
			ofc_sema_lhs_delete(copy);
			return NULL;
		}
	}
	else
	{
		switch (lhs->type)
		{
			case OFC_SEMA_LHS_ARRAY_INDEX:
				copy->index = ofc_sema_array_index_copy_replace(
					lhs->index, replace, with);
				if (!copy->index)
				{
					free(copy);
					return NULL;
				}
				break;

			case OFC_SEMA_LHS_ARRAY_SLICE:
				copy->slice.slice
					= ofc_sema_array_slice_copy_replace(
						lhs->slice.slice, replace, with);
				copy->slice.dims
					= ofc_sema_array_slice_dims(lhs->slice.slice);
				if (!copy->slice.slice
					|| !copy->slice.dims)
				{
					ofc_sema_array_slice_delete(copy->slice.slice);
					ofc_sema_array_delete(copy->slice.dims);
					free(copy);
					return NULL;
				}
				break;

			case OFC_SEMA_LHS_SUBSTRING:
				if (lhs->substring.first)
				{
					copy->substring.first
						= ofc_sema_expr_copy_replace(
							lhs->substring.first, replace, with);
					if (!copy->substring.first)
					{
						free(copy);
						return NULL;
					}
				}

				if (lhs->substring.last)
				{
					copy->substring.last
						= ofc_sema_expr_copy_replace(
							lhs->substring.last, replace, with);
					if (!copy->substring.last)
					{
						ofc_sema_expr_delete(
							copy->substring.first);
						free(copy);
						return NULL;
					}
				}
				break;

			case OFC_SEMA_LHS_STRUCTURE_MEMBER:
				break;

			default:
				free(copy);
				return NULL;
		}

		copy->parent = ofc_sema_lhs_copy_replace(
			lhs->parent, replace, with);
		if (!copy->parent)
		{
			ofc_sema_lhs_delete(copy);
			return NULL;
		}
	}

	return copy;
}

ofc_sema_lhs_t* ofc_sema_lhs_copy(
	const ofc_sema_lhs_t* lhs)
{
	return ofc_sema_lhs_copy_replace(
		lhs, NULL, NULL);
}

bool ofc_sema_lhs_reference(
	ofc_sema_lhs_t* lhs)
{
	if (!lhs)
		return false;

	if ((lhs->refcnt + 1) == 0)
		return false;

	lhs->refcnt++;
	return true;
}

void ofc_sema_lhs_delete(
	ofc_sema_lhs_t* lhs)
{
	if (!lhs)
		return;

	if (lhs->refcnt > 0)
	{
		lhs->refcnt--;
		return;
	}

	switch (lhs->type)
	{
		case OFC_SEMA_LHS_DECL:
			ofc_sema_decl_delete(lhs->decl);
			break;

		case OFC_SEMA_LHS_ARRAY_INDEX:
		case OFC_SEMA_LHS_ARRAY_SLICE:
		case OFC_SEMA_LHS_SUBSTRING:
		case OFC_SEMA_LHS_STRUCTURE_MEMBER:
			ofc_sema_lhs_delete(lhs->parent);
			break;

		default:
			break;
	}

	switch (lhs->type)
	{
		case OFC_SEMA_LHS_ARRAY_INDEX:
			ofc_sema_array_index_delete(lhs->index);
			break;

		case OFC_SEMA_LHS_ARRAY_SLICE:
			ofc_sema_array_slice_delete(lhs->slice.slice);
			ofc_sema_array_delete(lhs->slice.dims);
			break;

		case OFC_SEMA_LHS_SUBSTRING:
			if (lhs->substring.last != lhs->substring.first)
				ofc_sema_expr_delete(lhs->substring.last);
			ofc_sema_expr_delete(lhs->substring.first);
			break;

		case OFC_SEMA_LHS_IMPLICIT_DO:
			ofc_sema_lhs_delete(lhs->implicit_do.lhs);
			ofc_sema_decl_delete(lhs->implicit_do.iter);
			ofc_sema_expr_delete(lhs->implicit_do.init);
			ofc_sema_expr_delete(lhs->implicit_do.last);
			ofc_sema_expr_delete(lhs->implicit_do.step);
			break;

		default:
			break;
	}

	free(lhs);
}


bool ofc_sema_lhs_init(
	ofc_sema_lhs_t* lhs,
	const ofc_sema_expr_t* init)
{
	if (!lhs || !init)
		return false;

	ofc_sema_decl_t* decl
		= ofc_sema_lhs_decl(lhs);

	switch (lhs->type)
	{
		case OFC_SEMA_LHS_DECL:
			return ofc_sema_decl_init(
				decl, init);

		case OFC_SEMA_LHS_ARRAY_INDEX:
		{
			unsigned offset;
			if (!ofc_sema_array_index_offset(
				decl, lhs->index, &offset))
				return false;

			return ofc_sema_decl_init_offset(
				decl, offset, init);
		}

		case OFC_SEMA_LHS_SUBSTRING:
			/* TODO - Handle nested substrings. */
			return ofc_sema_decl_init_substring(
				decl, init,
				lhs->substring.first,
				lhs->substring.last);

		/* TODO - Initialize all LHS types. */

		default:
			break;
	}

	return false;
}

bool ofc_sema_lhs_init_array(
	ofc_sema_lhs_t* lhs,
	const ofc_sema_array_t* array,
	unsigned count,
	const ofc_sema_expr_t** init)
{
	if (!lhs || !init)
		return false;

	if (count == 0)
		return true;

	/* TODO - Support initializing array slices. */
	if (lhs->type == OFC_SEMA_LHS_ARRAY_SLICE)
		return false;

	if (lhs->type != OFC_SEMA_LHS_DECL)
		return false;

	return ofc_sema_decl_init_array(
		ofc_sema_lhs_decl(lhs),
		array, count, init);
}


bool ofc_sema_lhs_is_array(
	const ofc_sema_lhs_t* lhs)
{
	if (!lhs)
		return false;

	switch (lhs->type)
	{
		case OFC_SEMA_LHS_DECL:
			return ofc_sema_decl_is_array(lhs->decl);

		case OFC_SEMA_LHS_ARRAY_SLICE:
			return true;

		case OFC_SEMA_LHS_ARRAY_INDEX:
		case OFC_SEMA_LHS_SUBSTRING:
			return false;

		case OFC_SEMA_LHS_STRUCTURE_MEMBER:
		default:
			break;
	}

	return false;
}

bool ofc_sema_lhs_is_parameter(
	const ofc_sema_lhs_t* lhs)
{
	if (!lhs)
		return false;

	switch (lhs->type)
	{
		case OFC_SEMA_LHS_DECL:
			return ofc_sema_decl_is_parameter(lhs->decl);

		/* TODO - Calculate properly for indices, slices, substrings, etc. */

		default:
			break;
	}

	return false;
}


const ofc_sema_array_t* ofc_sema_lhs_array(
	const ofc_sema_lhs_t* lhs)
{
	if (!lhs)
		return NULL;

	switch (lhs->type)
	{
		case OFC_SEMA_LHS_DECL:
			if (!lhs->decl) return NULL;
			return lhs->decl->array;

		case OFC_SEMA_LHS_ARRAY_SLICE:
			return lhs->slice.dims;

		default:
			break;
	}

	return NULL;
}

ofc_sema_typeval_t* ofc_sema_lhs_parameter(
	const ofc_sema_lhs_t* lhs)
{
	if (!lhs)
		return NULL;

	if (lhs->type != OFC_SEMA_LHS_DECL)
	{
		/* TODO - Handle more complex cases like a constant index
		          into a PARAMETER array. */
		return NULL;
	}

	const ofc_sema_decl_t* decl = lhs->decl;

	bool complete;
	if (!decl || !decl->is_parameter
		|| !ofc_sema_decl_has_initializer(decl, &complete)
		|| !complete)
		return NULL;

	/* TODO - Handle PARAMETER arrays properly. */
	if (ofc_sema_decl_is_composite(decl))
		return NULL;

	if (decl->init.is_substring)
	{
		/* TODO - Handle incomplete substring PARAMETERS. */
		return NULL;
	}

	return ofc_sema_typeval_copy(
		ofc_sema_expr_constant(
			decl->init.expr));
}


bool ofc_sema_lhs_mark_used(
	ofc_sema_lhs_t* lhs)
{
	if (!lhs)
		return false;

	if (lhs->type == OFC_SEMA_LHS_DECL)
	{
		if (!lhs->decl)
			return false;
		lhs->decl->used = true;
		return true;
	}

	return ofc_sema_lhs_mark_used(
		lhs->parent);
}


bool ofc_sema_lhs_compare(
	const ofc_sema_lhs_t* a,
	const ofc_sema_lhs_t* b)
{
	if (!a || !b)
		return false;

	if (a == b)
		return true;

	if (a->type != b->type)
		return false;

	if (a->type == OFC_SEMA_LHS_DECL)
		return (a->decl == b->decl);

	if (!ofc_sema_lhs_compare(
		a->parent, b->parent))
		return false;

	switch (a->type)
	{
		case OFC_SEMA_LHS_ARRAY_INDEX:
			if (!ofc_sema_array_index_compare(
				a->index, b->index))
				return false;
			break;

		case OFC_SEMA_LHS_ARRAY_SLICE:
			if (!ofc_sema_array_slice_compare(
				a->slice.slice, b->slice.slice))
				return false;
			break;

		default:
			return false;
	}

	return true;
}


ofc_sema_decl_t* ofc_sema_lhs_decl(
	ofc_sema_lhs_t* lhs)
{
	if (!lhs)
		return NULL;

	switch (lhs->type)
	{
		case OFC_SEMA_LHS_DECL:
			return lhs->decl;

		case OFC_SEMA_LHS_ARRAY_INDEX:
		case OFC_SEMA_LHS_ARRAY_SLICE:
		case OFC_SEMA_LHS_STRUCTURE_MEMBER:
		case OFC_SEMA_LHS_SUBSTRING:
			return ofc_sema_lhs_decl(lhs->parent);

		default:
			break;
	}

	return NULL;
}

const ofc_sema_type_t* ofc_sema_lhs_type(
	const ofc_sema_lhs_t* lhs)
{
	return (lhs ? lhs->data_type : NULL);
}

bool ofc_sema_lhs_elem_count(
	const ofc_sema_lhs_t* lhs,
	unsigned* count)
{
	if (!lhs) return false;

	unsigned ecount;
	if (!ofc_sema_type_elem_count(
		lhs->data_type, &ecount))
		return false;

	if (lhs->type == OFC_SEMA_LHS_IMPLICIT_DO)
	{
		if (lhs->implicit_do.count_var)
			return false;

		if (lhs->implicit_do.lhs
			&& (lhs->implicit_do.lhs->type
				== OFC_SEMA_LHS_IMPLICIT_DO))
		{
			unsigned idecount;
			if (!ofc_sema_lhs_elem_count(
				lhs->implicit_do.lhs, &idecount))
				return false;
			ecount *= idecount;
		}

		ecount *= lhs->implicit_do.count;
	}

	const ofc_sema_array_t* array
		= ofc_sema_lhs_array(lhs);
	if (array)
	{
		unsigned acount;
		if (!ofc_sema_array_total(
			array, &acount))
			return false;
		ecount *= acount;
	}

	if (count) *count = ecount;
	return true;
}

bool ofc_sema_lhs_print(
	ofc_colstr_t* cs,
	const ofc_sema_lhs_t* lhs)
{
	if (!cs || !lhs) return false;

	switch(lhs->type)
	{
		case OFC_SEMA_LHS_ARRAY_INDEX:
		case OFC_SEMA_LHS_ARRAY_SLICE:
		case OFC_SEMA_LHS_STRUCTURE_MEMBER:
		case OFC_SEMA_LHS_SUBSTRING:
			if (!ofc_sema_lhs_print(
				cs, lhs->parent))
				return false;
			break;
		default:
			break;
	}

	switch(lhs->type)
	{
		case OFC_SEMA_LHS_DECL:
			if (!ofc_sema_decl_print_name(cs, lhs->decl))
				return false;
			break;

		case OFC_SEMA_LHS_ARRAY_INDEX:
			if (!ofc_sema_array_index_print(cs, lhs->index))
				return false;
			break;

		case OFC_SEMA_LHS_ARRAY_SLICE:
			if (!ofc_sema_array_slice_print(cs, lhs->slice.slice))
				return false;
			break;

		case OFC_SEMA_LHS_SUBSTRING:
			if (!ofc_colstr_atomic_writef(cs, "("))
				return false;
			if (lhs->substring.first
				&& !ofc_sema_expr_print(cs,
					lhs->substring.first))
				return false;

			if (!ofc_colstr_atomic_writef(cs, ":"))
				return false;

			if (lhs->substring.last
				&& !ofc_sema_expr_print(cs,
					lhs->substring.last))
				return false;

			if (!ofc_colstr_atomic_writef(cs, ")"))
				return false;
			break;

		case OFC_SEMA_LHS_STRUCTURE_MEMBER:
			if (!ofc_colstr_atomic_writef(cs, ".%.*s",
				lhs->member.size, lhs->member.base))
					return false;
			break;

		case OFC_SEMA_LHS_IMPLICIT_DO:
			if (!ofc_colstr_atomic_writef(cs, "(")
				|| !ofc_sema_lhs_print(cs, lhs->implicit_do.lhs)
				|| !ofc_colstr_atomic_writef(cs, ",")
				|| !ofc_colstr_atomic_writef(cs, " ")
				|| !ofc_sema_decl_print_name(cs, lhs->implicit_do.iter)
				|| !ofc_colstr_atomic_writef(cs, " ")
				|| !ofc_colstr_atomic_writef(cs, "=")
				|| !ofc_colstr_atomic_writef(cs, " ")
				|| !ofc_sema_expr_print(cs, lhs->implicit_do.init)
				|| !ofc_colstr_atomic_writef(cs, ",")
				|| !ofc_colstr_atomic_writef(cs, " ")
				|| !ofc_sema_expr_print(cs, lhs->implicit_do.last))
				return false;

			if (lhs->implicit_do.step)
			{
				if (!ofc_colstr_atomic_writef(cs, ",")
					|| !ofc_colstr_atomic_writef(cs, " ")
					|| !ofc_sema_expr_print(cs, lhs->implicit_do.step))
					return false;
			}

			if (!ofc_colstr_atomic_writef(cs, ")"))
				return false;
			break;

		default:
			return false;
	}

	return true;
}


static ofc_sema_lhs_list_t* ofc_sema_lhs__list(
	ofc_sema_scope_t* scope,
	const ofc_parse_lhs_list_t* plist,
	bool allow_implicit_do)
{
	if (!plist)
		return NULL;

	ofc_sema_lhs_list_t* list
		= (ofc_sema_lhs_list_t*)malloc(
			sizeof(ofc_sema_lhs_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->lhs = (ofc_sema_lhs_t**)malloc(
		plist->count * sizeof(ofc_sema_lhs_t*));
	if (!list->lhs)
	{
		free(list);
		return NULL;
	}

	unsigned i;
	for (i = 0; i < plist->count; i++)
	{
		if (plist->lhs[i]->type == OFC_PARSE_LHS_IMPLICIT_DO)
		{
			if (!allow_implicit_do)
			{
				ofc_sema_lhs_list_delete(list);
				return NULL;
			}

			list->lhs[i] = ofc_sema_lhs__implicit_do(
				scope, plist->lhs[i]->src,
				plist->lhs[i]->implicit_do);
		}
		else
		{
			list->lhs[i] = ofc_sema_lhs(
				scope, plist->lhs[i]);
		}

		if (!list->lhs[i])
		{
			ofc_sema_lhs_list_delete(list);
			return NULL;
		}

		list->count++;
	}

	return list;
}

ofc_sema_lhs_list_t* ofc_sema_lhs_list(
	ofc_sema_scope_t* scope,
	const ofc_parse_lhs_list_t* plist)
{
	return ofc_sema_lhs__list(
		scope, plist, false);
}

ofc_sema_lhs_list_t* ofc_sema_lhs_list_id(
	ofc_sema_scope_t* scope,
	const ofc_parse_lhs_list_t* plist)
{
	return ofc_sema_lhs__list(
		scope, plist, true);
}


ofc_sema_lhs_list_t* ofc_sema_lhs_list_create(void)
{
	ofc_sema_lhs_list_t* list
		= (ofc_sema_lhs_list_t*)malloc(
			sizeof(ofc_sema_lhs_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->lhs   = NULL;
	return list;
}

void ofc_sema_lhs_list_delete(ofc_sema_lhs_list_t* list)
{
	if (!list)
		return;

	unsigned i;
	for (i = 0; i < list->count; i++)
		ofc_sema_lhs_delete(list->lhs[i]);
	free(list->lhs);

	free(list);
}

bool ofc_sema_lhs_list_add(
	ofc_sema_lhs_list_t* list,
	ofc_sema_lhs_t* lhs)
{
	if (!list || !lhs)
		return false;

	ofc_sema_lhs_t** nlhs
		= (ofc_sema_lhs_t**)realloc(list->lhs,
			((list->count + 1) * sizeof(ofc_sema_lhs_t*)));
	if (!nlhs) return false;
	list->lhs = nlhs;

	list->lhs[list->count++] = lhs;
	return true;
}

bool ofc_sema_lhs_list_elem_count(
	const ofc_sema_lhs_list_t* list, unsigned* count)
{
	if (!list)
		return false;

	unsigned i, c;
	for (i = 0, c = 0; i < list->count; i++)
	{
		unsigned e;
		if (!ofc_sema_lhs_elem_count(list->lhs[i], &e))
			return false;
		c += e;
	}

	if (count) *count = c;
	return true;
}

ofc_sema_lhs_t* ofc_sema_lhs_list_elem_get(
	const ofc_sema_lhs_list_t* list, unsigned offset)
{
	if (!list)
		return NULL;

	unsigned i, o;
	for (i = 0, o = offset; (i < list->count) && (o <= offset); i++)
	{
		unsigned e;
		if (!ofc_sema_lhs_elem_count(list->lhs[i], &e))
			return NULL;

		if (o < e)
		{
			return ofc_sema_lhs_elem_get(
				list->lhs[i], o);
		}

		o -= e;
	}

	return NULL;
}


bool ofc_sema_lhs_list_init(
	ofc_sema_lhs_list_t* lhs,
	const ofc_sema_expr_list_t* init)
{
	unsigned lhs_count;
	unsigned init_count;
	if (!ofc_sema_lhs_list_elem_count(lhs, &lhs_count)
		|| !ofc_sema_expr_list_elem_count(init, &init_count))
		return false;

	unsigned e = (lhs_count < init_count ? lhs_count : init_count);

	unsigned i;
	for (i = 0; i < e; i++)
	{
		ofc_sema_lhs_t* lhs_elem
			= ofc_sema_lhs_list_elem_get(lhs, i);
		if (!lhs_elem) return false;

		ofc_sema_expr_t* init_elem
			= ofc_sema_expr_list_elem_get(init, i);
		if (!init_elem)
		{
			ofc_sema_lhs_delete(lhs_elem);
			return false;
		}

		bool success = ofc_sema_lhs_init(
			lhs_elem, init_elem);

		ofc_sema_lhs_delete(lhs_elem);
		ofc_sema_expr_delete(init_elem);

		if (!success)
		{
			/* TODO - Fail atomically? */
			ofc_sparse_ref_error(init_elem->src,
				"Invalid initializer");
			return false;
		}
	}

	return true;
}


bool ofc_sema_lhs_list_print(
	ofc_colstr_t* cs,
	const ofc_sema_lhs_list_t* lhs_list)
{
	if (!cs || !lhs_list)
		return false;

	unsigned i;
	for (i = 0; i < lhs_list->count; i++)
	{
		if (!ofc_sema_lhs_print(cs, lhs_list->lhs[i]))
			return false;

		if ((lhs_list->count > 1)
			&& (i < lhs_list->count - 1))
		{
			if (!ofc_colstr_atomic_writef(cs, ", "))
				return false;
		}
	}
	return true;
}

