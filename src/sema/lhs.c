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

	const ofc_sema_type_t* base_type
		= lhs->data_type;

	const ofc_sema_type_t* type
		= ofc_sema_type_create_array(
			base_type, array);
	if (!type)
	{
		ofc_sema_lhs_delete(lhs);
		ofc_sema_array_delete(array);
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

	alhs->type      = OFC_SEMA_LHS_ARRAY_SLICE;
	alhs->src       = lhs->src;
	alhs->parent    = lhs;
	alhs->slice     = slice;
	alhs->data_type = type;
	alhs->refcnt    = 0;

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
	ofc_sema_scope_t* decl_scope,
	const ofc_parse_lhs_t* lhs,
	bool is_expr)
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
					scope, decl_scope, lhs->parent, false);
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
					scope, decl_scope, lhs->parent, false);
				if (!parent) return NULL;

				if (!ofc_sema_type_is_array(parent->data_type))
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
						parent->data_type->array,
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
						parent->data_type->array,
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

			if (!ofc_sema_decl_list_add(
				root->decl, decl))
			{
				ofc_sema_decl_delete(decl);
				return NULL;
			}
		}
	}
	else
	{
		decl = ofc_sema_scope_decl_find_modify(
			scope, lhs->variable.string, false);
		if (!decl)
		{
			decl = ofc_sema_decl_implicit_lhs(
				decl_scope, lhs);
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

	ofc_sema_lhs_t* slhs
		= (ofc_sema_lhs_t*)malloc(
			sizeof(ofc_sema_lhs_t));
	if (!slhs) return NULL;

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
		scope, scope, lhs, false);
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
	ofc_sema_scope_t* decl_scope,
	const ofc_parse_lhs_t* lhs)
{
	return ofc_sema__lhs(
		scope, decl_scope, lhs, true);
}

static ofc_sema_lhs_t* ofc_sema_lhs__offset(
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

			if (!ofc_sema_type_is_composite(lhs->data_type))
			{
				if (offset != 0)
					return NULL;
				return lhs;
			}

			if (ofc_sema_type_is_array(lhs->data_type))
			{
				const ofc_sema_type_t* base_type
					= ofc_sema_type_base(lhs->data_type);

				unsigned base_count;
				if (!ofc_sema_type_elem_count(
					base_type, &base_count))
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
					= ofc_sema_lhs__offset(nlhs, base_offset);
				if (rlhs != nlhs)
					ofc_sema_lhs_delete(nlhs);
				return rlhs;
			}

			/* TODO - Handle structures. */
			break;

		case OFC_SEMA_LHS_ARRAY_SLICE:
			/* TODO - Convert offset to index and treat as index. */
			break;

		case OFC_SEMA_LHS_SUBSTRING:
			if (offset != 0)
				return NULL;
			return lhs;

		default:
			break;
	}

	return NULL;
}

static ofc_sema_lhs_t* ofc_sema_lhs__offset_ref(
	ofc_sema_lhs_t* lhs, unsigned offset)
{
    if (!lhs)
		return NULL;

	switch (lhs->type)
	{
		case OFC_SEMA_LHS_DECL:
		case OFC_SEMA_LHS_ARRAY_INDEX:
		case OFC_SEMA_LHS_STRUCTURE_MEMBER:
		case OFC_SEMA_LHS_SUBSTRING:
			if (ofc_sema_type_is_composite(lhs->data_type))
				break;

			if (ofc_sema_type_is_procedure(lhs->data_type)
				|| (offset != 0)
				|| !ofc_sema_lhs_reference(lhs))
				return NULL;
			return lhs;

		default:
			break;
	}

	return ofc_sema_lhs__offset(lhs, offset);
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
			ofc_sema_array_slice_delete(lhs->slice);
			break;

		case OFC_SEMA_LHS_SUBSTRING:
			if (lhs->substring.last != lhs->substring.first)
				ofc_sema_expr_delete(lhs->substring.last);
			ofc_sema_expr_delete(lhs->substring.first);
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
				a->slice, b->slice))
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
	return ofc_sema_type_elem_count(
		lhs->data_type, count);
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
			if (!ofc_sema_array_slice_print(cs, lhs->slice))
				return false;
			break;

		case OFC_SEMA_LHS_SUBSTRING:
			if (!ofc_colstr_atomic_writef(cs, "("))
				return false;
			if (lhs->substring.first
				&& !ofc_sema_expr_print(cs,
					lhs->substring.first))
				return false;
			if (lhs->substring.first != lhs->substring.last)
			{
				if (!ofc_colstr_atomic_writef(cs, ":"))
					return false;
				if (lhs->substring.last
					&& !ofc_sema_expr_print(cs,
						lhs->substring.last))
					return false;
			}
			if (!ofc_colstr_atomic_writef(cs, ")"))
				return false;
			break;

		case OFC_SEMA_LHS_STRUCTURE_MEMBER:
			if (!ofc_colstr_atomic_writef(cs, ".%.*s",
				lhs->member.size, lhs->member.base))
					return false;
			break;

		default:
			return false;
	}

	return true;
}


ofc_sema_lhs_list_t* ofc_sema_lhs_list(
	ofc_sema_scope_t* scope,
	const ofc_parse_lhs_list_t* plist)
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
		list->lhs[i] = ofc_sema_lhs(
			scope, plist->lhs[i]);
		if (!list->lhs[i])
		{
			ofc_sema_lhs_list_delete(list);
			return NULL;
		}

		list->count++;
	}

	return list;
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

bool ofc_sema_lhs_list_add_list(
	ofc_sema_lhs_list_t* alist,
	ofc_sema_lhs_list_t* blist)
{
	if (!alist || !blist)
		return false;

	unsigned i;
	for (i = 0; i < blist->count; i++)
	{
		if (!ofc_sema_lhs_reference(blist->lhs[i]))
			return false;

		if (!ofc_sema_lhs_list_add(alist, blist->lhs[i]))
			return false;
	}

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
	for (i = 0, o = 0; (i < list->count) && (o <= offset); i++)
	{
		unsigned e;
		if (!ofc_sema_lhs_elem_count(list->lhs[i], &e))
			return NULL;

		if (offset < (o + e))
		{
			return ofc_sema_lhs__offset_ref(
				list->lhs[i], (offset - o));
		}

		o += e;
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
		ofc_sema_expr_t* init_elem
			= ofc_sema_expr_list_elem_get(init, i);

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

static ofc_sema_lhs_list_t* ofc_sema_lhs_list__implicit_do(
	ofc_sema_scope_t* scope,
	ofc_sema_scope_t* decl_scope,
	ofc_parse_implicit_do_t* id)
{
	if (!scope || !id)
		return NULL;

	ofc_sema_scope_t* idscope
		= ofc_sema_scope_implicit_do(scope);
	if (!idscope)
		return NULL;

	ofc_sema_parameter_t* param
		= ofc_sema_parameter_assign(idscope, id->init);
	if (!param)
	{
		ofc_sema_scope_delete(idscope);
		return NULL;
	}

	if (!ofc_sema_scope_parameter_add(idscope, param))
	{
		ofc_sema_parameter_delete(param);
		ofc_sema_scope_delete(idscope);
		return NULL;
	}

	const ofc_sema_type_t* dtype
		= ofc_sema_parameter_type(param);
	if (!ofc_sema_type_is_scalar(dtype))
	{
		ofc_sparse_ref_error(id->init->name->src,
			"Implicit do loop iterator must be a scalar type.");
		ofc_sema_scope_delete(idscope);
		return NULL;
	}

	if (!ofc_sema_type_is_integer(dtype))
	{
		ofc_sparse_ref_warning(id->init->name->src,
			"Using REAL in implicit do loop iterator..");
	}

	ofc_sema_expr_t* limit
		= ofc_sema_expr(scope, id->limit);
	if (!limit)
	{
		ofc_sema_scope_delete(idscope);
		return NULL;
	}

	if (!ofc_sema_type_compare(dtype,
		ofc_sema_expr_type(limit)))
	{
		ofc_sema_expr_t* cast
			= ofc_sema_expr_cast(limit, dtype);
		if (!cast)
		{
			const ofc_sema_type_t* expr_type =
				ofc_sema_expr_type(limit);
			ofc_sparse_ref_error(id->limit->src,
					"Expression type %s doesn't match iterator type %s",
				ofc_sema_type_str_rep(expr_type),
				ofc_sema_type_str_rep(dtype));
			ofc_sema_expr_delete(limit);
			ofc_sema_scope_delete(idscope);
			return NULL;
		}
		limit = cast;
	}

	ofc_sema_expr_t* step = NULL;

	if (id->step)
	{
		step = ofc_sema_expr(
			scope, id->step);
		if (!step)
		{
			ofc_sema_expr_delete(limit);
			ofc_sema_scope_delete(idscope);
			return NULL;
		}

		if (!ofc_sema_type_compare(dtype,
			ofc_sema_expr_type(step)))
		{
			ofc_sema_expr_t* cast
				= ofc_sema_expr_cast(step, dtype);
			if (!cast)
			{
				const ofc_sema_type_t* expr_type =
					ofc_sema_expr_type(step);
				ofc_sparse_ref_error(id->step->src,
					"Expression type %s doesn't match iterator type %s",
					ofc_sema_type_str_rep(expr_type),
					ofc_sema_type_str_rep(dtype));
				ofc_sema_expr_delete(step);
				ofc_sema_expr_delete(limit);
				ofc_sema_scope_delete(idscope);
				return NULL;
			}
			step = cast;
		}
	}
	else
	{
		step = ofc_sema_expr_integer(1);
		if (!step)
		{
			ofc_sema_expr_delete(limit);
			ofc_sema_scope_delete(idscope);
			return NULL;
		}

		if (!ofc_sema_type_compare(dtype,
			ofc_sema_expr_type(step)))
		{
			ofc_sema_expr_t* cast
				= ofc_sema_expr_cast(step, dtype);
			if (!cast)
			{
				ofc_sema_expr_delete(step);
				ofc_sema_expr_delete(limit);
				ofc_sema_scope_delete(idscope);
				return NULL;
			}

			step = cast;
		}
	}

	ofc_sema_lhs_list_t* list
		= ofc_sema_lhs_list_create();
	if (!list)
	{
		ofc_sema_expr_delete(step);
		ofc_sema_expr_delete(limit);
		ofc_sema_scope_delete(idscope);
		return NULL;
	}

	ofc_sema_typeval_t* value
		= ofc_sema_typeval_le(
			param->typeval, limit->constant);
	if (!value)
	{
		ofc_sema_expr_delete(step);
		ofc_sema_expr_delete(limit);
		ofc_sema_scope_delete(idscope);
		ofc_sema_lhs_list_delete(list);
		return NULL;
	}

	while (value->logical)
	{
		if (id->dlist->type == OFC_PARSE_LHS_IMPLICIT_DO)
		{
			ofc_sema_lhs_list_t* implicit_do
				= ofc_sema_lhs_list__implicit_do(
					idscope, decl_scope, id->dlist->implicit_do);
			if (!implicit_do)
			{
				ofc_sema_expr_delete(step);
				ofc_sema_expr_delete(limit);
				ofc_sema_lhs_list_delete(list);
				ofc_sema_scope_delete(idscope);
				ofc_sema_typeval_delete(value);
				return NULL;
			}

			bool success = ofc_sema_lhs_list_add_list(
				list, implicit_do);
			ofc_sema_lhs_list_delete(implicit_do);

			if (!success)
			{
				ofc_sema_expr_delete(step);
				ofc_sema_expr_delete(limit);
				ofc_sema_lhs_list_delete(list);

				ofc_sema_scope_delete(idscope);
				ofc_sema_typeval_delete(value);
				return NULL;
			}
		}
		else
		{
			ofc_sema_lhs_t* lhs = ofc_sema__lhs(
				idscope, decl_scope, id->dlist, false);
			if (!ofc_sema_lhs_list_add(list, lhs))
			{
				ofc_sema_expr_delete(step);
				ofc_sema_expr_delete(limit);
				ofc_sema_lhs_list_delete(list);
				ofc_sema_scope_delete(idscope);
				ofc_sema_typeval_delete(value);
				return NULL;
			}
		}

		ofc_sema_typeval_t* ntv
			= ofc_sema_typeval_add(
				param->typeval, step->constant);
		ofc_sema_typeval_delete(param->typeval);
		param->typeval = ntv;

		ofc_sema_typeval_delete(value);
		value = ofc_sema_typeval_le(
			param->typeval, limit->constant);
		if (!value)
		{
			ofc_sema_expr_delete(step);
			ofc_sema_expr_delete(limit);
			ofc_sema_scope_delete(idscope);
			ofc_sema_lhs_list_delete(list);
			ofc_sema_typeval_delete(value);
			return NULL;
		}
	}

	ofc_sema_expr_delete(step);
	ofc_sema_expr_delete(limit);
	ofc_sema_scope_delete(idscope);
	ofc_sema_typeval_delete(value);

	return list;
}

ofc_sema_lhs_list_t* ofc_sema_lhs_list_implicit_do(
	ofc_sema_scope_t* scope, ofc_parse_implicit_do_t* id)
{
	return ofc_sema_lhs_list__implicit_do(
		scope, scope, id);
}


bool ofc_sema_lhs_list_print(
	ofc_colstr_t* cs,
	const ofc_sema_lhs_list_t* lhs_list)
{
	if (!cs || !lhs_list) return false;

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

