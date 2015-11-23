#include <ofc/sema.h>


static ofc_sema_lhs_t* ofc_sema_lhs_index(
	ofc_sema_lhs_t* lhs,
	ofc_sema_array_index_t* index)
{
	if (!lhs || !index)
		return NULL;

	ofc_sema_lhs_t* alhs
		= (ofc_sema_lhs_t*)malloc(
			sizeof(ofc_sema_lhs_t));
	if (!alhs) return NULL;

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
	if (!lhs || !slice)
		return NULL;

	ofc_sema_array_t* array
		= ofc_sema_array_slice_dims(slice);
	if (!array) return NULL;

	const ofc_sema_type_t* base_type
		= lhs->data_type;

	const ofc_sema_type_t* type
		= ofc_sema_type_create_array(
			base_type, array,
			base_type->is_static,
			base_type->is_automatic,
			base_type->is_volatile);
	if (!type)
	{
		ofc_sema_array_delete(array);
		return NULL;
	}

	ofc_sema_lhs_t* alhs
		= (ofc_sema_lhs_t*)malloc(
			sizeof(ofc_sema_lhs_t));
	if (!alhs) return NULL;

	alhs->type      = OFC_SEMA_LHS_ARRAY_SLICE;
	alhs->src       = lhs->src;
	alhs->parent    = lhs;
	alhs->slice     = slice;
	alhs->data_type = type;
	alhs->refcnt    = 0;

	return alhs;
}

static ofc_sema_lhs_t* ofc_sema_lhs_member(
	ofc_sema_lhs_t* lhs,
	ofc_str_ref_t member)
{
	if (!lhs || ofc_str_ref_empty(member))
		return NULL;

	/* TODO - Implement. */
	return NULL;
}


const ofc_sema_type_t* ofc_sema_lhs_decl_type(
	ofc_sema_scope_t* scope,
	const ofc_sema_type_t* type,
	const ofc_parse_lhs_t* lhs)
{
	if (!type || !lhs)
		return NULL;

	bool is_array = (lhs->type == OFC_PARSE_LHS_ARRAY);
	if (!is_array && (lhs->type != OFC_PARSE_LHS_VARIABLE))
		return NULL;

	const ofc_sema_type_t* itype = type;
	if (!itype)
	{
		if (!scope)
			return NULL;

		ofc_str_ref_t base_name;
		if (!ofc_parse_lhs_base_name(
			*lhs, &base_name))
			return NULL;

		itype = ofc_sema_implicit_get(
			scope->implicit, base_name.base[0]);
		if (!itype) return NULL;
	}

	if (!is_array)
		return itype;

	ofc_sema_array_t* array = ofc_sema_array(
		scope, lhs->array.index);
	if (!array) return NULL;

	const ofc_sema_type_t* atype
		= ofc_sema_type_create_array(
			itype, array,
			itype->is_static,
			itype->is_automatic,
			itype->is_volatile);
	ofc_sema_array_delete(array);
	if (!atype) return NULL;

	return ofc_sema_lhs_decl_type(
		scope, atype, lhs->parent);
}

static ofc_sema_lhs_t* ofc_sema__lhs(
	ofc_sema_scope_t* scope,
	const ofc_parse_lhs_t* lhs,
	bool is_expr)
{
	if (!scope || !lhs)
		return NULL;

	switch (lhs->type)
	{
		case OFC_PARSE_LHS_IMPLICIT_DO:
			ofc_sema_scope_error(scope, lhs->src,
				"Can't resolve implicit do to single %s.",
				(is_expr ? "primary expression": "LHS"));
			return NULL;

		case OFC_PARSE_LHS_STAR_LEN:
			ofc_sema_scope_error(scope, lhs->src,
				"Can't resolve star length to %s.",
				(is_expr ? "primary expression": "LHS"));
			return NULL;

		case OFC_PARSE_LHS_MEMBER_TYPE:
		case OFC_PARSE_LHS_MEMBER_STRUCTURE:
			{
				ofc_sema_lhs_t* parent
					= ofc_sema_lhs(scope, lhs->parent);
				if (!parent) return NULL;

				/* TODO - Check dereference type against structure type. */

				if (!parent->data_type
					|| (parent->data_type->type
						!= OFC_SEMA_TYPE_STRUCTURE))
				{
					ofc_sema_scope_error(scope, lhs->src,
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
				ofc_sema_lhs_t* parent
					= ofc_sema_lhs(scope, lhs->parent);
				if (!parent) return NULL;

				if (!ofc_sema_type_is_array(parent->data_type))
				{
					ofc_sema_scope_error(scope, lhs->src,
						"Attempting to index a variable that's not an array.");
					ofc_sema_lhs_delete(parent);
					return NULL;
				}

				ofc_sema_array_index_t* index
					= ofc_sema_array_index(scope,
						parent->data_type->array,
						lhs->array.index);
				if (index)
				{
					ofc_sema_lhs_t* slhs
						= ofc_sema_lhs_index(parent, index);
					if (!slhs)
					{
						ofc_sema_lhs_delete(parent);
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
				if (!slhs)
				{
					ofc_sema_lhs_delete(parent);
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
			? ofc_str_ref_equal(lhs->variable, root->name)
			: ofc_str_ref_equal_ci(lhs->variable, root->name)))
	{
		/* Special case for FUNCTION return value. */

		decl = ofc_sema_scope_decl_find_modify(
			scope, lhs->variable, true);
		if (!decl)
		{
			decl = ofc_sema_scope_decl_find_modify(
				root, lhs->variable, true);
		}

		if (!decl)
		{
			ofc_sema_decl_t* fdecl
				= ofc_sema_scope_decl_find_modify(
					root, lhs->variable, false);
			if (!fdecl)
			{
				/* This should never happen. */
				return NULL;
			}

			const ofc_sema_type_t* rtype
				= ofc_sema_decl_base_type(fdecl);
			decl = ofc_sema_decl_create(rtype, lhs->variable);
			if (!decl) return NULL;

			if (!ofc_sema_decl_list_add(
				root->decl, decl))
			{
				ofc_sema_decl_delete(decl);
				return NULL;
			}

			/* Can't modify return type once it's been referenced. */
			fdecl->is_implicit = false;
			decl->is_implicit  = false;
		}
	}
	else
	{
		decl = ofc_sema_scope_decl_find_modify(
				scope, lhs->variable, false);
		if (!decl)
		{
			decl = ofc_sema_decl_implicit_lhs(
				scope, lhs);
			if (!decl)
			{
				ofc_sema_scope_error(scope, lhs->src,
					"No declaration for '%.*s' and no valid IMPLICIT rule.",
					lhs->variable.size, lhs->variable.base);
				return NULL;
			}

			if (!ofc_sema_decl_list_add(
				scope->decl, decl))
			{
				ofc_sema_decl_delete(decl);
				return NULL;
			}

			if (is_expr)
			{
				ofc_sema_scope_warning(scope, lhs->src,
					"Referencing uninitialized variable '%.*s' in expression.",
					lhs->variable.size, lhs->variable.base);
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
		decl->lock = true;
	return slhs;
}

ofc_sema_lhs_t* ofc_sema_lhs(
	ofc_sema_scope_t* scope,
	const ofc_parse_lhs_t* lhs)
{
	return ofc_sema__lhs(
		scope, lhs, false);
}

ofc_sema_lhs_t* ofc_sema_lhs_expr(
	ofc_sema_scope_t* scope,
	const ofc_parse_lhs_t* lhs)
{
	return ofc_sema__lhs(
		scope, lhs, true);
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

		default:
			break;
	}

	free(lhs);
}


bool ofc_sema_lhs_init(
	const ofc_sema_scope_t* scope,
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
				scope, decl, init);

		case OFC_SEMA_LHS_ARRAY_INDEX:
		{
			unsigned offset;
			if (!ofc_sema_array_index_offset(
				scope, decl, lhs->index, &offset))
				return false;

			return ofc_sema_decl_init_offset(
				scope, decl, offset, init);
		}

		/* TODO - Initialize all LHS types. */

		default:
			break;
	}

	return false;
}

bool ofc_sema_lhs_init_array(
	const ofc_sema_scope_t* scope,
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
		scope, ofc_sema_lhs_decl(lhs),
		array, count, init);
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

unsigned ofc_sema_lhs_elem_count(
	const ofc_sema_lhs_t* lhs)
{
	if (!lhs)
		return 0;

	return ofc_sema_type_elem_count(
		lhs->data_type);
}

