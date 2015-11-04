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
	if (!lhs) return NULL;

	alhs->type      = OFC_SEMA_LHS_ARRAY_INDEX;
	alhs->parent    = lhs;
	alhs->index     = index;
	alhs->data_type = lhs->data_type->subtype;
	alhs->refcnt    = 0;

	return alhs;
}

static ofc_sema_lhs_t* ofc_sema_lhs_slice(
	ofc_sema_lhs_t* lhs,
	ofc_sema_array_t* slice)
{
	if (!lhs || !slice)
		return NULL;

	ofc_sema_array_t* cslice
		= ofc_sema_array_copy(slice);
	if (!cslice) return NULL;

	const ofc_sema_type_t* base_type
		= lhs->data_type;

	const ofc_sema_type_t* type
		= ofc_sema_type_create_array(
			base_type, cslice,
			base_type->is_static,
			base_type->is_automatic,
			base_type->is_volatile);
	if (!type)
	{
		ofc_sema_array_delete(cslice);
		return NULL;
	}

	ofc_sema_lhs_t* alhs
		= (ofc_sema_lhs_t*)malloc(
			sizeof(ofc_sema_lhs_t));
	if (!lhs) return NULL;

	alhs->type      = OFC_SEMA_LHS_ARRAY_SLICE;
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
	ofc_parse_lhs_t* lhs)
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
		scope, NULL, lhs->array.index);
	if (!array) return NULL;

	const ofc_sema_type_t* atype
		= ofc_sema_type_create_array(
			itype, array,
			itype->is_static,
			itype->is_automatic,
			itype->is_volatile);
	if (!atype)
	{
		ofc_sema_array_delete(array);
		return NULL;
	}

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

				if (!parent->data_type
					|| (parent->data_type->type
						!= OFC_SEMA_TYPE_ARRAY))
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
					ofc_sema_lhs_delete(parent);
					if (!slhs)
					{
						ofc_sema_array_index_delete(index);
						return NULL;
					}
					return slhs;
				}

				/* TODO - Don't double-error when an index is out-of-bounds. */

				ofc_sema_array_t* slice
					= ofc_sema_array(scope,
						parent->data_type->array,
						lhs->array.index);
				if (!slice)
				{
					/* TODO - Resolve as function/subroutine assignment? */
					ofc_sema_lhs_delete(parent);
					return NULL;
				}

				ofc_sema_lhs_t* slhs
					= ofc_sema_lhs_slice(parent, slice);
				ofc_sema_lhs_delete(parent);
				if (!slhs)
				{
					ofc_sema_array_delete(slice);
					return NULL;
				}
				return slhs;
			}

		case OFC_PARSE_LHS_VARIABLE:
			break;

		default:
			return NULL;
	}

	ofc_sema_decl_t* decl
		= ofc_sema_scope_decl_find_modify(
			scope, lhs->variable);
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

	ofc_sema_lhs_t* slhs
		= (ofc_sema_lhs_t*)malloc(
			sizeof(ofc_sema_lhs_t));
	if (!slhs) return NULL;

	slhs->type      = OFC_SEMA_LHS_DECL;
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
			ofc_sema_array_index_delete(lhs->index);
			break;

		case OFC_SEMA_LHS_ARRAY_SLICE:
			ofc_sema_array_delete(lhs->slice);
			break;

		default:
			break;
	}

	free(lhs);
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
			if (!ofc_sema_array_compare(
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
	return (lhs ? lhs->decl : NULL);
}

const ofc_sema_type_t* ofc_sema_lhs_type(
	const ofc_sema_lhs_t* lhs)
{
	return (lhs ? lhs->data_type : NULL);
}
