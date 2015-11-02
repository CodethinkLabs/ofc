#include <ofc/sema.h>


ofc_sema_lhs_t* ofc_sema_lhs(
	ofc_sema_scope_t* scope,
	ofc_parse_lhs_t* lhs)
{
	if (!scope || !lhs)
		return NULL;

	switch (lhs->type)
	{
		case OFC_PARSE_LHS_IMPLICIT_DO:
			/* TODO - Error: Can't resolve implicit do to single LHS. */
			return NULL;

		case OFC_PARSE_LHS_STAR_LEN:
			/* TODO - Error: Can't resolve star length to LHS. */
			return NULL;

		case OFC_PARSE_LHS_MEMBER_TYPE:
		case OFC_PARSE_LHS_MEMBER_STRUCTURE:
			/* TODO - Support structure members properly. */
			return NULL;

		case OFC_PARSE_LHS_ARRAY:
			{
				ofc_sema_lhs_t* parent
					= ofc_sema_lhs(scope, lhs->parent);
				if (!parent) return NULL;

				if (!parent->type
					|| (parent->type->type != OFC_SEMA_TYPE_ARRAY))
				{
					ofc_sema_scope_error(scope, lhs->src,
						"Attempting to index a variable that's not an array.");
					ofc_sema_lhs_delete(parent);
					return NULL;
				}

				ofc_sema_array_t* array
					= ofc_sema_array(scope,
						parent->type->array,
						lhs->array.index);
				if (!array)
				{
					ofc_sema_lhs_delete(parent);
					return NULL;
				}

				ofc_sema_lhs_t* slhs
					= ofc_sema_lhs_index(parent, array);
				ofc_sema_lhs_delete(parent);
				if (!slhs)
				{
					ofc_sema_array_delete(array);
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
		/* This shouldn't happen because variables should either
		   have been implicitly declared, or an error raised. */
		ofc_sema_scope_error(scope, lhs->src,
			"No matching declaration found for LHS.");
		return NULL;
	}

	ofc_sema_lhs_t* slhs
		= (ofc_sema_lhs_t*)malloc(
			sizeof(ofc_sema_lhs_t));
	if (!slhs) return NULL;

	slhs->decl   = decl;
	slhs->index  = NULL;
	slhs->type   = decl->type;
	slhs->refcnt = 0;
	return slhs;
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

	ofc_sema_array_delete(lhs->index);
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

	if (a->decl != b->decl)
		return false;

	if (!a->index && !b->index)
		return true;

	return ofc_sema_array_compare(
		a->index, b->index);
}


ofc_sema_decl_t* ofc_sema_lhs_decl(
	ofc_sema_lhs_t* lhs)
{
	return (lhs ? lhs->decl : NULL);
}

const ofc_sema_type_t* ofc_sema_lhs_type(
	const ofc_sema_lhs_t* lhs)
{
	return (lhs ? lhs->type : NULL);
}


ofc_sema_lhs_t* ofc_sema_lhs_index(
	ofc_sema_lhs_t* lhs,
	ofc_sema_array_t* index)
{
	if (!index || (index->dimensions == 0))
		return NULL;

	const ofc_sema_type_t* type
		= ofc_sema_lhs_type(lhs);
	if (!type) return NULL;

	if ((type->type != OFC_SEMA_TYPE_ARRAY)
		|| !type->array)
		return NULL;

    if (index->dimensions > type->array->dimensions)
	{
		/* TODO - Error: Too many dimensions in array index. */
		return NULL;
	}

	unsigned dims = type->array->dimensions;
	unsigned sdims = 0;
	unsigned base[dims];
	unsigned count[dims];

	unsigned i;
	for (i = 0; i < index->dimensions; i++)
	{
		unsigned ib = index->size[i][0];
		unsigned tb = type->array->size[i][0];
		unsigned ic = index->size[i][1];
		unsigned tc = type->array->size[i][1];

		if ((ib < tb) || (((ib - tb) + ic) > tc))
		{
			/* TODO - Error: Array index out of range. */
			return NULL;
		}

		base[i]  = ib;
		count[i] = ic;

		if (count[i] > 0)
			sdims++;
	}
	for (; i < type->array->dimensions; i++)
	{
		base[i]  = type->array->size[i][0];
		count[i] = type->array->size[i][1];

		if (count[i] > 0)
			sdims++;
	}

	ofc_sema_lhs_t* lhs_index
		= (ofc_sema_lhs_t*)malloc(
			sizeof(ofc_sema_lhs_t));
	if (!lhs_index) return NULL;

	lhs_index->decl  = lhs->decl;
	lhs_index->index = index;
	lhs_index->refcnt = 0;

	if (sdims == 0)
	{
		lhs_index->type = type;
	}
	else
	{
		unsigned sbase[sdims];
		unsigned scount[sdims];

		unsigned j = 0;
		for (i = 0; i < dims; i++)
		{
			if (count[i] == 0)
				continue;

			sbase[j]  = base[i];
			scount[j] = count[i];
			j += 1;
		}

		ofc_sema_array_t* sindex
			= ofc_sema_array_create(
				sdims, sbase, scount);
		if (!sindex)
		{
			free(lhs_index);
			return NULL;
		}

		const ofc_sema_type_t* stype
			= ofc_sema_type_create_array(
				type, sindex,
				type->is_static,
				type->is_automatic,
				type->is_volatile);
		if (!stype)
		{
			ofc_sema_array_delete(sindex);
			free(lhs_index);
			return NULL;
		}

		lhs_index->type = stype;
	}

	return lhs_index;
}

ofc_sema_lhs_t* ofc_sema_lhs_member(
	ofc_sema_lhs_t* lhs,
	ofc_str_ref_t member)
{
	if (!lhs || ofc_str_ref_empty(member))
		return NULL;

	/* TODO - Implement. */
	return NULL;
}
