#include <ofc/sema.h>


ofc_sema_array_t* ofc_sema_array_create(
	unsigned dimensions,
	unsigned* base, unsigned* count)
{
    if (!count) return NULL;

	ofc_sema_array_t* array
		= (ofc_sema_array_t*)(sizeof(ofc_sema_array_t)
			+ (dimensions * 2 * sizeof(unsigned)));
	if (!array) return NULL;

	array->dimensions = dimensions;

	unsigned i;
	for (i = 0; i < dimensions; i++)
	{
		unsigned b = (base ? base[i] : 1);
		array->size[i][0] = b;
		array->size[i][1] = count[i];
	}

	return array;
}

ofc_sema_array_t* ofc_sema_array(
	const ofc_sema_scope_t* scope,
	const ofc_sema_array_t* base,
	ofc_parse_array_index_t* index)
{
	if (!scope || !index)
		return NULL;

	if (base && (index->count > base->dimensions))
	{
		/* TODO - Get source location for array. */
		ofc_sema_scope_error(scope, OFC_STR_REF_EMPTY,
			"Index has too many dimensions for array.");
		return NULL;
	}

	unsigned ibase[index->count];
	unsigned icount[index->count];

	unsigned i;
	for (i = 0; i < index->count; i++)
	{
		ofc_parse_array_range_t* range
			= index->range[i];

		if (!range)
			return NULL;

		unsigned first = 0;
		if (range->first)
		{
			ofc_sema_expr_t* expr
				= ofc_sema_expr(scope, range->first);
			if (!expr) return NULL;
			/* TODO - Allow negative array indices. */
			bool resolved = ofc_sema_expr_resolve_uint(expr, &first);
			ofc_sema_expr_delete(expr);
			if (!resolved)
			{
				ofc_sema_scope_error(scope, range->first->src,
					"Failed to resolve array index as integer at compile time.");
				return NULL;
			}
		}
		else if (base && range->is_slice)
		{
			first = base->size[i][0];
		}
		else
		{
			/* This should never happen. */
			return NULL;
		}

		unsigned count = 0;
		if (range->last)
		{
			ofc_sema_expr_t* expr
				= ofc_sema_expr(scope, range->last);
			if (!expr) return NULL;
			/* TODO - Allow negative array indices. */
			unsigned last;
			bool resolved = ofc_sema_expr_resolve_uint(expr, &last);
			ofc_sema_expr_delete(expr);
			if (!resolved)
			{
				ofc_sema_scope_error(scope, range->last->src,
					"Failed to resolve array index (last) as integer at compile time.");
				return NULL;
			}

			if (last < first)
			{
				ofc_sema_scope_error(scope, range->last->src,
					"Last array index must be greater than first.");
				return NULL;
			}
			count = (last - first);
		}
		else if (range->is_slice)
		{
			if (!base)
			{
				ofc_sema_scope_error(scope, range->last->src,
					"Array declaration incomplete, last index undefined.");
				return NULL;
			}

			count = base->size[i][1];
		}
		else if (!base)
		{
			count = first;
			first = 1;
		}

		unsigned stride = 1;
		if (range->stride)
		{
			ofc_sema_expr_t* expr
				= ofc_sema_expr(scope, range->stride);
			if (!expr) return NULL;
			bool resolved = ofc_sema_expr_resolve_uint(expr, &stride);
			ofc_sema_expr_delete(expr);
			if (!resolved)
			{
				ofc_sema_scope_error(scope, range->first->src,
					"Failed to resolve array stride as positive integer at compile time.");
				return NULL;
			}

			if (stride == 0)
			{
				ofc_sema_scope_error(scope, range->first->src,
					"Array stride must be greater than zero.");
				return NULL;
			}
		}

		if (stride > 1)
		{
			ofc_sema_scope_error(scope, range->first->src,
				"Array stride not yet supported.");
			/* TODO - Support array stride at semantic level. */
			return NULL;
		}

		if (base)
		{
			if ((first < base->size[i][0])
				|| ((first + count) >= (base->size[i][0] + base->size[i][1])))
			{
				/* TODO - Handle this in 'parse/array.h'. */
				ofc_str_ref_t src = (range->first
					? range->first->src : OFC_STR_REF_EMPTY);
				if (range->last)
					src = ofc_str_ref_bridge(src, range->last->src);
				if (range->stride)
					src = ofc_str_ref_bridge(src, range->stride->src);

				ofc_sema_scope_error(scope, src,
					"Array index out of range.");
				return NULL;
			}
		}

		ibase[i]  = first;
		icount[i] = count;
	}

	return ofc_sema_array_create(
		index->count, ibase, icount);
}


void ofc_sema_array_delete(ofc_sema_array_t* array)
{
	free(array);
}


uint8_t ofc_sema_array_hash(
	const ofc_sema_array_t* array)
{
	if (!array)
		return 0;

	uint8_t hash = array->dimensions;

	unsigned i;
	for (i = 0; i < array->dimensions; i++)
	{
		hash += array->size[i][0];
		hash += array->size[i][1];
	}

	return hash;
}

bool ofc_sema_array_compare(
	const ofc_sema_array_t* a,
	const ofc_sema_array_t* b)
{
	if (!a || !b)
		return false;

	if (a == b)
		return true;

	if (a->dimensions != b->dimensions)
		return false;

	unsigned i, j;
	for (i = 0, j = 0; i < a->dimensions; i++, j += 2)
	{
		if ((a->size[j][0] != b->size[j][0])
			|| (a->size[j][1] != b->size[j][1]))
			return false;
	}

	return true;
}


unsigned ofc_sema_array_total(const ofc_sema_array_t* array)
{
	if (!array || (array->dimensions == 0))
		return 0;

	unsigned total = 1;
	unsigned i;
	for (i = 0; i < array->dimensions; i++)
		total *= array->size[i][1];

	return total;
}
