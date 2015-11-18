#include <ofc/sema.h>



ofc_sema_array_t* ofc_sema_array(
	ofc_sema_scope_t*              scope,
	const ofc_parse_array_index_t* index)
{
	if (!index || (index->count == 0))
		return NULL;

	unsigned i;
	for (i = 0; i < index->count; i++)
	{
		if (!index->range[i])
			return NULL;

		if (!index->range[i]->first
			|| index->range[i]->stride)
			return NULL;
	}

	ofc_sema_array_t* array
		= (ofc_sema_array_t*)malloc(sizeof(ofc_sema_array_t)
			+ (index->count * sizeof(ofc_sema_array_dims_t)));
	if (!array) return NULL;

	array->dimensions = index->count;

	for (i = 0; i < index->count; i++)
	{
		array->segment[i].base = 1;

		ofc_sema_expr_t* expr = ofc_sema_expr(
			scope, index->range[i]->first);

		const ofc_sema_typeval_t* ctv
			= ofc_sema_expr_constant(expr);

		int64_t d;
		bool got = ofc_sema_typeval_get_integer(ctv, &d);
		ofc_sema_expr_delete(expr);

		if (!got)
		{
			ofc_sema_scope_error(scope,
				index->range[i]->first->src,
				"Failed to resolve array dimension");
			ofc_sema_array_delete(array);
			return NULL;
		}

		if (index->range[i]->last)
		{
			array->segment[i].base = d;
			if ((int64_t)array->segment[i].base != d)
			{
				ofc_sema_scope_error(scope,
					index->range[i]->first->src,
					"Array base out-of-range");
				ofc_sema_array_delete(array);
				return NULL;
			}

			expr = ofc_sema_expr(
				scope, index->range[i]->last);

			ctv = ofc_sema_expr_constant(expr);

			got = ofc_sema_typeval_get_integer(ctv, &d);
			ofc_sema_expr_delete(expr);

			if (!got)
			{
				ofc_sema_scope_error(scope,
					index->range[i]->last->src,
					"Failed to resolve last array index");
				ofc_sema_array_delete(array);
				return NULL;
			}

			if (d <= array->segment[i].base)
			{
				ofc_sema_scope_error(scope,
					index->range[i]->first->src,
					"Last array index must be greater than base");
				ofc_sema_array_delete(array);
				return NULL;
			}

			d -= array->segment[i].base;

			/* Count is inclusive. */
			if ((d + 1) == 0)
			{
				ofc_sema_array_delete(array);
				return NULL;
			}
			d += 1;

			array->segment[i].count = d;
			if ((int64_t)array->segment[i].count != d)
			{
				ofc_sema_scope_error(scope,
					index->range[i]->first->src,
					"Last array index out-of-range");
				ofc_sema_array_delete(array);
				return NULL;
			}
		}
		else
		{
			if (d < 0)
			{
				ofc_sema_scope_error(scope,
					index->range[i]->first->src,
					"Array count must be positive");
				ofc_sema_array_delete(array);
				return NULL;
			}

			array->segment[i].count = d;
			if ((int64_t)array->segment[i].count != d)
			{
				ofc_sema_scope_error(scope,
					index->range[i]->first->src,
					"Array count out-of-range");
				ofc_sema_array_delete(array);
				return NULL;
			}
		}
	}

	return array;
}

ofc_sema_array_t* ofc_sema_array_copy(
	const ofc_sema_array_t* array)
{
	if (!array)
		return NULL;

	ofc_sema_array_t* copy
		= (ofc_sema_array_t*)malloc(sizeof(ofc_sema_array_t)
			+ (sizeof(ofc_sema_array_dims_t) * array->dimensions));
	if (!copy) return NULL;

	copy->dimensions = array->dimensions;

	unsigned i;
	for (i = 0; i < copy->dimensions; i++)
	{
		copy->segment[i].base  = array->segment[i].base;
		copy->segment[i].count = array->segment[i].count;
	}

	return copy;
}

void ofc_sema_array_delete(
	ofc_sema_array_t* array)
{
	if (!array)
		return;

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
		hash += array->segment[i].base;
		hash += array->segment[i].count;
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

	unsigned i;
	for (i = 0; i < a->dimensions; i++)
	{
		if ((a->segment[i].base != b->segment[i].base)
			|| (a->segment[i].count != b->segment[i].count))
			return false;
	}

	return true;
}

unsigned ofc_sema_array_total(const ofc_sema_array_t* array)
{
	if (!array)
		return 0;

	unsigned total = 1;
	unsigned i;
	for (i = 0; i < array->dimensions; i++)
		total *= array->segment[i].count;

	return total;
}



ofc_sema_array_index_t* ofc_sema_array_index(
	ofc_sema_scope_t*              scope,
	const ofc_sema_array_t*        array,
	const ofc_parse_array_index_t* index)
{
	if (!index || !array
		|| (index->count != array->dimensions))
		return NULL;

	unsigned i;
	for (i = 0; i < index->count; i++)
	{
		if (!index->range[i]
			|| index->range[i]->is_slice
			|| index->range[i]->last
			|| index->range[i]->stride)
			return NULL;
	}

	ofc_sema_array_index_t* ai
		= (ofc_sema_array_index_t*)malloc(sizeof(ofc_sema_array_index_t)
			+ (index->count * sizeof(ofc_sema_expr_t*)));
	if (!ai) return NULL;

	ai->dimensions = index->count;

	for (i = 0; i < index->count; i++)
		ai->index[i] = NULL;

	for (i = 0; i < index->count; i++)
	{
		ai->index[i] = ofc_sema_expr(
			scope, index->range[i]->first);
		if (!ai->index[i])
		{
			ofc_sema_array_index_delete(ai);
			return NULL;
		}

		ofc_sema_expr_t* expr = ai->index[i];

		const ofc_sema_type_t* type
			= ofc_sema_expr_type(expr);

		if (!ofc_sema_type_is_scalar(type))
		{
			ofc_sema_scope_error(scope, expr->src,
				"Array index type must be scalar.");
			ofc_sema_array_index_delete(ai);
			return NULL;
		}

		if (!ofc_sema_type_is_integer(type))
		{
			ofc_sema_expr_t* cast
				= ofc_sema_expr_cast(scope, expr,
					ofc_sema_type_integer_default());
			if (!cast)
			{
				ofc_sema_array_index_delete(ai);
				return NULL;
			}
			ai->index[i] = cast;
			expr = ai->index[i];
		}

		const ofc_sema_typeval_t* ctv
			= ofc_sema_expr_constant(expr);
		if (ctv)
		{
			int64_t idx;
			if (!ofc_sema_typeval_get_integer(ctv, &idx))
			{
				ofc_sema_scope_error(scope, expr->src,
					"Array index must resolve as integer");
				ofc_sema_array_index_delete(ai);
				return NULL;
			}

			if (idx < array->segment[i].base)
			{
				ofc_sema_scope_error(scope, expr->src,
					"Array index out-of-bounds (underflow)");
				ofc_sema_array_index_delete(ai);
				return NULL;
			}
			idx -= array->segment[i].base;

			if (idx >= array->segment[i].count)
			{
				ofc_sema_scope_error(scope, expr->src,
					"Array index out-of-bounds (overflow)");
				ofc_sema_array_index_delete(ai);
				return NULL;
			}
		}
	}

	return ai;
}

bool ofc_sema_array_index_print(ofc_colstr_t* cs,
	const ofc_sema_array_index_t* index)
{
	if (!cs || !index) return false;

	unsigned i;
	for (i = 0; i < index->dimensions; i++)
	{
		if (!ofc_sema_expr_print(cs, index->index[i]))
			return false;
	}

	return true;
}

void ofc_sema_array_index_delete(
	ofc_sema_array_index_t* index)
{
	if (!index)
		return;

	unsigned i;
	for (i = 0; i < index->dimensions; i++)
		ofc_sema_expr_delete(index->index[i]);

	free(index);
}


bool ofc_sema_array_index_offset(
	const ofc_sema_scope_t*       scope,
	const ofc_sema_decl_t*        decl,
	const ofc_sema_array_index_t* index,
	unsigned* offset)
{
    if (!scope || !decl || !index
		|| (index->dimensions == 0))
		return false;

	if (!ofc_sema_type_is_array(decl->type))
	{
		/* TODO - Positional error. */
		ofc_sema_scope_error(scope, OFC_STR_REF_EMPTY,
			"Can't index non-array type");
		return false;
	}

	const ofc_sema_array_t* array
		= decl->type->array;
	if (!array) return false;

	if (index->dimensions
		!= array->dimensions)
	{
		/* TODO - Positional error. */
		ofc_sema_scope_error(scope, OFC_STR_REF_EMPTY,
			"Index dimensions don't match array");
		return false;
	}

	int64_t o = 0;
	int64_t s = 1;

	unsigned i;
	for (i = 0; i < index->dimensions; i++)
	{
		const ofc_sema_array_dims_t dims
			= array->segment[i];

		const ofc_sema_expr_t* expr
			= index->index[i];
		if (!expr) return false;

		int64_t so;
		if (!ofc_sema_typeval_get_integer(
			ofc_sema_expr_constant(expr), &so))
		{
			ofc_sema_scope_error(scope, expr->src,
				"Failed to resolve array index");
			return false;
		}

		if (so < dims.base)
		{
			ofc_sema_scope_error(scope, expr->src,
				"Array index out-of-range, too low");
			return false;
		}
		so -= dims.base;

		if (so >= dims.count)
		{
			ofc_sema_scope_error(scope, expr->src,
				"Array index out-of-range, too high");
			return false;
		}

		o += (so * s);
		s *= dims.count;
	}

	unsigned uo = o;
	if ((int64_t)uo != o)
		return false;

	if (offset) *offset = uo;
	return true;
}


bool ofc_sema_array_index_compare(
	const ofc_sema_array_index_t* a,
	const ofc_sema_array_index_t* b)
{
	if (!a || !b)
		return false;

	if (a == b)
		return true;

	if (a->dimensions != b->dimensions)
		return false;

	unsigned i;
	for (i = 0; i < a->dimensions; i++)
	{
		if (!ofc_sema_expr_compare(
			a->index[i], b->index[i]))
			return false;
	}

	return true;
}


ofc_sema_array_slice_t* ofc_sema_array_slice(
	ofc_sema_scope_t*              scope,
	const ofc_sema_array_t*        array,
	const ofc_parse_array_index_t* index)
{
	if (!scope || !array || !index)
		return NULL;

	/* TODO - Implement. */

	return NULL;
}

void ofc_sema_array_slice_delete(
	ofc_sema_array_slice_t* slice)
{
	if (!slice)
		return;

	unsigned i;
	for (i = 0; i < slice->dimensions; i++)
	{
		ofc_sema_expr_delete(
			slice->segment[i].index);
	}

	free(slice);
}

bool ofc_sema_array_slice_compare(
	const ofc_sema_array_slice_t* a,
	const ofc_sema_array_slice_t* b)
{
	if (!a || !b)
		return false;

	if (a == b)
		return true;

	if (a->dimensions
		!= b->dimensions)
		return false;

	unsigned i;
	for (i = 0; i < a->dimensions; i++)
	{
		if (a->segment[i].index)
		{
			if (!ofc_sema_expr_compare(
				a->segment[i].index,
				b->segment[i].index))
				return false;
		}
		else
		{
			if ((a->segment[i].base != b->segment[i].base)
				|| (a->segment[i].count != b->segment[i].count)
				|| (a->segment[i].stride != b->segment[i].stride))
				return false;
		}
	}

	return true;
}

ofc_sema_array_t* ofc_sema_array_slice_dims(
	const ofc_sema_array_slice_t* slice)
{
	if (!slice)
		return NULL;

	unsigned i, d;
	for (i = 0, d = 0; i < slice->dimensions; i++)
	{
		if (!slice->segment[i].index)
			d++;
	}

	if (d == 0)
		return NULL;

	ofc_sema_array_t* array
		= (ofc_sema_array_t*)malloc(sizeof(ofc_sema_array_t)
			+ (d * sizeof(ofc_sema_array_dims_t)));
	if (!array) return NULL;

	array->dimensions = d;

	unsigned j;
	for (i = 0, j = 0; i < slice->dimensions; i++)
	{
		if (slice->segment[i].index)
			continue;

		array->segment[j].base  = slice->segment[i].base;
		array->segment[j].count = slice->segment[i].count;
		j++;
	}

	return array;
}
