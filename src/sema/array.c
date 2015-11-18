#include <ofc/sema.h>


static bool ofc_sema_array__base(
	const ofc_sema_array_t* array,
	unsigned dim,
	int64_t* base)
{
	if (!array)
		return false;

	if (dim >= array->dimensions)
		return false;

	ofc_sema_expr_t* expr
		= array->slice[dim].base;

	if (!expr)
	{
		if (base) *base = 1;
		return true;
	}

	const ofc_sema_typeval_t* ctv
		= ofc_sema_expr_constant(expr);
	if (!ctv) return NULL;

	return ofc_sema_typeval_get_integer(ctv, base);
}


ofc_sema_array_t* ofc_sema_array(
	ofc_sema_scope_t*              scope,
	const ofc_sema_array_t*        array,
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
			&& !index->range[i]->is_slice)
			return NULL;
	}

	if (array)
	{
		if (index->count > array->dimensions)
		{
			/* TODO - Positional error. */
			ofc_sema_scope_error(scope, OFC_STR_REF_EMPTY,
				"Array slice has too many dimensions.");
			return NULL;
		}
		else if (index->count == array->dimensions)
		{
			bool is_slice = false;
			for (i = 0; i < index->count; i++)
			{
				if (index->range[i]->is_slice
					|| index->range[i]->last
					|| index->range[i]->stride)
				{
					is_slice = true;
					break;
				}
			}

			if (!is_slice)
				return NULL;
		}
	}

	unsigned dims = (array ? array->dimensions : index->count);

	ofc_sema_array_t* slice
		= (ofc_sema_array_t*)malloc(sizeof(ofc_sema_array_t)
			+ (dims * sizeof(ofc_sema_array_slice_t)));
	if (!slice) return NULL;

	slice->dimensions = dims;

	for (i = 0; i < dims; i++)
		slice->slice[i].base = NULL;

	for (i = 0; i < index->count; i++)
	{
		if (index->range[i]->first)
		{
			slice->slice[i].base = ofc_sema_expr(
				scope, index->range[i]->first);
			if (!slice->slice[i].base)
			{
				ofc_sema_array_delete(slice);
				return NULL;
			}

			const ofc_sema_type_t* type
				= ofc_sema_expr_type(slice->slice[i].base);

			if (!ofc_sema_type_is_scalar(type))
			{
				ofc_sema_scope_error(scope,
					slice->slice[i].base->src,
					"Array index type must be scalar.");
				ofc_sema_array_delete(slice);
				return NULL;
			}

			if (!ofc_sema_type_is_integer(type))
			{
				ofc_sema_expr_t* cast = ofc_sema_expr_cast(
					scope, slice->slice[i].base,
					ofc_sema_type_integer_default());
				if (!cast)
				{
					ofc_sema_array_delete(slice);
					return NULL;
				}
				slice->slice[i].base = cast;
			}
		}

		int64_t array_base = 1;
		if (array && !ofc_sema_array__base(
			array, i, &array_base))
		{
			ofc_sema_array_delete(slice);
			return NULL;
		}

		int64_t base;
		bool base_const
			= ofc_sema_array__base(
				slice, i, &base);

		if (array && base_const)
		{
			if (base < array_base)
			{
				ofc_sema_scope_error(scope,
					slice->slice[i].base->src,
					"Array index out-of-bounds (underflow).");
				ofc_sema_array_delete(slice);
				return NULL;
			}
			else if (base >= (array_base + array->slice[i].count))
			{
				ofc_sema_scope_error(scope,
					slice->slice[i].base->src,
					"Array index out-of-bounds (overflow).");
				ofc_sema_array_delete(slice);
				return NULL;
			}
		}

		if (array && !index->range[i]->is_slice
			&& !index->range[i]->last
			&& !index->range[i]->stride)
		{
			slice->slice[i].count  = 1;
			slice->slice[i].stride = 1;
			continue;
		}

		if (!base_const)
		{
			ofc_sema_scope_error(scope,
				slice->slice[i].base->src,
				"Array slice base must be constant.");
			ofc_sema_array_delete(slice);
			return NULL;
		}

		int64_t count;
		if (index->range[i]->last)
		{
			ofc_sema_expr_t* last = ofc_sema_expr(
				scope, index->range[i]->last);
			if (!last)
			{
				ofc_sema_array_delete(slice);
				return NULL;
			}

			const ofc_sema_type_t* type
				= ofc_sema_expr_type(last);

			if (!ofc_sema_type_is_scalar(type))
			{
				ofc_sema_scope_error(scope, last->src,
					"Array slice last index must be scalar.");
				ofc_sema_expr_delete(last);
				ofc_sema_array_delete(slice);
				return NULL;
			}

			if (!ofc_sema_type_is_integer(type))
			{
				ofc_sema_expr_t* cast
					= ofc_sema_expr_cast(scope, last,
						ofc_sema_type_integer_default());
				if (!cast)
				{
					ofc_sema_expr_delete(last);
					ofc_sema_array_delete(slice);
					return NULL;
				}
				last = cast;
			}

			const ofc_sema_typeval_t* ctv
				= ofc_sema_expr_constant(last);
			if (!ctv)
			{
				ofc_sema_scope_error(scope, last->src,
					"Array slice last index must be constant.");
				ofc_sema_expr_delete(last);
				ofc_sema_array_delete(slice);
				return NULL;
			}

			bool resolved = ofc_sema_typeval_get_integer(ctv, &count);
			ofc_sema_expr_delete(last);
			if (!resolved)
			{
				ofc_sema_array_delete(slice);
				return NULL;
			}

			if (count < base)
			{
				ofc_sema_scope_error(scope, last->src,
					"Array slice last index must be greater than"
					" or equal to base.");
				ofc_sema_array_delete(slice);
				return NULL;
			}

			count = 1 + (count - base);
		}
		else if (array)
		{
			int64_t offset = (base - array_base);
			count = (array->slice[i].count - offset);
		}
		else
		{
			count = base;
			base = 1;
			ofc_sema_expr_delete(slice->slice[i].base);
			slice->slice[i].base = NULL;
		}

		if (count == 0)
		{
			/* TODO - Proper positional error. */
			ofc_sema_scope_error(scope, OFC_STR_REF_EMPTY,
				"Array slice count must be greater than zero.");
			ofc_sema_array_delete(slice);
			return NULL;
		}

		slice->slice[i].count = count;
		if ((int64_t)slice->slice[i].count != count)
		{
			/* TODO - Proper positional error. */
			ofc_sema_scope_error(scope, OFC_STR_REF_EMPTY,
				"Array slice count out of range.");
			ofc_sema_array_delete(slice);
			return NULL;
		}

		int64_t stride = 1;
		if (index->range[i]->stride)
		{
			ofc_sema_expr_t* expr = ofc_sema_expr(
				scope, index->range[i]->stride);
			if (!expr)
			{
				ofc_sema_array_delete(slice);
				return NULL;
			}

			const ofc_sema_type_t* type
				= ofc_sema_expr_type(expr);

			if (!ofc_sema_type_is_scalar(type))
			{
				ofc_sema_scope_error(scope, expr->src,
					"Array slice stride must be scalar.");
				ofc_sema_expr_delete(expr);
				ofc_sema_array_delete(slice);
				return NULL;
			}

			if (!ofc_sema_type_is_integer(type))
			{
				ofc_sema_expr_t* cast
					= ofc_sema_expr_cast(scope, expr,
					ofc_sema_type_integer_default());
				if (!cast)
				{
					ofc_sema_expr_delete(expr);
					ofc_sema_array_delete(slice);
					return NULL;
				}
				expr = cast;
			}

			const ofc_sema_typeval_t* ctv
				= ofc_sema_expr_constant(expr);
			if (!ctv)
			{
				ofc_sema_scope_error(scope, expr->src,
					"Array slice stride must be constant.");
				ofc_sema_expr_delete(expr);
				ofc_sema_array_delete(slice);
				return NULL;
			}

			bool resolved = ofc_sema_typeval_get_integer(ctv, &stride);
			ofc_sema_expr_delete(expr);
			if (!resolved)
			{
				ofc_sema_array_delete(slice);
				return NULL;
			}
		}

		if (array)
			stride *= array->slice[i].stride;

		if (stride == 0)
		{
			/* TODO - Proper positional error. */
			ofc_sema_scope_error(scope, OFC_STR_REF_EMPTY,
				"Array slice stride count must be non-zero.");
			ofc_sema_array_delete(slice);
			return NULL;
		}

		if (stride < 0)
		{
			/* TODO - Support negative array stride. */
			/* TODO - Proper positional error. */
			ofc_sema_scope_error(scope, OFC_STR_REF_EMPTY,
				"Negative array slice strides not yet supported.");
			ofc_sema_array_delete(slice);
			return NULL;
		}

		slice->slice[i].stride = stride;
		if ((int64_t)slice->slice[i].stride != stride)
		{
			/* TODO - Proper positional error. */
			ofc_sema_scope_error(scope, OFC_STR_REF_EMPTY,
				"Array slice stride out of range.");
			ofc_sema_array_delete(slice);
			return NULL;
		}
	}

	return slice;
}

ofc_sema_array_t* ofc_sema_array_copy(
	const ofc_sema_array_t* array)
{
	if (!array)
		return NULL;

	unsigned i;
	for (i = 0; i < array->dimensions; i++)
	{
		/* TODO - Handle array dims separately to slice. */
		if (array->slice[i].base)
			return NULL;
	}

	ofc_sema_array_t* copy
		= (ofc_sema_array_t*)malloc(sizeof(ofc_sema_array_t)
			+ (sizeof(ofc_sema_array_slice_t) * array->dimensions));
	if (!copy) return NULL;

	copy->dimensions = array->dimensions;

	for (i = 0; i < copy->dimensions; i++)
	{
		copy->slice[i].base   = NULL;
		copy->slice[i].count  = array->slice[i].count;
		copy->slice[i].stride = array->slice[i].stride;
	}

	return copy;
}

void ofc_sema_array_delete(
	ofc_sema_array_t* array)
{
	if (!array)
		return;

	unsigned i;
	for (i = 0; i < array->dimensions; i++)
	{
		ofc_sema_expr_delete(
			array->slice[i].base);
	}

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
		/* TODO - Hash the base expr. */
		hash += array->slice[i].count;
		hash += array->slice[i].stride;
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
		if ((a->slice[i].count != b->slice[i].count)
			|| (a->slice[i].stride != b->slice[i].stride)
			|| !ofc_sema_expr_compare(a->slice[i].base, b->slice[i].base))
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
		total *= array->slice[i].count;

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
			int64_t base, first;
			if (ofc_sema_array__base(array, i, &base)
				&& ofc_sema_typeval_get_integer(ctv, &first))
			{
				first *= array->slice[i].stride;

				if (first < base)
				{
					ofc_sema_scope_error(scope, expr->src,
						"Array index out-of-bounds (underflow).");
					ofc_sema_array_index_delete(ai);
					return NULL;
				}
				else if (first >= (base + array->slice[i].count))
				{
					ofc_sema_scope_error(scope, expr->src,
						"Array index out-of-bounds (overflow).");
					ofc_sema_array_index_delete(ai);
					return NULL;
				}
			}
		}
	}

	return ai;
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

	if (!decl->type || (decl->type->type != OFC_SEMA_TYPE_ARRAY))
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
		const ofc_sema_array_slice_t slice
			= array->slice[i];

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

		int64_t base;
		if (!ofc_sema_typeval_get_integer(
			ofc_sema_expr_constant(slice.base), &base))
			return false;

		if (so < base)
		{
			ofc_sema_scope_error(scope, expr->src,
				"Array index out-of-range, too low");
			return false;
		}
		so -= base;

		if (so >= slice.count)
		{
			ofc_sema_scope_error(scope, expr->src,
				"Array index out-of-range, too high");
			return false;
		}

		o += (so * s);
		s *= slice.count;
	}

	unsigned ec = ofc_sema_type_elem_count(
		decl->type->subtype);
	if (ec > 0) o *= ec;

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
