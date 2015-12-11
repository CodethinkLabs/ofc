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
		ofc_sema_array_dims_t* seg
			= &array->segment[i];

		seg->first     = 1;
		seg->first_var = false;

		if (index->range[i]->first)
		{
			ofc_sema_expr_t* expr = ofc_sema_expr(
				scope, index->range[i]->first);
			if (!expr)
			{
				ofc_sema_scope_error(scope,
					index->range[i]->first->src,
					"Invalid array base expression");
				ofc_sema_array_delete(array);
				return NULL;
			}

			const ofc_sema_typeval_t* ctv
				= ofc_sema_expr_constant(expr);
			seg->first_var = (ctv == NULL);
			if (!seg->first_var)
			{
				int64_t d;
				if (!ofc_sema_typeval_get_integer(ctv, &d))
				{
					ofc_sema_scope_error(scope,
						index->range[i]->first->src,
						"Failed to resolve array base");
					ofc_sema_expr_delete(expr);
					ofc_sema_array_delete(array);
					return NULL;
				}

				seg->first = d;
				if ((int64_t)seg->first != d)
				{
					ofc_sema_scope_error(scope,
						index->range[i]->first->src,
						"Array base out-of-range");
					ofc_sema_expr_delete(expr);
					ofc_sema_array_delete(array);
					return NULL;
				}
			}
			ofc_sema_expr_delete(expr);
		}

		if (index->range[i]->last)
		{
			ofc_sema_expr_t* expr = ofc_sema_expr(
				scope, index->range[i]->last);
			if (!expr)
			{
				ofc_sema_scope_error(scope,
					index->range[i]->last->src,
					"Invalid array last expression");
				ofc_sema_array_delete(array);
				return NULL;
			}

			const ofc_sema_typeval_t* ctv
				= ofc_sema_expr_constant(expr);
			seg->last_var = (ctv == NULL);
			if (!seg->last_var)
			{
				int64_t d;
				if (!ofc_sema_typeval_get_integer(ctv, &d))
				{
					ofc_sema_scope_error(scope,
						index->range[i]->last->src,
						"Failed to resolve array last");
					ofc_sema_expr_delete(expr);
					ofc_sema_array_delete(array);
					return NULL;
				}

				seg->last = d;
				if ((int64_t)seg->last != d)
				{
					ofc_sema_scope_error(scope,
						index->range[i]->last->src,
						"Array last out-of-range");
					ofc_sema_expr_delete(expr);
					ofc_sema_array_delete(array);
					return NULL;
				}

				if (!seg->first_var
					&& (seg->first > seg->last))
				{
					ofc_sema_scope_error(scope,
						index->range[i]->first->src,
						"Array last must be greater than base");
					ofc_sema_expr_delete(expr);
					ofc_sema_array_delete(array);
					return NULL;
				}
			}
			ofc_sema_expr_delete(expr);
		}
		else if (index->range[i]->first
			&& !seg->first_var)
		{
			seg->last_var = false;
			seg->last  = seg->first;

			seg->first_var = false;
			seg->first = 1;
		}
		else
		{
			seg->last_var = true;
			seg->last     = 0;
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
		copy->segment[i].first = array->segment[i].first;
		copy->segment[i].last  = array->segment[i].last;

		copy->segment[i].first_var = array->segment[i].first_var;
		copy->segment[i].last_var  = array->segment[i].last_var;
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
		if (!array->segment[i].first_var)
			hash += array->segment[i].first;
		if (!array->segment[i].last_var)
			hash += array->segment[i].last;
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
		if (a->segment[i].first_var
			|| a->segment[i].last_var
			|| b->segment[i].first_var
			|| b->segment[i].last_var)
			return false;

		if ((a->segment[i].first != b->segment[i].first)
			|| (a->segment[i].last != b->segment[i].last))
			return false;
	}

	return true;
}

bool ofc_sema_array_total(
	const ofc_sema_array_t* array,
	unsigned* total)
{
	if (!array)
		return false;

	unsigned t = 1;
	unsigned i;
	for (i = 0; i < array->dimensions; i++)
	{
		ofc_sema_array_dims_t seg
			= array->segment[i];

		if (seg.first_var || seg.last_var
			|| (seg.first > seg.last))
			return false;

		t *= ((seg.last - seg.first) + 1);
	}

	if (total) *total = t;
	return true;
}

bool ofc_sema_array_print(
	ofc_colstr_t* cs,
	const ofc_sema_array_t* array)
{
	if (!cs || !array)
		return false;

	if (!ofc_colstr_atomic_writef(cs, "("))
		return false;

	unsigned i;
	for (i = 0; i < array->dimensions; i++)
	{
		if ((i > 0) && !ofc_colstr_atomic_writef(cs, ", "))
			return false;

		ofc_sema_array_dims_t dims
			= array->segment[i];
		if (dims.first == 1)
		{
			if (!ofc_colstr_atomic_writef(
				cs, "%d", dims.last))
				return false;
		}
		else
		{
			if (!ofc_colstr_atomic_writef(cs, "%d:%d",
				dims.first, dims.last))
				return false;
		}
	}

	return ofc_colstr_atomic_writef(cs, ")");
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

			if (idx < array->segment[i].first)
			{
				ofc_sema_scope_warning(scope, expr->src,
					"Array index out-of-bounds (underflow)");
			}
			else if (idx > array->segment[i].last)
			{
				ofc_sema_scope_warning(scope, expr->src,
					"Array index out-of-bounds (overflow)");
			}
		}
	}

	return ai;
}

bool ofc_sema_array_index_print(
	ofc_colstr_t* cs,
	const ofc_sema_array_index_t* index)
{
	if (!cs || !index) return false;

	if (!ofc_colstr_atomic_writef(cs, "("))
		return false;

	unsigned i;
	for (i = 0; i < index->dimensions; i++)
	{
		if (!ofc_sema_expr_print(cs, index->index[i]))
			return false;
	}

	return ofc_colstr_atomic_writef(cs, ")");
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

		if (so < dims.first)
		{
			ofc_sema_scope_error(scope, expr->src,
				"Array index out-of-range, too low");
			return false;
		}
		else if (so > dims.last)
		{
			ofc_sema_scope_error(scope, expr->src,
				"Array index out-of-range, too high");
			return false;
		}

		o += ((so - dims.first) * s);
		s *= ((dims.last - dims.first) + 1);
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

		array->segment[j].first
			= slice->segment[i].base;
		array->segment[j].last
			= (slice->segment[i].base + slice->segment[i].count) - 1;
		j++;
	}

	return array;
}

bool ofc_sema_array_slice_print(
	ofc_colstr_t* cs,
	const ofc_sema_array_slice_t* slice)
{
	if (!cs || !slice)
		return false;

	if (!ofc_colstr_atomic_writef(cs, "("))
		return false;

	unsigned i;
	for (i = 0; i < slice->dimensions; i++)
	{
		if ((i > 0) && !ofc_colstr_atomic_writef(cs, ", "))
			return false;

		ofc_sema_array_segment_t seg
			= slice->segment[i];
		if (seg.index)
		{
			if (!ofc_sema_expr_print(cs, seg.index))
				return false;
		}
		else
		{
			if (!ofc_colstr_atomic_writef(cs, "%d:%u",
				seg.base, ((seg.base + seg.count) - 1)))
				return false;

			if ((seg.stride != 1) && !ofc_colstr_atomic_writef(
				cs, ":%u", seg.stride))
				return false;
		}
	}

	return ofc_colstr_atomic_writef(cs, ")");
}
