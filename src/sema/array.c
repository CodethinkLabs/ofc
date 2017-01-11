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



ofc_sema_array_t* ofc_sema__array(
	ofc_sema_scope_t*              scope,
	const ofc_parse_array_index_t* index,
	bool scan)
{
	if (!index || (index->count == 0))
		return NULL;

	unsigned i;
	for (i = 0; i < index->count; i++)
	{
		if (!index->range[i])
			return NULL;

		/* For assumed size/shape arrays. */
		if (!index->range[i]->first
			&& !index->range[i]->last)
			continue;

		if (!index->range[i]->first
			|| index->range[i]->stride)
			return NULL;
	}

	ofc_sema_array_t* array
		= (ofc_sema_array_t*)malloc(sizeof(ofc_sema_array_t)
			+ (index->count * sizeof(ofc_sema_array_dims_t)));
	if (!array) return NULL;

	array->scan       = scan;
	array->dimensions = index->count;

	for (i = 0; i < index->count; i++)
	{
		array->segment[i].first = NULL;
		array->segment[i].last  = NULL;
	}

	if (scan) return array;

	for (i = 0; i < index->count; i++)
	{
		ofc_sema_array_dims_t* seg
			= &array->segment[i];

		if (index->range[i]->first)
		{
			seg->first = ofc_sema_expr(
				scope, index->range[i]->first);
			if (!seg->first)
			{
				ofc_sparse_ref_error(
					index->range[i]->first->src,
					"Invalid array base expression");
				ofc_sema_array_delete(array);
				return NULL;
			}

			const ofc_sema_type_t* type
				= ofc_sema_expr_type(seg->first);
			if (!type)
			{
				ofc_sema_array_delete(array);
				return NULL;
			}

			if (!ofc_sema_type_compatible(
				type, ofc_sema_type_integer_default()))
			{
				ofc_sema_expr_t* cast
					= ofc_sema_expr_cast(seg->first,
						ofc_sema_type_integer_default());
				if (!cast)
				{
					ofc_sema_array_delete(array);
					return NULL;
				}
				seg->first = cast;
			}
		}

		if (index->range[i]->last)
		{
			seg->last = ofc_sema_expr(
				scope, index->range[i]->last);
			if (!seg->last)
			{
				ofc_sparse_ref_error(
					index->range[i]->last->src,
					"Invalid array last expression");
				ofc_sema_array_delete(array);
				return NULL;
			}

			const ofc_sema_type_t* type
				= ofc_sema_expr_type(seg->last);
			if (!type)
			{
				ofc_sema_array_delete(array);
				return NULL;
			}

			if (!ofc_sema_type_compatible(
				type, ofc_sema_type_integer_default()))
			{
				ofc_sema_expr_t* cast
					= ofc_sema_expr_cast(seg->last,
						ofc_sema_type_integer_default());
				if (!cast)
				{
					ofc_sema_array_delete(array);
					return NULL;
				}
				seg->last = cast;
			}
		}
		else if (!index->range[i]->is_slice)
		{
			seg->last  = seg->first;
			seg->first = NULL;
		}
	}

	return array;
}

ofc_sema_array_t* ofc_sema_array(
	ofc_sema_scope_t*              scope,
	const ofc_parse_array_index_t* index)
{
	return ofc_sema__array(scope, index, false);
}

ofc_sema_array_t* ofc_sema_array_scan(
	ofc_sema_scope_t*              scope,
	const ofc_parse_array_index_t* index)
{
	return ofc_sema__array(scope, index, true);
}


ofc_sema_array_t* ofc_sema_array_array(
	ofc_sema_scope_t* scope,
	const ofc_parse_expr_list_t* list)
{
	if (!list || (list->count == 0))
		return NULL;

	ofc_sema_array_t* array
		= (ofc_sema_array_t*)malloc(sizeof(ofc_sema_array_t)
			+ (list->count * sizeof(ofc_sema_array_dims_t)));
	if (!array) return NULL;
	array->dimensions = list->count;

	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		array->segment[i].first = NULL;
		array->segment[i].last  = NULL;
	}

	for (i = 0; i < list->count; i++)
	{
		ofc_sema_expr_t* expr
			= ofc_sema_expr(scope, list->expr[i]);
		if (!expr)
		{
			ofc_sema_array_delete(array);
			return NULL;
		}

		array->segment[i].last = expr;

		if (!ofc_sema_expr_is_constant(expr)
			|| !ofc_sema_expr_validate_uint(expr))
		{
			ofc_sparse_ref_error(expr->src,
				"Expected positive INTEGER in SHAPE definition");
			ofc_sema_array_delete(array);
			return NULL;
		}
	}

	return array;
}

ofc_sema_array_t* ofc_sema_array_copy_replace(
	const ofc_sema_array_t* array,
	const ofc_sema_decl_t*  replace,
	const ofc_sema_expr_t*  with)
{
	if (!array)
		return NULL;

	ofc_sema_array_t* copy
		= (ofc_sema_array_t*)malloc(sizeof(ofc_sema_array_t)
			+ (sizeof(ofc_sema_array_dims_t) * array->dimensions));
	if (!copy) return NULL;

	copy->dimensions = array->dimensions;

	bool fail = false;
	unsigned i;
	for (i = 0; i < copy->dimensions; i++)
	{
		copy->segment[i].first
			= ofc_sema_expr_copy_replace(
				array->segment[i].first, replace, with);
		copy->segment[i].last
			= ofc_sema_expr_copy_replace(
				array->segment[i].last, replace, with);

		if (array->segment[i].first
			&& !copy->segment[i].first)
			fail = true;
		if (array->segment[i].last
			&& !copy->segment[i].last)
			fail = true;
	}

	if (fail)
	{
		ofc_sema_array_delete(copy);
		return NULL;
	}

	return copy;
}

ofc_sema_array_t* ofc_sema_array_copy(
	const ofc_sema_array_t* array)
{
	return ofc_sema_array_copy_replace(
		array, NULL, NULL);
}

void ofc_sema_array_delete(
	ofc_sema_array_t* array)
{
	if (!array)
		return;

	unsigned i;
	for (i = 0; i < array->dimensions; i++)
	{
		ofc_sema_expr_delete(array->segment[i].first);
		ofc_sema_expr_delete(array->segment[i].last);
	}

	free(array);
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

	if (a->scan || b->scan)
		return true;

	unsigned i;
	for (i = 0; i < a->dimensions; i++)
	{
		if (!ofc_sema_expr_compare(
			a->segment[i].first,
			b->segment[i].first))
		{
			int first[2] = { 1, 1 };
			if (a->segment[i].first
				&& !ofc_sema_expr_resolve_int(
					a->segment[i].first, &first[0]))
				return false;
			if (b->segment[i].first
				&& !ofc_sema_expr_resolve_int(
					b->segment[i].first, &first[1]))
				return false;

			if (first[0] != first[1])
				return false;
		}

		if (!ofc_sema_expr_compare(
			a->segment[i].last,
			b->segment[i].last))
		{
			if (!a->segment[i].last
				&& b->segment[i].last)
				return false;
			if (!b->segment[i].last
				&& a->segment[i].last)
				return false;

			if (a->segment[i].last
				&& b->segment[i].last)
			{
				int last[2];
				if (!ofc_sema_expr_resolve_int(
					a->segment[i].last, &last[0])
					|| !ofc_sema_expr_resolve_int(
						b->segment[i].last, &last[1]))
					return false;
				if (last[0] != last[1])
					return false;
			}
		}
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

		int first = 1, last;
		if (seg.first && !ofc_sema_expr_resolve_int(
			seg.first, &first))
			return false;
		if (!ofc_sema_expr_resolve_int(
			seg.last, &last))
			return false;

		if (last < first)
			return false;

		t *= ((last - first) + 1);
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

	unsigned i;
	for (i = 0; i < array->dimensions; i++)
	{
		if ((i > 0) && !ofc_colstr_atomic_writef(cs, ", "))
			return false;

		ofc_sema_array_dims_t dims
			= array->segment[i];

		if (dims.first)
		{
			if (!ofc_sema_expr_print(cs, dims.first)
				|| !ofc_colstr_atomic_writef(cs, ":"))
				return false;
		}

		/* TODO - Properly differentiate between assumed size and shape. */
		bool is_last = ((i + 1) == array->dimensions);
		if (dims.last)
		{
			if (!ofc_sema_expr_print(cs, dims.last))
				return false;
		}
		else if (!is_last)
		{
			if (!dims.first
				&& !ofc_colstr_atomic_writef(cs, ":"))
				return false;
		}
		else
		{
			if (!ofc_colstr_atomic_writef(cs, "*"))
				return false;
		}
	}

	return true;
}

bool ofc_sema_array_print_size(
	ofc_colstr_t* cs,
	const ofc_sema_array_t* array)
{
	if (!cs || !array)
		return false;

	unsigned i;
	for (i = 0; i < array->dimensions; i++)
	{
		if ((i > 0) && !ofc_colstr_atomic_writef(cs, ", "))
			return false;

		ofc_sema_array_dims_t dims
			= array->segment[i];

		int first = 1, last;
		if (dims.first && !ofc_sema_expr_resolve_int(
			dims.first, &first))
			return false;
		if (!ofc_sema_expr_resolve_int(
			dims.last, &last))
			return false;

		unsigned count = 1 + (first <= last ?
			(last - first) : (first - last));

		if (!ofc_colstr_atomic_writef(cs, "%u", count))
			return false;
	}

	return true;
}

bool ofc_sema_array_print_brackets(
	ofc_colstr_t* cs,
	const ofc_sema_array_t* array)
{
	return (ofc_colstr_atomic_writef(cs, "(")
		&& ofc_sema_array_print(cs, array)
		&& ofc_colstr_atomic_writef(cs, ")"));
}


bool ofc_sema_array_foreach_expr(
	ofc_sema_array_t* array, void* param,
	bool (*func)(ofc_sema_expr_t* expr, void* param))
{
	if (!array || !func)
		return false;

	unsigned i;
	for (i = 0; i < array->dimensions; i++)
	{
		if (array->segment[i].first
			&& !func(array->segment[i].first, param))
			return false;
		if (array->segment[i].last
			&& !func(array->segment[i].last, param))
			return false;
	}

	return true;
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
			ofc_sparse_ref_error(expr->src,
				"Array index type must be scalar.");
			ofc_sema_array_index_delete(ai);
			return NULL;
		}

		if (!ofc_sema_type_is_integer(type))
		{
			ofc_sema_expr_t* cast
				= ofc_sema_expr_cast(expr,
					ofc_sema_type_integer_default());
			if (!cast)
			{
				ofc_sema_array_index_delete(ai);
				return NULL;
			}
			ai->index[i] = cast;
			expr = ai->index[i];
		}

		if (ofc_sema_expr_is_constant(expr))
		{
			int idx;
			if (!ofc_sema_expr_resolve_int(
				expr, &idx))
			{
				ofc_sparse_ref_error(expr->src,
					"Array index must resolve as integer");
				ofc_sema_array_index_delete(ai);
				return NULL;
			}

			int first;
			if (ofc_sema_expr_resolve_int(
				array->segment[i].first, &first)
				&& (idx < first))
			{
				ofc_sparse_ref_warning(expr->src,
					"Array index out-of-bounds (underflow)");
			}

			int last;
			if (ofc_sema_expr_resolve_int(
				array->segment[i].last, &last)
				&& (idx > last))
			{
				ofc_sparse_ref_warning(expr->src,
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
		if (i > 0)
		{
			if (!ofc_colstr_atomic_writef(cs, ",")
				|| !ofc_colstr_atomic_writef(cs, " "))
				return false;
		}

		if (!ofc_sema_expr_print(cs, index->index[i]))
			return false;
	}

	return ofc_colstr_atomic_writef(cs, ")");
}

ofc_sema_array_index_t* ofc_sema_array_index_copy_replace(
	const ofc_sema_array_index_t* index,
	const ofc_sema_decl_t*        replace,
	const ofc_sema_expr_t*        with)
{
	if (!index)
		return NULL;

	ofc_sema_array_index_t* copy
		= (ofc_sema_array_index_t*)malloc(
			sizeof(ofc_sema_array_index_t)
			+ (index->dimensions * sizeof(ofc_sema_expr_t*)));
	if (!copy) return NULL;

	copy->dimensions = index->dimensions;

	bool success = true;
	unsigned i;
	for (i = 0; i < index->dimensions; i++)
	{
		copy->index[i] = ofc_sema_expr_copy_replace(
			index->index[i], replace, with);
		if (!copy->index[i])
			success = false;
	}

	if (!success)
	{
		ofc_sema_array_index_delete(copy);
		return NULL;
	}

	return copy;
}

ofc_sema_array_index_t* ofc_sema_array_index_copy(
	const ofc_sema_array_index_t* index)
{
	return ofc_sema_array_index_copy_replace(
		index, NULL, NULL);
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


ofc_sema_array_index_t* ofc_sema_array_index_from_offset(
	const ofc_sema_decl_t* decl, unsigned offset)
{
	if (!ofc_sema_decl_is_array(decl))
		return NULL;

	ofc_sema_array_t* array
		= decl->array;
	if (!array) return NULL;

	int      first[array->dimensions];
	unsigned count[array->dimensions];

	unsigned i;
	for (i = 0; i < array->dimensions; i++)
	{
		first[i] = 1;
		if (array->segment[i].first
			&& !ofc_sema_expr_resolve_int(
				array->segment[i].first, &first[i]))
			return NULL;

		int last;
		if (!ofc_sema_expr_resolve_int(
			array->segment[i].last, &last))
			return NULL;

		if (last < first[i])
			return NULL;

		count[i] = ((last + 1) - first[i]);
	}

	int idx[array->dimensions];
	for (i = 0; i < array->dimensions; i++)
	{
		idx[i] = first[i] + (offset % count[i]);
		offset /= count[i];
	}

	ofc_sema_array_index_t* index
		= (ofc_sema_array_index_t*)malloc(sizeof(ofc_sema_array_index_t)
				+ (array->dimensions * sizeof(ofc_sema_expr_t*)));
	if (!index) return NULL;

	bool success = true;
	index->dimensions = array->dimensions;
	for (i = 0; i < array->dimensions; i++)
	{
		index->index[i] = ofc_sema_expr_integer(
			idx[i], OFC_SEMA_KIND_DEFAULT);

		if (!index->index[i])
			success = false;
	}

	if (!success)
	{
		ofc_sema_array_index_delete(index);
		return NULL;
	}

	return index;
}

ofc_sema_array_index_t* ofc_sema_array_slice_index_from_offset(
	const ofc_sema_array_slice_t* slice, unsigned offset)
{
	if (!slice) return NULL;

	int      first[slice->dimensions];
	unsigned count[slice->dimensions];

	unsigned i;
	for (i = 0; i < slice->dimensions; i++)
	{
		first[i] = 1;
		if (slice->segment[i].first
			&& !ofc_sema_expr_resolve_int(
				slice->segment[i].first, &first[i]))
			return NULL;

		if(slice->segment[i].is_index)
		{
			count[i] = 0;
			continue;
		}

		int last;
		if (!ofc_sema_expr_resolve_int(
			slice->segment[i].last, &last))
			return NULL;

		if (last < first[i])
			return NULL;

		count[i] = ((last + 1) - first[i]);
	}

	int idx[slice->dimensions];
	for (i = 0; i < slice->dimensions; i++)
	{
		if (count[i] == 0)
		{
			idx[i] = first[i];
			continue;
		}

		idx[i] = first[i] + (offset % count[i]);
		offset /= count[i];

	}

	ofc_sema_array_index_t* index
		= (ofc_sema_array_index_t*)malloc(sizeof(ofc_sema_array_index_t)
				+ (slice->dimensions * sizeof(ofc_sema_expr_t*)));
	if (!index) return NULL;

	bool success = true;
	index->dimensions = slice->dimensions;
	for (i = 0; i < slice->dimensions; i++)
	{
		index->index[i] = ofc_sema_expr_integer(
			idx[i], OFC_SEMA_KIND_DEFAULT);
		if (!index->index[i])
			success = false;
	}

	if (!success)
	{
		ofc_sema_array_index_delete(index);
		return NULL;
	}

	return index;
}

bool ofc_sema_array_index_offset(
	const ofc_sema_decl_t*        decl,
	const ofc_sema_array_index_t* index,
	unsigned* offset)
{
	if (!index || (index->dimensions == 0))
		return false;

	if (!ofc_sema_decl_is_array(decl))
	{
		/* TODO - Positional error. */
		ofc_sparse_ref_error(OFC_SPARSE_REF_EMPTY,
			"Can't index non-array type");
		return false;
	}

	const ofc_sema_array_t* array
		= decl->array;
	if (!array) return false;

	if (index->dimensions
		!= array->dimensions)
	{
		/* TODO - Positional error. */
		ofc_sparse_ref_error(OFC_SPARSE_REF_EMPTY,
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
			ofc_sparse_ref_error(expr->src,
				"Failed to resolve array index");
			return false;
		}

		int first = 1;
		if (dims.first && !ofc_sema_expr_resolve_int(
			dims.first, &first))
			return false;

		int last;
		if (!ofc_sema_expr_resolve_int(
			dims.last, &last))
			return false;

		if (so < first)
		{
			ofc_sparse_ref_error(expr->src,
				"Array index out-of-range, too low");
			return false;
		}
		else if (so > last)
		{
			ofc_sparse_ref_error(expr->src,
				"Array index out-of-range, too high");
			return false;
		}

		o += ((so - first) * s);
		s *= ((last - first) + 1);
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

	if (array->dimensions < index->count)
	{
		ofc_sparse_ref_error(index->src,
			"Too many dimensions in array slice");
		return NULL;
	}
	else if (array->dimensions != index->count)
	{
		ofc_sparse_ref_warning(index->src,
			"Non-standard array slice syntax");
	}

	ofc_sema_array_slice_t* slice
		= (ofc_sema_array_slice_t*)malloc(sizeof(ofc_sema_array_slice_t)
			+ (array->dimensions * sizeof(ofc_sema_array_segment_t)));
	if (!slice) return NULL;

	slice->dimensions = array->dimensions;

	unsigned i;
	for (i = 0; i < array->dimensions; i++)
	{
		slice->segment[i].first  = NULL;
		slice->segment[i].last   = NULL;
		slice->segment[i].stride = NULL;
		slice->segment[i].is_index = false;
	}

	for (i = 0; i < index->count; i++)
	{
		const ofc_parse_array_range_t* range
			= index->range[i];
		if (!range)
		{
			ofc_sema_array_slice_delete(slice);
			return NULL;
		}

		int sfirst;
		bool resolved_first = false;
		if (range->first)
		{
			slice->segment[i].first
				= ofc_sema_expr(scope, range->first);
			if (!slice->segment[i].first)
			{
				ofc_sema_array_slice_delete(slice);
				return NULL;
			}

			if (ofc_sema_expr_resolve_int(
					slice->segment[i].first, &sfirst))
			{
				resolved_first = true;

				int afirst;
				if (ofc_sema_expr_resolve_int(
					array->segment[i].first, &afirst)
					&& (sfirst < afirst))
				{
					ofc_sparse_ref_error(range->first->src,
						"Array slice lower bound underflow");
					ofc_sema_array_slice_delete(slice);
					return NULL;
				}

				int alast;
				if (ofc_sema_expr_resolve_int(
					array->segment[i].last, &alast)
					&& (sfirst > alast))
				{
					ofc_sparse_ref_error(range->first->src,
						"Array slice lower bound overflow");
					ofc_sema_array_slice_delete(slice);
					return NULL;
				}
			}
		}

		if (range->last)
		{
			slice->segment[i].last
				= ofc_sema_expr(scope, range->last);
			if (!slice->segment[i].last)
			{
				ofc_sema_array_slice_delete(slice);
				return NULL;
			}

			int slast;
			if (ofc_sema_expr_resolve_int(
					slice->segment[i].last, &slast))
			{
				if (resolved_first && (sfirst > slast))
				{
					ofc_sparse_ref_error(range->last->src,
						"Array slice upper bound smaller than lower bound");
					ofc_sema_array_slice_delete(slice);
					return NULL;
				}

				int afirst;
				if (ofc_sema_expr_resolve_int(
					array->segment[i].first, &afirst)
					&& (slast < afirst))
				{
					ofc_sparse_ref_error(range->last->src,
						"Array slice upper bound underflow");
					ofc_sema_array_slice_delete(slice);
					return NULL;
				}

				int alast;
				if (ofc_sema_expr_resolve_int(
					array->segment[i].last, &alast)
					&& (slast > alast))
				{
					ofc_sparse_ref_error(range->last->src,
						"Array slice upper bound overflow");
					ofc_sema_array_slice_delete(slice);
					return NULL;
				}
			}
		}

		if (range->stride)
		{
			slice->segment[i].stride
				= ofc_sema_expr(scope, range->stride);
			if (!slice->segment[i].stride)
			{
				ofc_sema_array_slice_delete(slice);
				return NULL;
			}

			int stride;
			if (ofc_sema_expr_resolve_int(
				slice->segment[i].stride, &stride)
				&& (stride == 0))
			{
				ofc_sparse_ref_error(range->stride->src,
					"Stride can't be zero");
				ofc_sema_array_slice_delete(slice);
				return NULL;
			}
		}

		slice->segment[i].is_index = (slice->segment[i].first
			&& !slice->segment[i].last && !slice->segment[i].stride
			&& !range->is_slice);
	}

	return slice;
}

ofc_sema_array_slice_t* ofc_sema_array_slice_copy_replace(
	const ofc_sema_array_slice_t* slice,
	const ofc_sema_decl_t*        replace,
	const ofc_sema_expr_t*        with)
{
	if (!slice)
		return NULL;

	ofc_sema_array_slice_t* copy
		= (ofc_sema_array_slice_t*)malloc(
			sizeof(ofc_sema_array_slice_t)
			+ (slice->dimensions * sizeof(ofc_sema_array_segment_t)));
	if (!copy) return NULL;

	copy->dimensions = slice->dimensions;

	bool success = true;
	unsigned i;
	for (i = 0; i < slice->dimensions; i++)
	{
		copy->segment[i].first = NULL;
		if (slice->segment[i].first)
		{
			copy->segment[i].first
				= ofc_sema_expr_copy_replace(
					slice->segment[i].first, replace, with);
			if (!copy->segment[i].first)
				success = false;
		}

		copy->segment[i].last = NULL;
		if (slice->segment[i].last)
		{
			copy->segment[i].last
				= ofc_sema_expr_copy_replace(
					slice->segment[i].last, replace, with);
			if (!copy->segment[i].last)
				success = false;
		}

		copy->segment[i].stride = NULL;
		if (slice->segment[i].stride)
		{
			copy->segment[i].stride
				= ofc_sema_expr_copy_replace(
					slice->segment[i].stride, replace, with);
			if (!copy->segment[i].stride)
				success = false;
		}

		copy->segment[i].is_index
			= slice->segment[i].is_index;
	}

	if (!success)
	{
		ofc_sema_array_slice_delete(copy);
		return NULL;
	}

	return copy;
}

ofc_sema_array_slice_t* ofc_sema_array_slice_copy(
	const ofc_sema_array_slice_t* slice)
{
	return ofc_sema_array_slice_copy_replace(
		slice, NULL, NULL);
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
			slice->segment[i].first);
		ofc_sema_expr_delete(
			slice->segment[i].last);
		ofc_sema_expr_delete(
			slice->segment[i].stride);
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
		if (!ofc_sema_expr_compare(
			a->segment[i].first,
			b->segment[i].first))
			return false;

		if ((a->segment[i].last
			|| b->segment[i].last)
				&& !ofc_sema_expr_compare(
					a->segment[i].last,
					b->segment[i].last))
			return false;

		if (!ofc_sema_expr_compare_def_one(
			a->segment[i].stride, b->segment[i].stride))
			return false;
	}

	return true;
}

ofc_sema_array_t* ofc_sema_array_slice_dims(
	const ofc_sema_array_slice_t* slice,
	const ofc_sema_array_t* array)
{
	if (!slice || !array)
		return NULL;

	unsigned i, d = array->dimensions;
	for (i = 0; i < slice->dimensions; i++)
	{
		if (slice->segment[i].is_index)
			d--;
	}

	if (d == 0)
		return NULL;

	ofc_sema_array_t* dims
		= (ofc_sema_array_t*)malloc(sizeof(ofc_sema_array_t)
			+ (d * sizeof(ofc_sema_array_dims_t)));
	if (!dims) return NULL;

	dims->dimensions = d;

	bool fail = false;
	unsigned j;
	for (i = 0, j = 0; i < slice->dimensions; i++)
	{
		if (slice->segment[i].is_index)
			continue;

		ofc_sema_expr_t* first;
		if (slice->segment[i].first)
			first = ofc_sema_expr_copy(
				slice->segment[i].first);
		else if (array->segment[i].first)
			first = ofc_sema_expr_copy(
				array->segment[i].first);
		else
			first = ofc_sema_expr_integer(
				1, OFC_SEMA_KIND_DEFAULT);

		ofc_sema_expr_t* last;
		if (slice->segment[i].last)
			last = slice->segment[i].last;
		else if (array->segment[i].last)
			last = array->segment[i].last;
		else
		{
			ofc_sema_expr_delete(first);
			ofc_sema_array_delete(dims);
			return NULL;
		}

		ofc_sema_expr_t* expr_sub
			= ofc_sema_expr_sub(last, first);
		ofc_sema_expr_delete(first);

		ofc_sema_expr_t* unity
			= ofc_sema_expr_integer(
				1, OFC_SEMA_KIND_DEFAULT);

		/* last - first + 1 */
		ofc_sema_expr_t* expr
			= ofc_sema_expr_add(
				expr_sub, unity);
		ofc_sema_expr_delete(expr_sub);
		if (!expr)
		{
			ofc_sema_expr_delete(unity);
			ofc_sema_array_delete(dims);
			return NULL;
		}

		dims->segment[j].last = expr;

		/* ((last - first + 1) / stride) round up
		   ceiling(a/b) = ((a + b - 1) / b) */
		if (slice->segment[i].stride)
		{
			ofc_sema_expr_t* expr_stride_sub
				= ofc_sema_expr_sub(
					slice->segment[i].stride, unity);
			ofc_sema_expr_t* expr_stride_add
				= ofc_sema_expr_add(expr, expr_stride_sub);

			ofc_sema_expr_t* expr_stride
				= ofc_sema_expr_div(expr_stride_add,
					slice->segment[i].stride);

			ofc_sema_expr_delete(expr);
			ofc_sema_expr_delete(expr_stride_sub);
			ofc_sema_expr_delete(expr_stride_add);

			if (!expr_stride)
			{
				ofc_sema_expr_delete(unity);
				ofc_sema_array_delete(dims);
				return NULL;
			}

			dims->segment[j].last = expr_stride;
		}

		ofc_sema_expr_delete(unity);

		dims->segment[j].first
			= ofc_sema_expr_integer(
				1, OFC_SEMA_KIND_DEFAULT);

		j++;
	}

	for (; i < array->dimensions; i++, j++)
	{
		dims->segment[j].first = ofc_sema_expr_copy(
			array->segment[i].first);
		if (array->segment[i].first
			&& !dims->segment[j].first)
			fail = true;

		dims->segment[j].last = ofc_sema_expr_copy(
			array->segment[i].last);
		if (array->segment[i].last
			&& !dims->segment[j].last)
			fail = true;
	}

	if (fail)
	{
		ofc_sema_array_delete(dims);
		return NULL;
	}

	return dims;
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
		if (i > 0)
		{
			if (!ofc_colstr_atomic_writef(cs, ",")
				|| !ofc_colstr_atomic_writef(cs, " "))
				return false;
		}

		ofc_sema_array_segment_t seg
			= slice->segment[i];
		if (seg.first && !ofc_sema_expr_print(cs, seg.first))
			return false;

		if (seg.last || !seg.is_index)
		{
			if (!ofc_colstr_atomic_writef(cs, ":"))
				return NULL;
			if (seg.last && !ofc_sema_expr_print(cs, seg.last))
				return NULL;

			if (seg.stride)
			{
				if (!ofc_colstr_atomic_writef(cs, ":")
					|| !ofc_sema_expr_print(cs, seg.stride))
					return NULL;
			}
		}
	}

	return ofc_colstr_atomic_writef(cs, ")");
}
