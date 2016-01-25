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
		array->segment[i].first = NULL;
		array->segment[i].last  = NULL;
	}

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
				ofc_sema_expr_delete(seg->first);
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
				ofc_sema_expr_delete(seg->last);
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

	bool fail = false;
	unsigned i;
	for (i = 0; i < copy->dimensions; i++)
	{
		copy->segment[i].first
			= ofc_sema_expr_copy(
				array->segment[i].first);
		copy->segment[i].last
			= ofc_sema_expr_copy(
				array->segment[i].last);

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

void ofc_sema_array_delete(
	ofc_sema_array_t* array)
{
	if (!array)
		return;

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

		if (dims.first
			&& !ofc_sema_expr_print(
				cs, dims.first))
			return false;

		if ((dims.first || !dims.last)
			&& !ofc_colstr_atomic_writef(cs, ":"))
			return false;

		if (dims.last && !ofc_sema_expr_print(
			cs, dims.last))
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
		index->index[i] = ofc_sema_expr_integer(idx[i]);

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

	bool fail = false;
	unsigned j;
	for (i = 0, j = 0; i < slice->dimensions; i++)
	{
		if (slice->segment[i].index)
			continue;

		array->segment[j].first = NULL;
		if (slice->segment[i].base != 1)
		{
			array->segment[j].first
				= ofc_sema_expr_integer(
					slice->segment[i].base);
			if (!array->segment[j].first)
				fail = true;
		}

		array->segment[j].last
			= ofc_sema_expr_integer(
				(slice->segment[i].base + slice->segment[i].count) - 1);
		if (!array->segment[j].last)
			fail = true;

		j++;
	}

	if (fail)
	{
		ofc_sema_array_delete(array);
		return NULL;
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
