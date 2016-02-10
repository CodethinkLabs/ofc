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

#include "ofc/parse.h"


static ofc_parse_array_range_t* ofc_parse_array__range(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = 0;
	ofc_parse_expr_t* first  = ofc_parse_expr(src, ptr, debug, &i);
	ofc_parse_expr_t* last   = NULL;
	ofc_parse_expr_t* stride = NULL;

	bool is_slice = false;
	if (!first && (ptr[0] == '*'))
	{
		is_slice = true;
		i += 1;
	}

	if (ptr[i] == ':')
	{
		is_slice = true;
		i += 1;

		unsigned l;
		last = ofc_parse_expr(src, &ptr[i], debug, &l);
		if (last)
		{
			i += l;
		}
		else if (ptr[i] == '*')
		{
			i += 1;
		}

		if (ptr[i] == ':')
		{
			i += 1;

			stride = ofc_parse_expr(src, &ptr[i], debug, &l);
			if (stride)
			{
				i += l;
			}
			else if (ptr[i] == '*')
			{
				i += 1;
			}
		}
	}
	else if (!first && !is_slice)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}

	ofc_parse_array_range_t* range
		= (ofc_parse_array_range_t*)malloc(
			sizeof(ofc_parse_array_range_t));
	if (!range)
	{
		ofc_parse_expr_delete(first);
		ofc_parse_expr_delete(last);
		ofc_parse_expr_delete(stride);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}

	range->src = ofc_sparse_ref(src, ptr, i);

	range->is_slice = is_slice;
	range->first    = first;
	range->last     = last;
	range->stride   = stride;

	if (len) *len = i;
	return range;
}

static void ofc_parse_array__range_delete(
	ofc_parse_array_range_t* range)
{
	if (!range)
		return;

	ofc_parse_expr_delete(range->first);
	ofc_parse_expr_delete(range->last);
	ofc_parse_expr_delete(range->stride);
	free(range);
}

static ofc_parse_array_range_t* ofc_parse_array__range_copy(
	const ofc_parse_array_range_t* range)
{
	if (!range)
		return NULL;

	ofc_parse_array_range_t* copy
		= (ofc_parse_array_range_t*)malloc(
			sizeof(ofc_parse_array_range_t));
	if (!copy) return NULL;

	copy->is_slice = range->is_slice;
	copy->first    = ofc_parse_expr_copy(range->first);
	copy->last     = ofc_parse_expr_copy(range->last);
	copy->stride   = ofc_parse_expr_copy(range->stride);

	if ((range->first && !copy->first)
		|| (range->last && !copy->last)
		|| (range->stride && !copy->stride))
	{
		ofc_parse_array__range_delete(copy);
		return NULL;
	}

	return copy;
}

static bool ofc_parse_array__range_print(
	ofc_colstr_t* cs, const ofc_parse_array_range_t* range,
	bool is_decl)
{
	if (!range)
		return false;

	if (range->first)
	{
		if (!ofc_parse_expr_print(
			cs, range->first))
			return false;
	}

	if (range->is_slice)
	{
		if (!ofc_colstr_atomic_writef(cs, ":"))
			return false;

		if (range->last)
		{
			if (!ofc_parse_expr_print(
				cs, range->last))
				return false;
		}
		else if (range->first && is_decl)
		{
			if (!ofc_colstr_atomic_writef(cs, "*"))
				return false;
		}

		if (range->stride)
		{
			if (!ofc_colstr_atomic_writef(cs, ":")
				|| !ofc_parse_expr_print(
					cs, range->stride))
				return false;
		}
	}
	else if (!range->first)
	{
		/* Invalid array index. */
		return false;
	}

	return true;
}



ofc_parse_array_index_t* ofc_parse_array_index(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	unsigned i = 0;
	if (ptr[i++] != '(')
		return NULL;

	ofc_parse_array_index_t* index
		= (ofc_parse_array_index_t*)malloc(
			sizeof(ofc_parse_array_index_t));
	if (!index) return NULL;

	index->count = 0;
	index->range = NULL;

	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned l = ofc_parse_list(
		src, &ptr[i], debug, ',',
		&index->count, (void***)&index->range,
		(void*)ofc_parse_array__range,
		(void*)ofc_parse_array__range_delete);
	if (l == 0)
	{
		free(index);
		return NULL;
	}
	i += l;

	if (ptr[i++] != ')')
	{
		ofc_parse_list_delete(
			index->count, (void**)index->range,
			(void*)ofc_parse_array__range_delete);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}

	index->src = ofc_sparse_ref(src, ptr, i);
	if (len) *len = i;
	return index;
}

ofc_parse_array_index_t* ofc_parse_array_index_copy(
	const ofc_parse_array_index_t* index)
{
	if (!index)
		return NULL;

	ofc_parse_array_index_t* copy
		= (ofc_parse_array_index_t*)malloc(
			sizeof(ofc_parse_array_index_t));
	if (!copy) return NULL;

	copy->count = 0;
	copy->range = NULL;

	if (!ofc_parse_list_copy(
		&copy->count, (void***)&copy->range,
		index->count, (const void**)index->range,
		(void*)ofc_parse_array__range_copy,
		(void*)ofc_parse_array__range_delete))
	{
		free(copy);
		return NULL;
	}

	copy->src = index->src;
	return copy;
}

void ofc_parse_array_index_delete(
	ofc_parse_array_index_t* index)
{
	if (!index)
		return;

	ofc_parse_list_delete(
		index->count, (void**)index->range,
		(void*)ofc_parse_array__range_delete);
	free(index);
}

bool ofc_parse_array_index_print(
	ofc_colstr_t* cs, const ofc_parse_array_index_t* index,
	bool is_decl)
{
	if (!index)
		return false;

	if (!ofc_colstr_atomic_writef(cs, "("))
		return false;

	unsigned i;
	for (i = 0; i < index->count; i++)
	{
		if ((i > 0) && (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " ")))
			return false;

		if (!ofc_parse_array__range_print(
			cs, index->range[i], is_decl))
			return false;
	}

	if (!ofc_colstr_atomic_writef(cs, ")"))
		return false;

	return true;
}
