/* Copyright 2016 Codethink Ltd.
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

void ofc_sema_range_delete(
	ofc_sema_range_t* range)
{
	if (!range)
		return;

	ofc_sema_expr_delete(range->first);
	ofc_sema_expr_delete(range->last);
	free(range);
}

void ofc_sema_range_list_delete(
	ofc_sema_range_list_t* list)
{
	if (!list)
		return;

	unsigned i;
	for (i = 0; i < list->count; i++)
		ofc_sema_range_delete(list->range[i]);

	free(list->range);
	free(list);
}

ofc_sema_range_t* ofc_sema_range(
	ofc_sema_scope_t* scope,
	ofc_parse_array_range_t* array_range)
{
	if (!array_range) return NULL;

    ofc_sema_range_t* range
		= (ofc_sema_range_t*)malloc(
			sizeof(ofc_sema_range_t));

	range->src = array_range->src;
	range->is_range = array_range->is_slice;
	range->first = NULL;
	range->last  = NULL;

	range->first = ofc_sema_expr(scope, array_range->first);
	if (array_range->first && !range->first)
	{
		ofc_sema_range_delete(range);
		return NULL;
	}

	range->last  = ofc_sema_expr(scope, array_range->last);
	if (array_range->last && !range->last)
	{
		ofc_sema_range_delete(range);
		return NULL;
	}

	if (array_range->stride)
	{
		ofc_sparse_ref_error(array_range->stride->src,
			"Range can't have stride");
		ofc_sema_range_delete(range);
		return NULL;
	}

	return range;
}

ofc_sema_range_list_t* ofc_sema_range_list(
	ofc_sema_scope_t* scope,
	ofc_parse_array_index_t* index)
{
	if (!index) return NULL;

	ofc_sema_range_list_t* list
		= (ofc_sema_range_list_t*)malloc(
			sizeof(ofc_sema_range_list_t));

	list->count = 0;
	list->range = NULL;
	list->src   = index->src;

	unsigned i;
	for (i = 0; i < index->count; i++)
	{
		ofc_sema_range_t* nrange
			= ofc_sema_range(
				scope, index->range[i]);
		if (!ofc_sema_range_list_add(list, nrange))
		{
			ofc_sema_range_list_delete(list);
			return false;
		}
	}

	return list;
}

bool ofc_sema_range_list_add(
	ofc_sema_range_list_t* list,
	ofc_sema_range_t* range)
{
	if (!list || !range)
		return false;

	ofc_sema_range_t** nrange
		= (ofc_sema_range_t**)realloc(list->range,
			(sizeof(ofc_sema_range_t*) * (list->count + 1)));
	if (!nrange) return NULL;

	list->range = nrange;
	list->range[list->count++] = range;
	return true;
}

bool ofc_sema_range_is_constant(
	ofc_sema_range_t* range)
{
	if (!range) return false;

	return (ofc_sema_expr_is_constant(range->first)
		&& ofc_sema_expr_is_constant(range->last));
}

bool ofc_sema_range_intersects(
	ofc_sema_range_t* a,
	ofc_sema_range_t* b)
{
	if (!a || !b)
		return false;
	/* All this is wrong, need to change typeval to logic and delete the typvals */
	ofc_sema_range_t* range[2];
	range[0] = a;
	range[1] = b;

	const ofc_sema_typeval_t* first[2];
	first[0] = ofc_sema_expr_constant(range[0]->first);
	first[1] = ofc_sema_expr_constant(range[1]->first);

	const ofc_sema_typeval_t* last[2];
	last[0] = ofc_sema_expr_constant(range[0]->last);
	last[1] = ofc_sema_expr_constant(range[1]->last);

	ofc_sema_typeval_t* val[6];
	val[0] = ofc_sema_typeval_le(first[1], first[0]); /* F1 <= F0 */
	val[1] = ofc_sema_typeval_le(first[0], last[1]);  /* F0 <= L1 */
	val[2] = ofc_sema_typeval_le(first[0], first[1]); /* F0 <= F1 */
	val[3] = ofc_sema_typeval_le(first[1], last[0]);  /* F1 <= L0 */
	val[4] = ofc_sema_typeval_le(last[1], last[0]);   /* L1 <= L0 */
	val[5] = ofc_sema_typeval_eq(first[0], first[1]); /* F0 == F1 */

	bool ret = false;
	if (!range[0]->is_range && !range[1]->is_range)
	{
		/* F0 == F1 */
		ret = val[5]->logical;
	}
	else if (!range[0]->is_range)
	{
		/* CASE: F0, [F1,L1] */
		if (first[1]  && last[1])
			ret = val[0]->logical && val[1]->logical; /* F1 <= F0 <= L1 */
		/* CASE: F0, [F1, inf) */
		else if (first[1])
			ret = val[0]->logical; /* F1 <= F0 */
		/* CASE: F0, (-inf,L1] */
		else
			ret = val[1]->logical; /* F0 <= L1 */
	}
	else if (!range[1]->is_range)
	{
		/* CASE: [F0,L0], F1 */
		if (first[0]  && last[0])
			ret = val[2]->logical && val[3]->logical; /* F0 <= F1 <= L0 */
		/* CASE: [F0,inf), F1 */
		else if (first[0])
			ret = val[2]->logical; /* F0 <= F1 */
		/* CASE: (-inf,L0], F1 */
		else
			ret = val[3]->logical; /* F1 <= L0 */
	}
	else
	{
		/* [F0,inf) , [F1, inf) */
		/* (-inf,L0], (-inf,L1] */
		if ((first[0] && !last[0] && first[1] && !last[1])
			|| (!first[0] && last[0] && !first[1] && last[1]))
		{
			/* Always intersect */
			ret = true;
		}
		/* [F0,inf), (-inf,L1] */
		/* [F0,inf), [F1,L1]   */
		/* [F0,L0] , (-inf,L1] */
		else if ((first[0] && !last[0] && !first[1] && last[1])
			|| (first[0] && !last[0] && first[1] && last[1])
			|| (first[0] && last[0] && !first[1] && last[1]))
		{
			ret = val[1]->logical; /* iff F0 <= L1 */
		}
		/* (-inf,L0], [F1, inf) */
		/* (-inf,L0], [F1,L1]   */
		/* [F0,L0]  , [F1, inf) */
		else if ((!first[0] && last[0] && first[1] && !last[1])
			|| (!first[0] && last[0] && first[1] && last[1])
			|| (first[0] && last[0] && first[1] && !last[1]))
		{
			ret = val[3]->logical; /* iff F1 <= L0 */
		}
		/* [F0,L0], [F1,L1]   */
		else
		{
			/* iff (F0 <= F1 <= L0) or (F0 <= L1 <= L0) or (F1 <= F0 <= L1) */
			ret = ((val[2]->logical && val[3]->logical)
				|| (val[1]->logical && val[4]->logical)
				|| (val[0]->logical && val[1]->logical));
		}
	}

	unsigned i;
	for (i = 0; i < 6; i++)
	{
		ofc_sema_typeval_delete(val[i]);
	}

	return ret;
}

bool ofc_sema_range_list_intersects(
	ofc_sema_range_list_t* a,
	ofc_sema_range_list_t* b)
{
	if (!a || !b)
		return false;

	unsigned i, j;
	for (i = 0; i < a->count; i++)
	{
		for (j = 0; j < b->count; j++)
		{
			if (ofc_sema_range_intersects(
				a->range[i], b->range[j]))
				return true;
		}
	}

	return false;
}

bool ofc_sema_range_print(
	ofc_colstr_t* cs,
	const ofc_sema_range_t* range)
{
	if (!cs || !range) return false;

	if (range->first && !ofc_sema_expr_print(cs, range->first))
		return false;
	if (range->is_range && !ofc_colstr_atomic_writef(cs, ":"))
		return false;
	if (range->last && !ofc_sema_expr_print(cs, range->last))
		return false;

	return true;
}

bool ofc_sema_range_list_print(
	ofc_colstr_t* cs,
	const ofc_sema_range_list_t* range)
{
	if (!cs || !range) return false;

	if (!ofc_colstr_atomic_writef(cs, "("))
		return false;

	unsigned i;
	for (i = 0; i < range->count; i++)
	{
		if (i > 0)
		{
			if (!ofc_colstr_atomic_writef(cs, ",")
				|| !ofc_colstr_atomic_writef(cs, " "))
				return false;
		}

		if (!ofc_sema_range_print(cs, range->range[i]))
			return false;
	}

	return ofc_colstr_atomic_writef(cs, ")");
}
