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


ofc_parse_assign_t* ofc_parse_assign(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i;
	ofc_parse_lhs_t* name
		= ofc_parse_lhs(src, ptr, debug, &i);
	if (!name) return NULL;

	ofc_parse_expr_t* init = NULL;
	if (ptr[i] == '=')
	{
		i += 1;

		unsigned l;
		init = ofc_parse_expr(
			src, &ptr[i], debug, &l);
		if (!init)
		{
			ofc_parse_lhs_delete(name);
			ofc_parse_debug_rewind(debug, dpos);
			return NULL;
		}
		i += l;
	}

	ofc_parse_assign_t* assign
		= (ofc_parse_assign_t*)malloc(
			sizeof(ofc_parse_assign_t));
	if (!assign)
	{
		ofc_parse_expr_delete(init);
		ofc_parse_lhs_delete(name);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}

	assign->src  = ofc_sparse_ref(src, ptr, i);
	assign->name = name;
	assign->init = init;

	if (len) *len = i;
	return assign;
}

/* TODO - Make this a flag to a static function for speed. */
ofc_parse_assign_t* ofc_parse_assign_init(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i;
	ofc_parse_assign_t* assign
		= ofc_parse_assign(src, ptr, debug, &i);
	if (!assign) return NULL;

	if (!assign->init)
	{
		ofc_parse_assign_delete(assign);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}

	if (len) *len = i;
	return assign;
}

ofc_parse_assign_t* ofc_parse_assign_copy(
	const ofc_parse_assign_t* assign)
{
	if (!assign)
		return NULL;

	ofc_parse_assign_t* copy
		= (ofc_parse_assign_t*)malloc(
			sizeof(ofc_parse_assign_t));
	if (!copy) return NULL;

	copy->name = ofc_parse_lhs_copy(assign->name);
	copy->init = ofc_parse_expr_copy(assign->init);

	if (!copy->name
		|| (assign->init && !copy->init))
	{
		ofc_parse_assign_delete(copy);
		return NULL;
	}

	return copy;
}

void ofc_parse_assign_delete(
	ofc_parse_assign_t* assign)
{
	if (!assign)
		return;

	ofc_parse_expr_delete(assign->init);
	ofc_parse_lhs_delete(assign->name);
	free(assign);
}

bool ofc_parse_assign_print(
	ofc_colstr_t* cs, const ofc_parse_assign_t* assign)
{
	if (!assign)
		return false;

	if (!ofc_parse_lhs_print(
		cs, assign->name, false))
		return false;

	if (assign->init)
	{
		if (!ofc_colstr_atomic_writef(cs, " = ")
			|| !ofc_parse_expr_print(cs, assign->init))
			return false;
	}

	return true;
}



ofc_parse_assign_list_t* ofc_parse_assign_list(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	ofc_parse_assign_list_t* list
		= (ofc_parse_assign_list_t*)malloc(
			sizeof(ofc_parse_assign_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->assign = NULL;

	unsigned i = ofc_parse_list(
		src, ptr, debug, ',',
		&list->count, (void***)&list->assign,
		(void*)ofc_parse_assign,
		(void*)ofc_parse_assign_delete);
	if (i == 0) return NULL;

	if (len) *len = i;
	return list;
}

ofc_parse_assign_list_t* ofc_parse_assign_list_copy(
	const ofc_parse_assign_list_t* list)
{
	if (!list)
		return NULL;

	ofc_parse_assign_list_t* copy
		= (ofc_parse_assign_list_t*)malloc(
			sizeof(ofc_parse_assign_list_t));
	if (!copy) return NULL;

	copy->count = 0;
	copy->assign = NULL;

	if (!ofc_parse_list_copy(
		&copy->count, (void***)&copy->assign,
		list->count, (const void**)list->assign,
		(void*)ofc_parse_assign_copy,
		(void*)ofc_parse_assign_delete))
	{
		free(copy);
		return NULL;
	}

	return copy;
}

void ofc_parse_assign_list_delete(
	ofc_parse_assign_list_t* list)
{
	if (!list)
		return;

	ofc_parse_list_delete(
		list->count, (void**)list->assign,
		(void*)ofc_parse_assign_delete);

	free(list);
}

bool ofc_parse_assign_list_print(
	ofc_colstr_t* cs, const ofc_parse_assign_list_t* list)
{
	if (!list)
		return false;

	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		if ((i > 0) && (!ofc_colstr_atomic_writef(cs, ", ")))
			return false;

		if (!ofc_parse_assign_print(
			cs, list->assign[i]))
			return false;
	}

	return (i > 0);
}
