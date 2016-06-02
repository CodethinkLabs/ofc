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


bool ofc_parse__implicit_do_iter(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug, unsigned* len,
	ofc_parse_expr_t** iter, ofc_parse_expr_t** init,
	ofc_parse_expr_t** limit, ofc_parse_expr_t** step)
{
	unsigned i = 0;

	unsigned l;
	/* Are we sure it's not in the list already? */
	*iter = ofc_parse_expr(
		src, &ptr[i], debug, &l);
	if (!iter) return false;
	i += l;

	if (ptr[i++] != '=')
		return false;

	*init = ofc_parse_expr(
		src, &ptr[i], debug, &l);
	if (!init) return false;
	i += l;

	if (ptr[i++] != ',')
		return false;

	*limit = ofc_parse_expr(
		src, &ptr[i], debug, &l);
	if (!limit) return false;
	i += l;

	if (ptr[i] == ',')
	{
		i += 1;

		*step = ofc_parse_expr(
			src, &ptr[i], debug, &l);
		if (!step) return false;
		i += l;
	}

	if (len) *len = i;
	return true;
}

ofc_parse_expr_implicit_do_t* ofc_parse_expr_implicit_do(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	unsigned i = 0;

	if (ptr[i++] != '(')
		return NULL;

	ofc_parse_expr_implicit_do_t* id
		= (ofc_parse_expr_implicit_do_t*)malloc(
			sizeof(ofc_parse_expr_implicit_do_t));
	if (!id) return NULL;

	id->dlist = NULL;
	id->iter  = NULL;
	id->init  = NULL;
	id->limit = NULL;
	id->step  = NULL;

	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned l;
	id->dlist = ofc_parse_expr_list(
		src, &ptr[i], debug, &l);
	if (!id->dlist)
	{
		free(id);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}
	i += l;

	if (ptr[i++] != ',')
	{
		ofc_parse_expr_implicit_do_delete(id);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}

	if (!ofc_parse__implicit_do_iter(src, &ptr[i], debug, &l,
		&id->iter, &id->init, &id->limit, &id->step))
	{
		ofc_parse_expr_implicit_do_delete(id);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}
	i += l;


	if (ptr[i++] != ')')
	{
		ofc_parse_expr_implicit_do_delete(id);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}

	if (len) *len = i;
	return id;
}

ofc_parse_lhs_implicit_do_t* ofc_parse_lhs_implicit_do(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	unsigned i = 0;

	if (ptr[i++] != '(')
		return NULL;

	ofc_parse_lhs_implicit_do_t* id
		= (ofc_parse_lhs_implicit_do_t*)malloc(
			sizeof(ofc_parse_lhs_implicit_do_t));
	if (!id) return NULL;

	id->dlist = NULL;
	id->iter  = NULL;
	id->init  = NULL;
	id->limit = NULL;
	id->step  = NULL;

	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned l;
	id->dlist = ofc_parse_lhs_list(
		src, &ptr[i], debug, &l);
	if (!id->dlist)
	{
		free(id);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}
	i += l;

	if (ptr[i++] != ',')
	{
		ofc_parse_lhs_implicit_do_delete(id);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}

	if (!ofc_parse__implicit_do_iter(src, &ptr[i], debug, &l,
		&id->iter, &id->init, &id->limit, &id->step))
	{
		ofc_parse_lhs_implicit_do_delete(id);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}
	i += l;

	if (ptr[i++] != ')')
	{
		ofc_parse_lhs_implicit_do_delete(id);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}

	if (len) *len = i;
	return id;
}

ofc_parse_expr_implicit_do_t* ofc_parse_expr_implicit_do_copy(
	ofc_parse_expr_implicit_do_t* id)
{
	if (!id)
		return NULL;

	ofc_parse_expr_implicit_do_t* copy
		= (ofc_parse_expr_implicit_do_t*)malloc(
			sizeof(ofc_parse_expr_implicit_do_t));
	if (!copy) return NULL;

	copy->dlist = ofc_parse_expr_list_copy(id->dlist);
	copy->iter  = ofc_parse_expr_copy(id->iter);
	copy->init  = ofc_parse_expr_copy(id->init);
	copy->limit = ofc_parse_expr_copy(id->limit);
	copy->step  = ofc_parse_expr_copy(id->step);

	if (!copy->dlist
		|| !copy->init || !copy->limit
		|| (id->step && !copy->step))
	{
		ofc_parse_expr_implicit_do_delete(copy);
		return NULL;
	}

	return copy;
}

ofc_parse_lhs_implicit_do_t* ofc_parse_lhs_implicit_do_copy(
	ofc_parse_lhs_implicit_do_t* id)
{
	if (!id)
		return NULL;

	ofc_parse_lhs_implicit_do_t* copy
		= (ofc_parse_lhs_implicit_do_t*)malloc(
			sizeof(ofc_parse_lhs_implicit_do_t));
	if (!copy) return NULL;

	copy->dlist = ofc_parse_lhs_list_copy(id->dlist);
	copy->iter  = ofc_parse_expr_copy(id->iter);
	copy->init  = ofc_parse_expr_copy(id->init);
	copy->limit = ofc_parse_expr_copy(id->limit);
	copy->step  = ofc_parse_expr_copy(id->step);

	if (!copy->dlist
		|| !copy->init || !copy->limit
		|| (id->step && !copy->step))
	{
		ofc_parse_lhs_implicit_do_delete(copy);
		return NULL;
	}

	return copy;
}

void ofc_parse_expr_implicit_do_delete(
	ofc_parse_expr_implicit_do_t* id)
{
	if (!id)
		return;

	ofc_parse_expr_list_delete(id->dlist);
	ofc_parse_expr_delete(id->iter);
	ofc_parse_expr_delete(id->init);
	ofc_parse_expr_delete(id->limit);
	ofc_parse_expr_delete(id->step);
	free(id);
}

void ofc_parse_lhs_implicit_do_delete(
	ofc_parse_lhs_implicit_do_t* id)
{
	if (!id)
		return;

	ofc_parse_lhs_list_delete(id->dlist);
	ofc_parse_expr_delete(id->iter);
	ofc_parse_expr_delete(id->init);
	ofc_parse_expr_delete(id->limit);
	ofc_parse_expr_delete(id->step);
	free(id);
}

bool ofc_parse_expr_implicit_do_print(
	ofc_colstr_t* cs, const ofc_parse_expr_implicit_do_t* id)
{
	if (!ofc_colstr_atomic_writef(cs, "(")
		|| !ofc_parse_expr_list_print(cs, id->dlist)
		|| !ofc_colstr_atomic_writef(cs, ", ")
		|| !ofc_parse_expr_print(cs, id->iter)
		|| !ofc_colstr_atomic_writef(cs, " = ")
		|| !ofc_parse_expr_print(cs, id->init)
		|| !ofc_colstr_atomic_writef(cs, ", ")
		|| !ofc_parse_expr_print(cs, id->limit))
		return false;

	if (id->step)
	{
		if (!ofc_colstr_atomic_writef(cs, ", ")
			|| !ofc_parse_expr_print(cs, id->step))
			return false;
	}

	return ofc_colstr_atomic_writef(cs, ")");
}

bool ofc_parse_lhs_implicit_do_print(
	ofc_colstr_t* cs, const ofc_parse_lhs_implicit_do_t* id)
{
	if (!ofc_colstr_atomic_writef(cs, "(")
		|| !ofc_parse_lhs_list_print(cs, id->dlist, false)
		|| !ofc_colstr_atomic_writef(cs, ", ")
		|| !ofc_parse_expr_print(cs, id->iter)
		|| !ofc_colstr_atomic_writef(cs, " = ")
		|| !ofc_parse_expr_print(cs, id->init)
		|| !ofc_colstr_atomic_writef(cs, ", ")
		|| !ofc_parse_expr_print(cs, id->limit))
		return false;

	if (id->step)
	{
		if (!ofc_colstr_atomic_writef(cs, ", ")
			|| !ofc_parse_expr_print(cs, id->step))
			return false;
	}

	return ofc_colstr_atomic_writef(cs, ")");
}
