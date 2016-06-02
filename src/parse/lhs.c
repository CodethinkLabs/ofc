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


static void ofc_parse_lhs__cleanup(
	ofc_parse_lhs_t lhs)
{
	switch (lhs.type)
	{
		case OFC_PARSE_LHS_ARRAY:
		case OFC_PARSE_LHS_STAR_LEN:
		case OFC_PARSE_LHS_MEMBER_STRUCTURE:
		case OFC_PARSE_LHS_MEMBER_TYPE:
			ofc_parse_lhs_delete(lhs.parent);
			break;
		default:
			break;
	}

	switch (lhs.type)
	{
		case OFC_PARSE_LHS_ARRAY:
			ofc_parse_array_index_delete(lhs.array.index);
			break;
		case OFC_PARSE_LHS_STAR_LEN:
			ofc_parse_expr_delete(lhs.star_len.len);
			break;
		case OFC_PARSE_LHS_IMPLICIT_DO:
			ofc_parse_lhs_implicit_do_delete(lhs.implicit_do);
			break;
		default:
			break;
	}
}

static ofc_parse_lhs_t* ofc_parse_lhs__alloc(
	ofc_parse_lhs_t lhs)
{
	ofc_parse_lhs_t* alhs
		= (ofc_parse_lhs_t*)malloc(
			sizeof(ofc_parse_lhs_t));
	if (!alhs) return NULL;

	*alhs = lhs;
	return alhs;
}

static bool ofc_parse_lhs__clone(
	ofc_parse_lhs_t* dst, const ofc_parse_lhs_t* src)
{
	if (!src || !dst)
		return NULL;

	ofc_parse_lhs_t clone;
	clone.type = src->type;
	clone.src  = src->src;

	switch (src->type)
	{
		case OFC_PARSE_LHS_ARRAY:
		case OFC_PARSE_LHS_STAR_LEN:
		case OFC_PARSE_LHS_MEMBER_STRUCTURE:
		case OFC_PARSE_LHS_MEMBER_TYPE:
			clone.parent = ofc_parse_lhs_copy(
				src->parent);
			if (src->parent && !clone.parent)
				return false;
			break;
		default:
			break;
	}

	switch (src->type)
	{
		case OFC_PARSE_LHS_VARIABLE:
			clone.variable = src->variable;
			break;

		case OFC_PARSE_LHS_ARRAY:
			clone.array.index = ofc_parse_array_index_copy(
				src->array.index);
			if (src->array.index
				&& !clone.array.index)
			{
				ofc_parse_lhs_delete(clone.parent);
				return false;
			}
			break;

		case OFC_PARSE_LHS_STAR_LEN:
			clone.star_len.var = src->star_len.var;
			clone.star_len.len = ofc_parse_expr_copy(
				src->star_len.len);
			if (src->star_len.len
				&& !clone.star_len.len)
			{
				ofc_parse_lhs_delete(clone.parent);
				return false;
			}
			break;

		case OFC_PARSE_LHS_MEMBER_STRUCTURE:
		case OFC_PARSE_LHS_MEMBER_TYPE:
			clone.member.name = src->member.name;
			break;

		case OFC_PARSE_LHS_IMPLICIT_DO:
			clone.implicit_do = ofc_parse_lhs_implicit_do_copy(
				src->implicit_do);
			if (src->implicit_do
				&& !clone.implicit_do)
				return false;
			break;

		default:
			break;
	}

	*dst = clone;
	return true;
}



static ofc_parse_lhs_t* ofc_parse_lhs__array(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_lhs_t* parent, unsigned* len)
{
	if (!parent)
		return NULL;

	switch (parent->type)
	{
		case OFC_PARSE_LHS_IMPLICIT_DO:
			return NULL;
		default:
			break;
	}

	unsigned i = 0;
	if (ptr[i] != '(')
		return NULL;

	unsigned dpos = ofc_parse_debug_position(debug);

	ofc_parse_lhs_t lhs;
	unsigned l = 0;
	lhs.array.index = ofc_parse_array_index(
		src, &ptr[i], debug, &l);
	if (!lhs.array.index)
	{
		if (ptr[i + 1] != ')')
			return NULL;
		l = 2;
	}
	i += l;

	lhs.type   = OFC_PARSE_LHS_ARRAY;
	lhs.src    = ofc_sparse_ref(src, ptr, i);
	lhs.parent = NULL;

	ofc_parse_lhs_t* alhs
		= ofc_parse_lhs__alloc(lhs);
	if (!alhs)
	{
		ofc_parse_array_index_delete(lhs.array.index);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}

	alhs->parent = parent;

	if (len) *len = i;
	return alhs;
}

static ofc_parse_lhs_t* ofc_parse_lhs__star_len(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_lhs_t* parent, unsigned* len)
{
	if (!parent)
		return NULL;

	switch (parent->type)
	{
		case OFC_PARSE_LHS_IMPLICIT_DO:
			return NULL;
		default:
			break;
	}

	ofc_parse_lhs_t lhs;
	lhs.type   = OFC_PARSE_LHS_STAR_LEN;
	lhs.parent = NULL;

	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = ofc_parse_star_len(
		src, ptr, debug,
		&lhs.star_len.len,
		&lhs.star_len.var);
	if (i == 0) return NULL;

	lhs.src = ofc_sparse_ref(src, ptr, i);

	ofc_parse_lhs_t* alhs
		= ofc_parse_lhs__alloc(lhs);
	if (!alhs)
	{
		ofc_parse_expr_delete(lhs.star_len.len);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}

	alhs->parent = parent;

	if (len) *len = i;
	return alhs;
}

static ofc_parse_lhs_t* ofc_parse_lhs__member(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_lhs_t* parent, unsigned* len)
{
	if (!parent)
		return NULL;

	switch (parent->type)
	{
		case OFC_PARSE_LHS_IMPLICIT_DO:
			return NULL;
		default:
			break;
	}


	ofc_parse_lhs_t lhs;
	lhs.type   = OFC_PARSE_LHS_MEMBER_TYPE;
	lhs.parent = NULL;

	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i;
	if (ptr[0] == '.')
	{
		i = ofc_parse_operator(
			src, ptr, debug, NULL);
		if (i > 0)
		{
			ofc_parse_debug_rewind(debug, dpos);
			return NULL;
		}
		lhs.type = OFC_PARSE_LHS_MEMBER_STRUCTURE;
	}
	else if (ptr[0] != '%')
	{
		return NULL;
	}
	i = 1;

	unsigned l = ofc_parse_name(
		src, &ptr[i], debug, &lhs.member.name);
	if (l == 0) return NULL;
	i += l;

	lhs.src = ofc_sparse_ref(src, ptr, i);

	ofc_parse_lhs_t* alhs
		= ofc_parse_lhs__alloc(lhs);
	if (!alhs)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}

	alhs->parent = parent;

	if (len) *len = i;
	return alhs;
}


static ofc_parse_lhs_t* ofc_parse__lhs(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug, bool allow_array,
	bool star_len,
	unsigned* len)
{
	ofc_parse_lhs_t lhs;

	unsigned dpos = ofc_parse_debug_position(debug);

	lhs.type = OFC_PARSE_LHS_VARIABLE;
	unsigned i = ofc_parse_name(
		src, ptr, debug, &lhs.variable);
	if (i == 0) return NULL;

	lhs.src = ofc_sparse_ref(src, ptr, i);

	ofc_parse_lhs_t* alhs
		= ofc_parse_lhs__alloc(lhs);
	if (!alhs)
	{
		ofc_parse_lhs__cleanup(lhs);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}

	while (true)
	{
		unsigned l;
		ofc_parse_lhs_t* child_lhs;

		if(allow_array)
		{
			child_lhs = ofc_parse_lhs__array(
					src, &ptr[i], debug, alhs, &l);
			if (child_lhs)
			{
				i += l;
				alhs->src = ofc_sparse_ref(src, ptr, i);
				alhs = child_lhs;
				continue;
			}
		}

		if (star_len)
		{
			child_lhs = ofc_parse_lhs__star_len(
					src, &ptr[i], debug, alhs, &l);
			if (child_lhs)
			{
				i += l;
				alhs->src = ofc_sparse_ref(src, ptr, i);
				alhs = child_lhs;
				continue;
			}
		}

		child_lhs = ofc_parse_lhs__member(
				src, &ptr[i], debug, alhs, &l);
		if (child_lhs)
		{
			i += l;
			alhs->src = ofc_sparse_ref(src, ptr, i);
			alhs = child_lhs;
			continue;
		}

		break;
	}

	if (len) *len = i;
	return alhs;
}

ofc_parse_lhs_t* ofc_parse_lhs_star_len(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	return ofc_parse__lhs(
		src, ptr, debug, true , true, len);
}

ofc_parse_lhs_t* ofc_parse_lhs_variable(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	return ofc_parse__lhs(
		src, ptr, debug, false, false ,len);
}

ofc_parse_lhs_t* ofc_parse_lhs_alias(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	ofc_parse_lhs_t lhs;

	lhs.type = OFC_PARSE_LHS_ALIAS;
	unsigned i = ofc_parse_name(
		src, ptr, debug, &lhs.alias.name);
	if (i == 0)
		return NULL;

    if ((ptr[i++] == '=')
		&& (ptr[i++] == '>'))
	{
		unsigned rlen = ofc_parse_name(
			src, &ptr[i], debug, &lhs.alias.target);
		if (rlen == 0)
			return NULL;
		i += rlen;
	}
	else
	{
		return NULL;
	}

	if (len) *len = i;

	ofc_parse_lhs_t* alhs
		= ofc_parse_lhs__alloc(lhs);
	return alhs;
}


ofc_parse_lhs_t* ofc_parse_lhs(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	return ofc_parse__lhs(
		src, ptr, debug, true, false ,len);
}

ofc_parse_lhs_t* ofc_parse_lhs_id(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned l;
	ofc_parse_lhs_implicit_do_t* id
		= ofc_parse_lhs_implicit_do(
			src, ptr, debug, &l);
	if (id)
	{
		ofc_parse_lhs_t* lhs
			= (ofc_parse_lhs_t*)malloc(
				sizeof(ofc_parse_lhs_t));
		if (!lhs)
		{
			ofc_parse_lhs_implicit_do_delete(id);
			ofc_parse_debug_rewind(debug, dpos);
			return NULL;
		}

		lhs->type = OFC_PARSE_EXPR_IMPLICIT_DO;
		lhs->src  = ofc_sparse_ref(src, ptr, l);
		lhs->implicit_do = id;

		if (len) *len = l;
		return lhs;
	}

	return ofc_parse__lhs(
		src, ptr, debug, true, false, len);
}


ofc_parse_lhs_t* ofc_parse_lhs_copy(
	ofc_parse_lhs_t* lhs)
{
	if (!lhs)
		return NULL;

	ofc_parse_lhs_t clone;
	if (!ofc_parse_lhs__clone(&clone, lhs))
		return NULL;

	ofc_parse_lhs_t* copy
		= ofc_parse_lhs__alloc(clone);
	if (!copy)
	{
		ofc_parse_lhs__cleanup(clone);
		return NULL;
	}

	return copy;
}

void ofc_parse_lhs_delete(
	ofc_parse_lhs_t* lhs)
{
	if (!lhs)
		return;

	ofc_parse_lhs__cleanup(*lhs);
	free(lhs);
}

bool ofc_parse_lhs_print(
	ofc_colstr_t* cs, const ofc_parse_lhs_t* lhs,
	bool is_decl)
{
	switch (lhs->type)
	{
		case OFC_PARSE_LHS_VARIABLE:
			return ofc_sparse_ref_print(cs, lhs->variable);
		case OFC_PARSE_LHS_ALIAS:
			return (ofc_sparse_ref_print(cs, lhs->alias.target)
				&& ofc_colstr_atomic_writef(cs, " ")
				&& ofc_colstr_atomic_writef(cs, "=>")
				&& ofc_colstr_atomic_writef(cs, " ")
				&& ofc_sparse_ref_print(cs, lhs->alias.name));
		case OFC_PARSE_LHS_ARRAY:
			if (!ofc_parse_lhs_print(
				cs, lhs->parent, is_decl))
				return false;
			if (lhs->array.index)
				return ofc_parse_array_index_print(
					cs, lhs->array.index, is_decl);
			return ofc_colstr_atomic_writef(cs, "()");
		case OFC_PARSE_LHS_STAR_LEN:
			if (!ofc_parse_lhs_print(
				cs, lhs->parent, is_decl)
				|| !ofc_colstr_atomic_writef(cs, "*"))
				return false;
			if (lhs->star_len.var)
			{
				return ofc_colstr_atomic_writef(cs, "(*)");
			}
			else if (lhs->star_len.len)
			{
				bool bracketed = (lhs->star_len.len->type
					!= OFC_PARSE_EXPR_CONSTANT);

				if (bracketed && !ofc_colstr_atomic_writef(cs, "("))
					return false;

				if (!ofc_parse_expr_print(
					cs, lhs->star_len.len))
					return false;

				return (!bracketed || ofc_colstr_atomic_writef(cs, "("));
			}
			break;
		case OFC_PARSE_LHS_MEMBER_TYPE:
			return (ofc_parse_lhs_print(cs, lhs->parent, is_decl)
				&& ofc_colstr_atomic_writef(cs, "%%")
				&& ofc_sparse_ref_print(cs, lhs->member.name));
		case OFC_PARSE_LHS_MEMBER_STRUCTURE:
			return (ofc_parse_lhs_print(cs, lhs->parent, is_decl)
				&& ofc_colstr_atomic_writef(cs, ".")
				&& ofc_sparse_ref_print(cs, lhs->member.name));
		case OFC_PARSE_LHS_IMPLICIT_DO:
			return ofc_parse_lhs_implicit_do_print(
				cs, lhs->implicit_do);
		default:
			break;
	}

	return false;
}

static bool ofc_parse_lhs_print__decl(
	ofc_colstr_t* cs, const ofc_parse_lhs_t* lhs)
{
	return ofc_parse_lhs_print(cs, lhs, true);
}

static bool ofc_parse_lhs_print__not_decl(
	ofc_colstr_t* cs, const ofc_parse_lhs_t* lhs)
{
	return ofc_parse_lhs_print(cs, lhs, false);
}


bool ofc_parse_lhs_base_name(
	const ofc_parse_lhs_t lhs,
	ofc_sparse_ref_t* name)
{
	if (lhs.type == OFC_PARSE_LHS_VARIABLE)
	{
		if (name) *name = lhs.variable;
		return true;
	}

	if (!lhs.parent)
		return false;

	return ofc_parse_lhs_base_name(
		*lhs.parent, name);
}


bool ofc_parse_lhs_possible_function_call(
	const ofc_parse_lhs_t lhs)
{
	if ((lhs.type != OFC_PARSE_LHS_ARRAY)
		|| !lhs.parent
		|| (lhs.parent->type != OFC_PARSE_LHS_VARIABLE))
		return false;

	if (ofc_parse_lhs_possible_function_call(*lhs.parent))
		return true;

	if (!lhs.array.index)
		return true;

	if (!lhs.array.index->range)
		return false;

	unsigned i;
	for (i = 0; i < lhs.array.index->count; i++)
	{
		ofc_parse_array_range_t* range
			= lhs.array.index->range[i];
		if (!range) return false;

		if (!range->first || range->is_slice
			|| range->last || range->stride)
			return false;
	}

	return true;
}



ofc_parse_lhs_list_t* ofc_parse_lhs_list(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	ofc_parse_lhs_list_t* list
		= (ofc_parse_lhs_list_t*)malloc(
			sizeof(ofc_parse_lhs_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->lhs = NULL;

	unsigned i = ofc_parse_list(
		src, ptr, debug, ',',
		&list->count, (void***)&list->lhs,
		(void*)ofc_parse_lhs_id,
		(void*)ofc_parse_lhs_delete);
	if (i == 0)
	{
		free(list);
		return NULL;
	}

	if (len) *len = i;
	return list;
}

ofc_parse_lhs_list_t* ofc_parse_lhs_list_bracketed(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	unsigned i = 0;

	if (ptr[i++] != '(')
		return NULL;

	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned l;
	ofc_parse_lhs_list_t* list
		= ofc_parse_lhs_list(src, &ptr[i], debug, &l);
	if (!list) return NULL;
	i += l;

	if (ptr[i++] != ')')
	{
		ofc_parse_lhs_list_delete(list);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}

	if (len) *len = i;
	return list;
}

ofc_parse_lhs_list_t* ofc_parse_lhs_alias_list(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	ofc_parse_lhs_list_t* list
		= (ofc_parse_lhs_list_t*)malloc(
			sizeof(ofc_parse_lhs_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->lhs = NULL;

	unsigned i = ofc_parse_list(
		src, ptr, debug, ',',
		&list->count, (void***)&list->lhs,
		(void*)ofc_parse_lhs_alias,
		(void*)ofc_parse_lhs_delete);
	if (i == 0)
	{
		free(list);
		return NULL;
	}

	if (len) *len = i;
	return list;
}

ofc_parse_lhs_list_t* ofc_parse_lhs_list_copy(
	const ofc_parse_lhs_list_t* list)
{
	if (!list)
		return NULL;

	ofc_parse_lhs_list_t* copy
		= (ofc_parse_lhs_list_t*)malloc(
			sizeof(ofc_parse_lhs_list_t));
	if (!copy) return NULL;

	copy->count = 0;
	copy->lhs = NULL;

	if (!ofc_parse_list_copy(
		&copy->count, (void***)&copy->lhs,
		list->count, (const void**)list->lhs,
		(void*)ofc_parse_lhs_copy,
		(void*)ofc_parse_lhs_delete))
	{
		free(copy);
		return NULL;
	}

	return copy;
}

void ofc_parse_lhs_list_delete(
	ofc_parse_lhs_list_t* list)
{
	if (!list)
		return;

	ofc_parse_list_delete(
		list->count, (void**)list->lhs,
		(void*)ofc_parse_lhs_delete);
	free(list);
}

bool ofc_parse_lhs_list_print(
	ofc_colstr_t* cs, const ofc_parse_lhs_list_t* list,
	bool is_decl)
{
	return ofc_parse_list_print(cs,
		list->count, (const void**)list->lhs,
		(void*)(is_decl
			? ofc_parse_lhs_print__decl
			: ofc_parse_lhs_print__not_decl));
}

bool ofc_parse_lhs_list_bracketed_print(
	ofc_colstr_t* cs, const ofc_parse_lhs_list_t* list,
	bool is_decl)
{
	return (ofc_colstr_atomic_writef(cs, "(")
		&& ofc_parse_lhs_list_print(cs, list, is_decl)
		&& ofc_colstr_atomic_writef(cs, ")"));
}
