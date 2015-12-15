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


static ofc_parse_call_arg_t* ofc_parse__call_arg(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	bool named, bool force, unsigned* len)
{
	ofc_parse_call_arg_t* call_arg
		= (ofc_parse_call_arg_t*)malloc(
			sizeof(ofc_parse_call_arg_t));
	if (!call_arg) return NULL;

	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = 0;
	call_arg->name = OFC_SPARSE_REF_EMPTY;
	if (named)
	{
		ofc_sparse_ref_t ident;
		unsigned l = ofc_parse_ident(
			src, &ptr[i], debug, &ident);
		if ((l > 0) && (ptr[i + l] == '='))
		{
			call_arg->name = ident;
			i += (l + 1);
		}
		else if (force)
		{
			free(call_arg);
			ofc_parse_debug_rewind(debug, dpos);
			return NULL;
		}
	}

	call_arg->expr = NULL;
	if ((ptr[i] == '*') || (ptr[i] == '&'))
	{
		i += 1;

		unsigned l = 0;
		call_arg->expr = ofc_parse_expr_integer_variable(
			src, &ptr[i], debug, &l);
		call_arg->type = ((call_arg->expr != NULL)
			? OFC_PARSE_CALL_ARG_RETURN
			: OFC_PARSE_CALL_ARG_ASTERISK);
		i += l;
	}
	else
	{
		unsigned l;
		call_arg->expr = ofc_parse_expr(
			src, &ptr[i], debug, &l);
		if (!call_arg->expr)
		{
			free(call_arg);
			ofc_parse_debug_rewind(debug, dpos);
			return NULL;
		}
		i += l;
		call_arg->type = OFC_PARSE_CALL_ARG_EXPR;
	}

	call_arg->src = ofc_sparse_ref(src, ptr, i);

	if (len) *len = i;
	return call_arg;
}

ofc_parse_call_arg_t* ofc_parse_call_arg_force_named(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	return ofc_parse__call_arg(
		src, ptr, debug,
		true, true, len);
}

ofc_parse_call_arg_t* ofc_parse_call_arg_named(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	return ofc_parse__call_arg(
		src, ptr, debug,
		true, false, len);
}

ofc_parse_call_arg_t* ofc_parse_call_arg(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	return ofc_parse__call_arg(
		src, ptr, debug,
		false, false, len);
}

void ofc_parse_call_arg_delete(
	ofc_parse_call_arg_t* call_arg)
{
	if (!call_arg)
		return;

	ofc_parse_expr_delete(call_arg->expr);
	free(call_arg);
}

bool ofc_parse_call_arg_print(
	ofc_colstr_t* cs, const ofc_parse_call_arg_t* call_arg)
{
	if (!call_arg)
		return false;

	if (!ofc_sparse_ref_empty(call_arg->name))
	{
		if (!ofc_sparse_ref_print(cs, call_arg->name)
			|| !ofc_colstr_atomic_writef(cs, "="))
			return false;
	}

	switch (call_arg->type)
	{
		case OFC_PARSE_CALL_ARG_RETURN:
		case OFC_PARSE_CALL_ARG_ASTERISK:
			if (!ofc_colstr_atomic_writef(cs, "*"))
				return false;
			break;
		default:
			break;
	}

	switch (call_arg->type)
	{
		case OFC_PARSE_CALL_ARG_RETURN:
		case OFC_PARSE_CALL_ARG_EXPR:
			if (!ofc_parse_expr_print(
				cs, call_arg->expr))
				return false;
			break;
		default:
			break;
	}

	return true;
}



static ofc_parse_call_arg_list_t* ofc_parse_call_arg__list(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	bool named, bool force, unsigned* len)
{
	ofc_parse_call_arg_list_t* list
		= (ofc_parse_call_arg_list_t*)malloc(
			sizeof(ofc_parse_call_arg_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->call_arg = NULL;

	unsigned i = ofc_parse_list(src, ptr, debug, ',',
		&list->count, (void***)&list->call_arg,
		(named ? (force ? (void*)ofc_parse_call_arg_force_named
				: (void*)ofc_parse_call_arg_named)
			: (void*)ofc_parse_call_arg),
		(void*)ofc_parse_call_arg_delete);
	if (i == 0)
	{
		free(list);
		return NULL;
	}

	if (len) *len = i;
	return list;
}

ofc_parse_call_arg_list_t* ofc_parse_call_arg_list_force_named(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	return ofc_parse_call_arg__list(
		src, ptr, debug, true, true, len);
}

ofc_parse_call_arg_list_t* ofc_parse_call_arg_list_named(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	return ofc_parse_call_arg__list(
		src, ptr, debug, true, false, len);
}

ofc_parse_call_arg_list_t* ofc_parse_call_arg_list(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	return ofc_parse_call_arg__list(
		src, ptr, debug, false, false, len);
}

ofc_parse_call_arg_list_t* ofc_parse_call_arg_list_wrap(
	ofc_parse_call_arg_t* arg)
{
	if (!arg)
		return NULL;

	ofc_parse_call_arg_list_t* list
		= (ofc_parse_call_arg_list_t*)malloc(
			sizeof(ofc_parse_call_arg_list_t));
	if (!list) return NULL;

	list->call_arg = (ofc_parse_call_arg_t**)malloc(
		sizeof(ofc_parse_call_arg_t*));
	if (!list->call_arg)
	{
		free(list);
		return NULL;
	}

	list->count = 1;
	list->call_arg[0] = arg;

	return list;
}

void ofc_parse_call_arg_list_delete(
	ofc_parse_call_arg_list_t* list)
{
	if (!list)
		return;

	ofc_parse_list_delete(
		list->count, (void**)list->call_arg,
		(void*)ofc_parse_call_arg_delete);
	free(list);
}

bool ofc_parse_call_arg_list_print(
	ofc_colstr_t* cs, const ofc_parse_call_arg_list_t* list)
{
	return ofc_parse_list_print(
		cs, list->count, (const void**)list->call_arg,
		(void*)ofc_parse_call_arg_print);
}
