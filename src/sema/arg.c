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

static ofc_sema_arg_list_t* ofc_sema_arg_list__create(unsigned count)
{
	ofc_sema_arg_list_t* list
		= (ofc_sema_arg_list_t*)malloc(
			sizeof(ofc_sema_arg_list_t));
	if (!list) return NULL;

	list->arg = (ofc_sema_arg_t*)malloc(
		sizeof(ofc_sema_arg_t) * count);
	if (!list->arg)
	{
		free(list);
		return NULL;
	}

	list->count = count;
	return list;
}

ofc_sema_arg_list_t* ofc_sema_arg_list(
	const ofc_parse_call_arg_list_t* plist)
{
	if (!plist)
		return NULL;

	ofc_sema_arg_list_t* list
		= ofc_sema_arg_list__create(plist->count);
	if (!list) return NULL;

	unsigned i;
	for (i = 0; i < plist->count; i++)
	{
		const ofc_parse_call_arg_t* arg
			= plist->call_arg[i];
		if (!arg)
		{
			ofc_sema_arg_list_delete(list);
			return NULL;
		}

		if (!ofc_sparse_ref_empty(arg->name))
		{
			ofc_sparse_ref_error(arg->src,
				"Named parameters not valid as FUNCTION arguments");
			ofc_sema_arg_list_delete(list);
			return NULL;
		}

		if (arg->type == OFC_PARSE_CALL_ARG_EXPR)
		{
			if (!arg->expr
				|| (arg->expr->type != OFC_PARSE_EXPR_VARIABLE)
				|| !arg->expr->variable
				|| (arg->expr->variable->type != OFC_PARSE_LHS_VARIABLE))
			{
				ofc_sparse_ref_error(arg->src,
					"Dummy arguments can only be names");
				ofc_sema_arg_list_delete(list);
				return NULL;
			}

			/* TODO - Make list exclusive. */

			list->arg[i].alt_return = false;
			list->arg[i].name = arg->expr->variable->variable;
		}
		else if (arg->type == OFC_PARSE_CALL_ARG_ASTERISK)
		{
			list->arg[i].alt_return = true;
			list->arg[i].name = OFC_SPARSE_REF_EMPTY;
		}
		else
		{
			ofc_sparse_ref_error(arg->src,
				"Dummy arguments can only be names or asterisks");
			ofc_sema_arg_list_delete(list);
			return NULL;
		}
	}

	return list;
}

ofc_sema_arg_list_t* ofc_sema_arg_list_stmt_func(
	const ofc_parse_array_index_t* index)
{
	if (!index)
		return NULL;

	ofc_sema_arg_list_t* list
		= ofc_sema_arg_list__create(index->count);
	if (!list) return NULL;

	unsigned i;
	for (i = 0; i < index->count; i++)
	{
		const ofc_parse_array_range_t* range
			= index->range[i];
		if (!range)
		{
			ofc_sema_arg_list_delete(list);
			return NULL;
		}

		if (range->is_slice || !range->first
			|| range->last || range->stride)
		{
			ofc_sparse_ref_error(range->src,
				"Invalid argument in statement function");
			ofc_sema_arg_list_delete(list);
			return NULL;
		}

		const ofc_parse_expr_t* expr
			= range->first;
		if ((expr->type != OFC_PARSE_EXPR_VARIABLE)
			|| !expr->variable
			|| (expr->variable->type != OFC_PARSE_LHS_VARIABLE))
		{
			ofc_sparse_ref_error(range->src,
				"Statement function's argument list"
				" must only contain argument names.");
			ofc_sema_arg_list_delete(list);
			return NULL;
		}

		if ((expr->type != OFC_PARSE_EXPR_VARIABLE)
			|| !expr->variable
			|| (expr->variable->type != OFC_PARSE_LHS_VARIABLE))
		{
			ofc_sparse_ref_error(range->src,
				"Dummy arguments can only be names");
			ofc_sema_arg_list_delete(list);
			return NULL;
		}

		/* TODO - Make list exclusive. */

		list->arg[i].alt_return = false;
		list->arg[i].name = expr->variable->variable;
	}

	return list;
}

bool ofc_sema_arg_list_print(
	ofc_colstr_t* cs,
	const ofc_sema_arg_list_t* list)
{
	if (!cs || !list)
		return false;

	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		if (list->arg[i].alt_return)
		{
			if (!ofc_colstr_atomic_writef(cs, "*"))
				return false;
		}
		else
		{
			if (!ofc_sparse_ref_print(cs, list->arg[i].name))
				return false;
		}

		if (i < (list->count - 1)
			&& !ofc_colstr_atomic_writef(cs, ", "))
			return false;
	}

	return true;
}

void ofc_sema_arg_list_delete(
	ofc_sema_arg_list_t* list)
{
	if (!list)
		return;

	free(list->arg);
	free(list);
}
