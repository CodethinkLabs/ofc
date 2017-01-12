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

void ofc_sema_dummy_arg_delete(
	ofc_sema_dummy_arg_t* arg)
{
	if (!arg)
		return;

	switch (arg->type)
	{
		case OFC_SEMA_DUMMY_ARG_EXPR:
			ofc_sema_expr_delete(arg->expr);
			break;
		default:
			break;
	}

	free(arg);
}

ofc_sema_dummy_arg_list_t* ofc_sema_dummy_arg_list_create(void)
{
	ofc_sema_dummy_arg_list_t* list
		= (ofc_sema_dummy_arg_list_t*)malloc(
			sizeof(ofc_sema_dummy_arg_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->dummy_arg  = NULL;
	return list;
}

void ofc_sema_dummy_arg_list_delete(
	ofc_sema_dummy_arg_list_t* list)
{
	if (!list)
		return;

	unsigned i;
	for (i = 0; i < list->count; i++)
		ofc_sema_dummy_arg_delete(list->dummy_arg[i]);
	free(list->dummy_arg);

	free(list);
}

bool ofc_sema_dummy_arg_list_add(
	ofc_sema_dummy_arg_list_t* list,
	ofc_sema_dummy_arg_t* dummy_arg)
{
	if (!list || !dummy_arg)
		return false;

	ofc_sema_dummy_arg_t** ndummy_arg
		= (ofc_sema_dummy_arg_t**)realloc(list->dummy_arg,
			(sizeof(ofc_sema_dummy_arg_t*) * (list->count + 1)));
	if (!ndummy_arg) return NULL;

	list->dummy_arg = ndummy_arg;
	list->dummy_arg[list->count++] = dummy_arg;
	return true;
}

static ofc_sema_dummy_arg_t* ofc_sema_dummy_arg__create(
	ofc_sema_dummy_arg_e type)
{
	if (type >= OFC_SEMA_DUMMY_ARG_COUNT)
		return NULL;

	ofc_sema_dummy_arg_t* dummy_arg
		= (ofc_sema_dummy_arg_t*)malloc(
			sizeof(ofc_sema_dummy_arg_t));
	if (!dummy_arg) return NULL;

	dummy_arg->type = type;

	dummy_arg->src = OFC_SPARSE_REF_EMPTY;

	switch (type)
	{
		case OFC_SEMA_DUMMY_ARG_EXTERNAL:
			dummy_arg->external = NULL;
			break;
		case OFC_SEMA_DUMMY_ARG_EXPR:
			dummy_arg->expr = NULL;
			break;
		default:
			break;
	}

	return dummy_arg;
}

ofc_sema_dummy_arg_t* ofc_sema_dummy_arg(
	ofc_sema_scope_t* scope,
	const ofc_parse_expr_t* expr)
{
	if (!expr) return NULL;


	if (expr->type == OFC_PARSE_EXPR_VARIABLE)
	{
		ofc_sparse_ref_t base_name;
		if (!ofc_parse_lhs_base_name(
			*(expr->variable), &base_name))
			return NULL;

		const ofc_sema_decl_t* decl
			= ofc_sema_scope_decl_find(
				scope, base_name.string, false);

		const ofc_sema_external_t* external
			= ofc_sema_external_list_find(
				scope->external, base_name.string);

		if (external && !decl)
		{
			ofc_sema_dummy_arg_t* dummy_arg
				= ofc_sema_dummy_arg__create(
					OFC_SEMA_DUMMY_ARG_EXTERNAL);
			if (!dummy_arg) return NULL;

			dummy_arg->external = external;
			dummy_arg->src      = expr->src;

			return dummy_arg;
		}
	}

	/* We only create an expression if it's not external
	   so we don't create a decl if it's external */
	ofc_sema_dummy_arg_t* dummy_arg
		= ofc_sema_dummy_arg__create(
			OFC_SEMA_DUMMY_ARG_EXPR);
	if (!dummy_arg) return NULL;

	dummy_arg->expr = ofc_sema_expr_dummy_arg(scope, expr);
	dummy_arg->src  = expr->src;

	if (!dummy_arg->expr)
	{
		ofc_sema_dummy_arg_delete(dummy_arg);
		return NULL;
	}

	return dummy_arg;
}

ofc_sema_dummy_arg_t* ofc_sema_dummy_arg_copy(
	const ofc_sema_dummy_arg_t* dummy_arg)
{
	return ofc_sema_dummy_arg_copy_replace(
		dummy_arg, NULL, NULL);
}

ofc_sema_dummy_arg_t* ofc_sema_dummy_arg_cast(
	const ofc_sema_dummy_arg_t* dummy_arg,
	const ofc_sema_type_t* type)
{
	if (!type || !dummy_arg)
		return NULL;

	if (!ofc_sema_dummy_arg_is_expr(dummy_arg))
		return ofc_sema_dummy_arg_copy(dummy_arg);

	ofc_sema_expr_t* copy
		= ofc_sema_expr_copy(
			dummy_arg->expr);
	if (!copy) return NULL;

	ofc_sema_expr_t* expr
		= ofc_sema_expr_cast(
			copy, type);
	if (!expr)
	{
		ofc_sema_expr_delete(copy);
		return NULL;
	}

	ofc_sema_dummy_arg_t* cast
		= ofc_sema_dummy_arg__create(
			OFC_SEMA_DUMMY_ARG_EXPR);
	if (!cast)
	{
		ofc_sema_expr_delete(expr);
		return NULL;
	}

	cast->expr = expr;
	cast->src  = expr->src;

	return cast;
}

ofc_sema_dummy_arg_t* ofc_sema_dummy_arg_alt_return(
	ofc_sema_scope_t* scope,
	const ofc_parse_expr_t* expr)
{
	if (!expr) return NULL;

	ofc_sema_dummy_arg_t* dummy_arg
		= ofc_sema_dummy_arg__create(
			OFC_SEMA_DUMMY_ARG_EXPR);
	if (!dummy_arg) return NULL;

	dummy_arg->expr = ofc_sema_expr_alt_return(scope, expr);
	dummy_arg->src  = expr->src;

	return dummy_arg;
}

bool ofc_sema_dummy_arg_is_alt_return(
	const ofc_sema_dummy_arg_t* dummy_arg)
{
	if (!dummy_arg)
		return false;

	if (dummy_arg->type != OFC_SEMA_DUMMY_ARG_EXPR)
		return false;

	return dummy_arg->expr->is_alt_return;
}

bool ofc_sema_dummy_arg_is_external(
	const ofc_sema_dummy_arg_t* dummy_arg)
{
	if (!dummy_arg) return false;

	return (dummy_arg->type == OFC_SEMA_DUMMY_ARG_EXTERNAL);
}

bool ofc_sema_dummy_arg_is_expr(
	const ofc_sema_dummy_arg_t* dummy_arg)
{
	if (!dummy_arg) return false;

	return (dummy_arg->type == OFC_SEMA_DUMMY_ARG_EXPR);
}

const ofc_sema_type_t* ofc_sema_dummy_arg_type(
	const ofc_sema_dummy_arg_t* dummy_arg)
{
	if (!dummy_arg) return NULL;

	switch (dummy_arg->type)
	{
		case OFC_SEMA_DUMMY_ARG_EXPR:
			return ofc_sema_expr_type(dummy_arg->expr);
		default:
			break;
	}

	/* NULL for externals too */
	return NULL;
}

ofc_sema_expr_t* ofc_sema_dummy_arg_get_expr(
	ofc_sema_dummy_arg_t* dummy_arg)
{
	if (!dummy_arg) return NULL;

	if (dummy_arg->type == OFC_SEMA_DUMMY_ARG_EXPR)
		return dummy_arg->expr;

	return NULL;
}

ofc_sema_dummy_arg_t* ofc_sema_dummy_arg_copy_replace(
	const ofc_sema_dummy_arg_t* dummy_arg,
	const ofc_sema_decl_t* replace,
	const ofc_sema_expr_t* with)
{
	if (!dummy_arg) return NULL;

	ofc_sema_dummy_arg_t* copy
		= ofc_sema_dummy_arg__create(dummy_arg->type);
	if (!copy) return NULL;

	copy->src = dummy_arg->src;

	bool success = true;
	switch (copy->type)
	{
		case OFC_SEMA_DUMMY_ARG_EXPR:
			copy->expr = ofc_sema_expr_copy_replace(
				dummy_arg->expr, replace, with);
			success = (copy->expr != NULL);
			break;

		case OFC_SEMA_DUMMY_ARG_EXTERNAL:
			copy->external = dummy_arg->external;
			break;

		default:
			break;
	}

	if (!success)
	{
		ofc_sema_dummy_arg_delete(copy);
		return NULL;
	}

	return copy;
}

ofc_sema_dummy_arg_t* ofc_sema_dummy_arg_wrap_expr(
	ofc_sema_expr_t* expr)
{
	if (!expr) return NULL;

	ofc_sema_dummy_arg_t* dummy_arg
		= ofc_sema_dummy_arg__create(
			OFC_SEMA_DUMMY_ARG_EXPR);
	if (!dummy_arg) return NULL;

	dummy_arg->expr = expr;
	dummy_arg->src  = expr->src;

	return dummy_arg;
}

bool ofc_sema_dummy_arg_compare(
	const ofc_sema_dummy_arg_t* a,
	const ofc_sema_dummy_arg_t* b)
{
	if (!a || !b)
		return false;

	if (a == b)
		return true;

	if (a->type != b->type)
		return false;

	switch (a->type)
	{
		case OFC_SEMA_DUMMY_ARG_EXTERNAL:
			return ofc_str_ref_equal(
				a->external->name.string,
				b->external->name.string);
		case OFC_SEMA_DUMMY_ARG_EXPR:
			return ofc_sema_expr_compare(
				a->expr, b->expr);

		default:
			break;
	}

	return false;
}

bool ofc_sema_dummy_arg_mark_used(
	ofc_sema_dummy_arg_t* dummy_arg)
{
	if (!dummy_arg)
		return false;

	switch (dummy_arg->type)
	{
		case OFC_SEMA_DUMMY_ARG_EXPR:
			if (dummy_arg->expr->type == OFC_SEMA_EXPR_LHS)
			{
				if (!ofc_sema_lhs_mark_used(
					dummy_arg->expr->lhs, true, true))
					return false;
			}
			break;
		case OFC_SEMA_DUMMY_ARG_EXTERNAL:
			if (dummy_arg->external->decl)
			{
				if (!ofc_sema_decl_mark_used(
					dummy_arg->external->decl, true, true))
					return false;
			}
			break;
		default:
			break;
	}

	return true;
}

ofc_sema_dummy_arg_list_t* ofc_sema_dummy_arg_list_copy_replace(
	const ofc_sema_dummy_arg_list_t* list,
	const ofc_sema_decl_t* replace,
	const ofc_sema_expr_t* with)
{
	if (!list) return NULL;

	ofc_sema_dummy_arg_list_t* copy
		= (ofc_sema_dummy_arg_list_t*)malloc(
			sizeof(ofc_sema_dummy_arg_list_t));
	if (!copy) return NULL;

	copy->dummy_arg = (ofc_sema_dummy_arg_t**)malloc(
		(sizeof(ofc_sema_dummy_arg_t*) * list->count));
	if (!copy->dummy_arg)
	{
		free(copy);
		return NULL;
	}

	copy->count = list->count;

	bool fail = false;
	unsigned i;
	for (i = 0; i < copy->count; i++)
	{
		const ofc_sema_dummy_arg_t* dummy_arg = list->dummy_arg[i];
		copy->dummy_arg[i] = ofc_sema_dummy_arg_copy_replace(
			dummy_arg, replace, with);
		if (copy->dummy_arg[i] == NULL)
			fail = true;
	}

	if (fail)
	{
		ofc_sema_dummy_arg_list_delete(copy);
		return NULL;
	}

	return copy;
}

bool ofc_sema_dummy_arg_list_compare(
	const ofc_sema_dummy_arg_list_t* a,
	const ofc_sema_dummy_arg_list_t* b)
{
	if (!a || !b)
		return false;

	if (a == b)
		return true;

	if (a->count != b->count)
		return false;

	unsigned i;
	for (i = 0; i < a->count; i++)
	{
		if (!ofc_sema_dummy_arg_compare(
			a->dummy_arg[i], b->dummy_arg[i]))
			return false;
	}

	return true;
}

bool ofc_sema_dummy_arg_print(
	ofc_colstr_t* cs,
	const ofc_sema_dummy_arg_t* dummy_arg)
{
	if (!cs || !dummy_arg)
		return false;

	if (dummy_arg->type == OFC_SEMA_DUMMY_ARG_EXPR)
	{
		if (!ofc_sema_expr_print(cs,
			dummy_arg->expr))
			return false;
	}
	else
	{
		if (!ofc_sema_external_arg_print(
			cs, dummy_arg->external))
			return false;
	}

	return true;
}

bool ofc_sema_dummy_arg_list_print(
	ofc_colstr_t* cs,
	const ofc_sema_dummy_arg_list_t* list)
{
	if (!cs || !list)
		return false;

	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		if (!ofc_sema_dummy_arg_print(
			cs, list->dummy_arg[i]))
			return false;

		if (i < (list->count - 1)
			&& !ofc_colstr_atomic_writef(cs, ", "))
			return false;
	}

	return true;
}

bool ofc_sema_dummy_arg_list_foreach_expr(
	ofc_sema_dummy_arg_list_t* list, void* param,
	bool (*func)(ofc_sema_expr_t* expr, void* param))
{
	if (!list || !func)
		return false;

	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		if (ofc_sema_dummy_arg_is_expr(
			list->dummy_arg[i]))
		{
			if (!ofc_sema_expr_foreach(
				list->dummy_arg[i]->expr,
				param, func))
				return false;
		}
	}

	return true;
}
