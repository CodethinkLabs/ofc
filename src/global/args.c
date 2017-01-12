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

 #include <ofc/global.h>

typedef struct
{
	const ofc_sema_decl_t*     subr;
	ofc_sema_dummy_arg_list_t* args;
	ofc_sema_expr_t*       ret;
} ofc_call_t;

typedef struct
{
	ofc_str_ref_t name;
	unsigned      count;
	ofc_call_t**  call;
} ofc_subroutine_list_t;

ofc_call_t* ofc_call_create(
	const ofc_sema_decl_t*     subr,
	ofc_sema_dummy_arg_list_t* args,
	ofc_sema_expr_t*           ret)
{
	ofc_call_t* call
		= (ofc_call_t*)malloc(
			sizeof(ofc_call_t));
	if (!call) return NULL;

	call->subr = subr;
	call->args = args;
	call->ret  = ret;

	return call;
}

static ofc_subroutine_list_t* ofc_subroutine_list_create(
	ofc_str_ref_t name, ofc_call_t* call)
{
	if (!call) return NULL;

	ofc_subroutine_list_t* list
		= (ofc_subroutine_list_t*)malloc(
			sizeof(ofc_subroutine_list_t));
	if (!list) return NULL;

	list->call
		= (ofc_call_t**)malloc(
			sizeof(ofc_call_t*));
	if (!list->call)
	{
		free(list);
		return NULL;
	}

	list->name = name;
	list->count = 1;
	list->call[0] = call;
	return list;
}

static void ofc_subroutine_list_delete(
	ofc_subroutine_list_t* list)
{
	if (!list) return;

	unsigned i;
	for (i = 0; i < list->count; i++)
		free(list->call[i]);
	free(list->call);
	free(list);
}

static ofc_str_ref_t* ofc_subroutine_list_name(
	ofc_subroutine_list_t* list)
{
	return (list ? &list->name : NULL);
}

static bool ofc_subroutine_list_add(
	ofc_subroutine_list_t* list, const ofc_sema_decl_t* subr,
	ofc_sema_dummy_arg_list_t* args, ofc_sema_expr_t* ret)
{
	if (!list || !subr)
		return false;

	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		if (list->call[i]->args == args)
			return true;
	}

	ofc_call_t* call
		= ofc_call_create(subr, args, ret);
	if (!call) return false;

	ofc_call_t** ncall
		= (ofc_call_t**)realloc(list->call,
			(sizeof(ofc_call_t*) * (list->count + 1)));
	if (!ncall) return false;
	list->call = ncall;

	list->call[list->count++] = call;
	return true;
}

static bool ofc_global_pass_args__stmt(
	ofc_sema_stmt_t* stmt,
	ofc_hashmap_t* args_table)
{
	if (!stmt || !args_table)
		return false;

	switch (stmt->type)
	{
		case OFC_SEMA_STMT_CALL:
			break;
		default:
			return true;
	}

	ofc_subroutine_list_t* list
		= ofc_hashmap_find_modify(
			args_table, &stmt->call.subroutine->name.string);
	if (list)
	{
		if (!ofc_subroutine_list_add(
			list, stmt->call.subroutine, stmt->call.args, NULL))
			return false;
	}
	else
	{
		ofc_call_t* call = ofc_call_create(
			stmt->call.subroutine, stmt->call.args, NULL);
		if (!call) return false;

		list = ofc_subroutine_list_create(
			stmt->call.subroutine->name.string, call);
		if (!list)
		{
			free(call);
			return false;
		}

		if (!ofc_hashmap_add(args_table, list))
		{
			ofc_subroutine_list_delete(list);
			return false;
		}
	}

	return true;
}

static bool ofc_global_pass_args__scope_call(
	ofc_sema_scope_t* scope,
	ofc_hashmap_t* args_table)
{
	if (!scope || !args_table)
		return false;

	if (!ofc_sema_scope_foreach_stmt(
		scope, args_table, (void*)ofc_global_pass_args__stmt))
	{
		return false;
	}

	return true;
}

static bool ofc_global_pass_args__expr(
	ofc_sema_expr_t* expr,
	ofc_hashmap_t* args_table)
{
	if (!expr || !args_table)
		return false;

	if (expr->type != OFC_SEMA_EXPR_FUNCTION)
		return true;

	ofc_subroutine_list_t* list
		= ofc_hashmap_find_modify(
			args_table, &expr->function->name.string);
	if (list)
	{
		if (!ofc_subroutine_list_add(
			list, expr->function, expr->args, expr))
			return false;
	}
	else
	{
		ofc_call_t* call = ofc_call_create(
			expr->function, expr->args, expr);
		if (!call) return false;

		list = ofc_subroutine_list_create(
			expr->function->name.string, call);
		if (!list)
		{
			free(call);
			return false;
		}

		if (!ofc_hashmap_add(args_table, list))
		{
			ofc_subroutine_list_delete(list);
			return false;
		}
	}

	return true;
}

static bool ofc_global_pass_args__scope_func(
	ofc_sema_scope_t* scope,
	ofc_hashmap_t* args_table)
{
	if (!scope || !args_table)
		return false;

	if (!ofc_sema_scope_foreach_expr(
		scope, args_table, (void*)ofc_global_pass_args__expr))
	{
		return false;
	}

	return true;
}

static bool ofc_global_pass_args__check(
	ofc_subroutine_list_t* list, ofc_sema_scope_t* scope)
{
	if (!list || !scope)
		return false;

	if ((scope->type != OFC_SEMA_SCOPE_SUBROUTINE)
		&& (scope->type != OFC_SEMA_SCOPE_FUNCTION))
		return false;

	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		if (scope->args)
		{
			if (scope->args->count != list->call[i]->args->count)
			{
				ofc_sparse_ref_warning(list->call[i]->subr->name,
					"Wrong number of arguments in %s call",
					ofc_sema_type_str_rep(list->call[i]->subr->type));
				continue;
			}

			unsigned l;
			for (l = 0; l < scope->args->count; l++)
			{
				ofc_sema_arg_t dummy_arg
					= scope->args->arg[l];
				ofc_sema_dummy_arg_t* actual_arg
					= list->call[i]->args->dummy_arg[l];

				if (dummy_arg.alt_return
					&& ofc_sema_dummy_arg_is_alt_return(actual_arg))
					continue;

				if (dummy_arg.alt_return
					&& !ofc_sema_dummy_arg_is_alt_return(actual_arg))
				{
					ofc_sparse_ref_warning(actual_arg->src,
						"Incompatible argument in %s call, expected label for alternate return.",
						ofc_sema_type_str_rep(list->call[i]->subr->type));
					continue;
				}
				else if (!dummy_arg.alt_return
					&& ofc_sema_dummy_arg_is_alt_return(actual_arg))
				{
					const ofc_sema_decl_t* dummy_arg_decl
						= ofc_sema_scope_decl_find(
							scope, dummy_arg.name.string, true);
					const ofc_sema_type_t* dummy_arg_type
						= ofc_sema_decl_type(dummy_arg_decl);

					ofc_sparse_ref_warning(actual_arg->src,
						"Incompatible alternate return argument in %s call, expected %s.",
						ofc_sema_type_str_rep(list->call[i]->subr->type),
						ofc_sema_type_str_rep(dummy_arg_type));
					continue;
				}

				/* Find the declaration of the argument in the function scope */
				const ofc_sema_decl_t* dummy_arg_decl
					= ofc_sema_scope_decl_find(
						scope, dummy_arg.name.string, true);
				/* Not all arguments are declared */
				if (!dummy_arg_decl) continue;

				const ofc_sema_type_t* dummy_arg_type
					= ofc_sema_decl_type(dummy_arg_decl);


				if (ofc_sema_dummy_arg_is_external(actual_arg))
				{
					if (!ofc_sema_type_is_function(dummy_arg_type)
						&& !ofc_sema_type_is_subroutine(dummy_arg_type))
					{
						ofc_sparse_ref_warning(actual_arg->src,
							"Incompatible argument type (EXTERNAL) in %s call, expected %s.",
							ofc_sema_type_str_rep(list->call[i]->subr->type),
							ofc_sema_type_str_rep(dummy_arg_type));
					}
					continue;
				}

				const ofc_sema_type_t* actual_arg_type
					= ofc_sema_expr_type(actual_arg->expr);
				if (!ofc_sema_type_compatible(actual_arg_type, dummy_arg_type))
				{
					if (!ofc_sema_type_cast_valid(
						actual_arg_type, dummy_arg_type))
					{
						ofc_sparse_ref_warning(actual_arg->src,
							"Incompatible argument type (%s) in %s call, expected %s.",
							ofc_sema_type_str_rep(actual_arg_type),
							ofc_sema_type_str_rep(list->call[i]->subr->type),
							ofc_sema_type_str_rep(dummy_arg_type));
					}
					else if (!ofc_sema_type_cast_is_lossless(
						actual_arg_type, dummy_arg_type))
					{
						ofc_sparse_ref_warning(actual_arg->src,
							"Argument cast from %s to %s may be lossy in %s call",
							ofc_sema_type_str_rep(actual_arg_type),
							ofc_sema_type_str_rep(dummy_arg_type),
							ofc_sema_type_str_rep(list->call[i]->subr->type));
					}
				}

				if ((actual_arg->type == OFC_SEMA_DUMMY_ARG_EXPR)
					&& ofc_sema_expr_is_constant(actual_arg->expr)
					&& dummy_arg_decl->was_written)
				{
					ofc_sparse_ref_warning(actual_arg->src,
						"Constant reference may be written to in %s call",
						ofc_sema_type_str_rep(list->call[i]->subr->type));
				}
			}
		}

		if (scope->type == OFC_SEMA_SCOPE_FUNCTION)
		{
			const ofc_sema_decl_t* func_decl
				= ofc_sema_scope_decl_find(
					scope, scope->name, true);
			const ofc_sema_type_t* func_type
				= ofc_sema_decl_type(func_decl);
			const ofc_sema_type_t* ret_type
				= ofc_sema_expr_type(list->call[i]->ret);

			if (!ofc_sema_type_compatible(func_type, ret_type))
			{
				if (!ofc_sema_type_cast_valid(
					func_type, ret_type))
				{
					ofc_sparse_ref_warning(list->call[i]->ret->src,
						"Function return type is %s, expected %s.",
						ofc_sema_type_str_rep(func_type),
						ofc_sema_type_str_rep(ret_type));
				}
				else if (!ofc_sema_type_cast_is_lossless(
					func_type, ret_type))
				{
					ofc_sparse_ref_warning(list->call[i]->ret->src,
						"Cast of function from %s to %s may be lossy.",
						ofc_sema_type_str_rep(func_type),
						ofc_sema_type_str_rep(ret_type));
				}
			}
		}
	}

	return true;
}

static bool ofc_global_pass_args__scope_decl(
	ofc_sema_scope_t* scope,
	ofc_hashmap_t* args_table)
{
	if ((scope->type != OFC_SEMA_SCOPE_SUBROUTINE)
		&& (scope->type != OFC_SEMA_SCOPE_FUNCTION))
		return true;

	ofc_subroutine_list_t* list
		= ofc_hashmap_find_modify(
			args_table, &scope->name);
	if (list)
	{
		ofc_global_pass_args__check(list, scope);
	}
	else if (global_opts.warn_unused_procedure)
	{
		ofc_file_warning(NULL, NULL, "Unused %s '%.*s'",
			((scope->type == OFC_SEMA_SCOPE_SUBROUTINE) ? "SUBROUTINE" : "FUNCTION"),
			scope->name.size, scope->name.base);
	}

	/* What about the calls that don't have a function declaration? */
	return true;
}


bool ofc_global_pass_args(
	ofc_sema_scope_t* scope)
{
	if (!scope)
		return false;

	ofc_hashmap_t* args_table
		= ofc_hashmap_create(
			(void*)ofc_str_ref_ptr_hash,
			(void*)ofc_str_ref_ptr_equal_ci,
			(void*)ofc_subroutine_list_name,
			(void*)ofc_subroutine_list_delete);
	if (!args_table) return false;

	/* Find all the subroutines */
	if (!ofc_sema_scope_foreach_scope(
		scope, args_table,
		(void*)ofc_global_pass_args__scope_call))
		return false;

	/* Find all the functions */
	if (!ofc_sema_scope_foreach_scope(
		scope, args_table,
		(void*)ofc_global_pass_args__scope_func))
		return false;

	bool success = ofc_sema_scope_foreach_scope(
		scope, args_table, (void*)ofc_global_pass_args__scope_decl);
	ofc_hashmap_delete(args_table);
	return success;
}
