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

	ofc_sema_call_list_t* list
		= ofc_hashmap_find_modify(
			args_table, &stmt->call->subr->name.string);
	if (list)
	{
		if (!ofc_sema_call_list_add(
			list, stmt->call->subr, stmt->call->args, stmt->call->ret))
			return false;
	}
	else
	{
		ofc_sema_call_t* call = ofc_sema_call_create(
			stmt->call->subr, stmt->call->args, stmt->call->ret);
		if (!call) return false;

		list = ofc_sema_call_list_create(
			stmt->call->subr->name.string, call);
		if (!list)
		{
			free(call);
			return false;
		}

		if (!ofc_hashmap_add(args_table, list))
		{
			ofc_sema_call_list_delete(list);
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

	ofc_sema_call_list_t* list
		= ofc_hashmap_find_modify(
			args_table, &expr->function->name.string);
	if (list)
	{
		if (!ofc_sema_call_list_add(
			list, expr->function, expr->args, expr))
			return false;
	}
	else
	{
		ofc_sema_call_t* call = ofc_sema_call_create(
			expr->function, expr->args, expr);
		if (!call) return false;

		list = ofc_sema_call_list_create(
			expr->function->name.string, call);
		if (!list)
		{
			free(call);
			return false;
		}

		if (!ofc_hashmap_add(args_table, list))
		{
			ofc_sema_call_list_delete(list);
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
	ofc_sema_call_list_t* list, ofc_sema_scope_t* scope)
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
				if (!dummy_arg_decl) return false;

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

/* For each external that doesn't have a decl in a scope
   look if there's a subroutine or function definition with
   the same name and create a decl for it in this scope */
static bool ofc_global_pass_args__scope_external_finalize(
	ofc_sema_scope_t* scope,
	ofc_hashmap_t* args_table)
{
	if (!scope || !args_table)
		return false;

	if (!scope->external)
		return true;

	unsigned i;
	for (i = 0; i < scope->external->count; i++)
	{
		/* If the external doesn't have a declaration or the declaration
		   is not final we need to create one or set the proper type */
		if (!ofc_sema_external_is_decl(scope->external->external[i])
			|| (ofc_sema_external_is_decl(scope->external->external[i])
				&& !ofc_sema_decl_is_final(scope->external->external[i]->decl)))
		{
			/* If we can't find the external in this list then we still
			   don't know what it is */
			ofc_sema_call_list_t* list
				= ofc_hashmap_find_modify(
					args_table, &scope->external->external[i]->name.string);
			if (list)
			{
				ofc_sema_decl_t* decl
					= ofc_sema_scope_decl_find_create(
						scope, scope->external->external[i]->name, false);
				if (!decl) return false;

				if (ofc_sema_decl_is_subroutine(list->call[0]->subr))
				{
					if (!ofc_sema_decl_subroutine(decl))
					{
						ofc_sema_decl_delete(decl);
						return false;
					}
				}
				else if (ofc_sema_decl_is_function(list->call[0]->subr))
				{
					if (!ofc_sema_decl_function(decl))
					{
						ofc_sema_decl_delete(decl);
						return false;
					}
				}
				else
				{
					ofc_file_warning(NULL, NULL, "Unused EXTERNAL '%.*s'",
						decl->name.string.size, decl->name.string.base);
				}

				if (!ofc_sema_decl_type_finalize(decl))
				{
					ofc_sema_decl_delete(decl);
					return false;
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

	ofc_sema_call_list_t* list
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
			(void*)ofc_sema_call_list_name,
			(void*)ofc_sema_call_list_delete);
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

	/* For each scope, see if for every call, all the arguments
	that are external and don't have a decl are defined in the args_table */
	if (!ofc_sema_scope_foreach_scope(
		scope, args_table,
		(void*)ofc_global_pass_args__scope_external_finalize))
		return false;

	bool success = ofc_sema_scope_foreach_scope(
		scope, args_table, (void*)ofc_global_pass_args__scope_decl);
	ofc_hashmap_delete(args_table);
	return success;
}
