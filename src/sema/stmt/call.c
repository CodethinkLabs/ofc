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


ofc_sema_stmt_t* ofc_sema_stmt_call(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_CALL))
		return NULL;

	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_CALL;

	ofc_sema_decl_t* subroutine
		= ofc_sema_scope_decl_find_create(
			scope, stmt->call_entry.name, false);

	if (!ofc_sema_decl_is_subroutine(subroutine)
		&& !ofc_sema_decl_subroutine(subroutine))
	{
		ofc_sparse_ref_error(stmt->src,
			"CALL target must be a valid SUBROUTINE");
		return NULL;
	}

	if (!ofc_sema_decl_type_finalize(subroutine))
		return NULL;
	ofc_sema_decl_mark_used(subroutine, false, true);
	s.call.subroutine = subroutine;

	s.call.args = NULL;
	if (stmt->call_entry.args)
	{
		s.call.args = ofc_sema_expr_list_create();
		if (!s.call.args) return NULL;

		unsigned i;
		for (i = 0; i < stmt->call_entry.args->count; i++)
		{
			ofc_parse_call_arg_t* arg
				= stmt->call_entry.args->call_arg[i];
			if (!arg)
			{
				ofc_sema_expr_list_delete(s.call.args);
				return NULL;
			}

			if (!ofc_sparse_ref_empty(arg->name))
			{
				ofc_sparse_ref_error(stmt->src,
					"CALL arguments musn't be named");
				ofc_sema_expr_list_delete(s.call.args);
				return NULL;
			}

			switch (arg->type)
			{
				case OFC_PARSE_CALL_ARG_EXPR:
				case OFC_PARSE_CALL_ARG_RETURN:
					break;
				default:
					ofc_sparse_ref_error(stmt->src,
						"CALL arguments must be an expression or return label");
					ofc_sema_expr_list_delete(s.call.args);
					return NULL;
			}

			ofc_sema_expr_t* expr;
			if (arg->type == OFC_PARSE_CALL_ARG_RETURN)
			{
				expr = ofc_sema_expr_alt_return(
					scope, arg->expr);
			}
			else
			{
				expr = ofc_sema_expr_dummy_arg(
					scope, arg->expr);
			}
			if (!expr)
			{
				ofc_sema_expr_list_delete(s.call.args);
				return NULL;
			}

			if (!ofc_sema_expr_list_add(
				s.call.args, expr))
			{
				ofc_sema_expr_delete(expr);
				ofc_sema_expr_list_delete(s.call.args);
				return NULL;
			}
		}
	}

	ofc_sema_stmt_t* as
		= ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_list_delete(s.call.args);
		return NULL;
	}

	return as;
}

bool ofc_sema_stmt_call_print(
	ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt)
{
	if (!cs || (stmt->type != OFC_SEMA_STMT_CALL))
		return false;

	if (!ofc_colstr_atomic_writef(cs, "CALL")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_sema_decl_print_name(cs, stmt->call.subroutine))
		return false;

	if (stmt->call.args
		&& (!ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_atomic_writef(cs, "(")
			|| !ofc_sema_expr_list_print(cs, stmt->call.args)
			|| !ofc_colstr_atomic_writef(cs, ")")))
		return false;

	return true;
}
