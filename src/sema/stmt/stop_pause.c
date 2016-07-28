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

ofc_sema_stmt_t* ofc_sema_stmt_stop_pause(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt)
		return NULL;

	ofc_sema_stmt_t s;
	switch (stmt->type)
	{
		case OFC_PARSE_STMT_STOP:
			s.type = OFC_SEMA_STMT_STOP;
			break;
		case OFC_PARSE_STMT_PAUSE:
			s.type = OFC_SEMA_STMT_PAUSE;
			break;
		default:
			return NULL;
	}

	ofc_sema_expr_t* expr
		= ofc_sema_expr(
			scope, stmt->stop_pause_return.value);
	if (expr)
	{
		const ofc_sema_type_t* type
			= ofc_sema_expr_type(expr);
		if (!type)
		{
			ofc_sema_expr_delete(expr);
			return NULL;
		}

		if (type->type != OFC_SEMA_TYPE_CHARACTER)
		{
			if (!ofc_sema_type_is_integer(type))
			{
				ofc_sparse_ref_error(expr->src,
					"STOP/PAUSE code must be a string or an integer");
				ofc_sema_expr_delete(expr);
				return NULL;
			}

			if (ofc_sema_expr_is_constant(expr))
			{
				unsigned v;
				if (!ofc_sema_expr_resolve_uint(expr, &v)
					|| (v >= 100000))
				{
					ofc_sparse_ref_warning(expr->src,
						"STOP/PAUSE code should be a positive integer"
						" less than 5 digits long");
				}
			}
		}
	}
	else if (stmt->stop_pause_return.value)
	{
		/* Failed to resolve expression. */
		return NULL;
	}
	s.stop_pause.str = expr;

	ofc_sema_stmt_t* as
		= ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_delete(
			s.stop_pause.str);
		return NULL;
	}

	return as;
}

bool ofc_sema_stmt_stop_pause_print(ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt)
{
	if (!cs || !stmt) return false;

	if (stmt->type == OFC_SEMA_STMT_STOP)
	{
		if (!ofc_colstr_keyword_atomic_writef(cs, "STOP"))
			return false;
	}
	else if (stmt->type == OFC_SEMA_STMT_PAUSE)
	{
		if (!ofc_colstr_keyword_atomic_writef(cs, "PAUSE"))
			return false;
	}
	else
	{
		return false;
	}

	if (stmt->stop_pause.str)
	{
		if (!ofc_sema_expr_print(cs, stmt->stop_pause.str))
			return false;
	}

	return true;
}
