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

static bool ofc_sema_pass_integer_logical__is_logical_cast(
	ofc_sema_expr_t* expr)
{
	return (expr->type == OFC_SEMA_EXPR_CAST)
		&& (expr->cast.type->type == OFC_SEMA_TYPE_LOGICAL
		&& ofc_sema_expr_type_is_integer(expr->cast.expr));
}

static bool ofc_sema_pass_integer_logical__expand(
	ofc_sema_expr_t* expr)
{
	ofc_sema_expr_t* a = expr->cast.expr;
	ofc_sema_expr_t* b = ofc_sema_expr_integer(0, OFC_SEMA_KIND_DEFAULT);
	/* TODO - Work out side a kind and use the same for side b*/

	if (!a || !b) return false;

	expr->type = OFC_SEMA_EXPR_NE;
	expr->a = a;
	expr->b = b;

	return true;
}

static bool ofc_sema_pass_integer_logical__expr(
	ofc_sema_expr_t* expr, void* param)
{
	(void)param;

	if (!expr) return false;

	if (expr->type == OFC_SEMA_EXPR_NOT)
	{
		ofc_sema_expr_t* sub_expr = expr->a;

		if (ofc_sema_pass_integer_logical__is_logical_cast(sub_expr))
		{
			if (!ofc_sema_pass_integer_logical__expand(sub_expr))
				return false;

			expr->type = OFC_SEMA_EXPR_EQ;
			expr->a = ofc_sema_expr_copy(sub_expr->a);
			expr->b = ofc_sema_expr_copy(sub_expr->b);

			ofc_sema_expr_delete(sub_expr);
		}
	}
	else if (ofc_sema_pass_integer_logical__is_logical_cast(expr))
	{
		if (!ofc_sema_pass_integer_logical__expand(expr))
			return false;
	}

	return true;
}

static bool ofc_sema_pass_integer_logical__scope(
	ofc_sema_scope_t* scope, void* param)
{
	(void)param;

	if (!scope) return false;

	return ofc_sema_scope_foreach_expr(
		scope, NULL, ofc_sema_pass_integer_logical__expr);
}

bool ofc_sema_pass_integer_logical(
	ofc_sema_scope_t* scope)
{
	if (!scope) return false;

	return ofc_sema_scope_foreach_scope(
		scope, NULL, ofc_sema_pass_integer_logical__scope);
}
