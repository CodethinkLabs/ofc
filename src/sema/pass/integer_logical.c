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


static bool ofc_sema_pass_integer_logical__expr(
	ofc_sema_expr_t* expr, void* param)
{
	(void)param;

	if (!expr)
		return false;

	if ((expr->type == OFC_SEMA_EXPR_CAST)
		&& ofc_sema_expr_type_is_integer(expr->cast.expr)
		&& (expr->cast.type->type == OFC_SEMA_TYPE_LOGICAL))
		/* Check explicitly for LOGICAL as we don't want to transform
		   casts to BYTE which can evaluate to LOGICAL.*/
	{
		ofc_sema_expr_t* a = expr->cast.expr;
		ofc_sema_expr_t* b = ofc_sema_expr_integer(0, OFC_SEMA_KIND_DEFAULT);

		if (!a || !b)
			return false;

		expr->type = OFC_SEMA_EXPR_NE;
		expr->a = a;
		expr->b = b;
	}

	return true;
}

static bool ofc_sema_pass_integer_logical__scope(
	ofc_sema_scope_t* scope, void* param)
{
	(void)param;

	if (!scope)
		return false;

	return ofc_sema_scope_foreach_expr(
		scope, NULL, ofc_sema_pass_integer_logical__expr);
}

bool ofc_sema_pass_integer_logical(
	ofc_sema_scope_t* scope)
{
	if (!scope)
		return false;

	return ofc_sema_scope_foreach_scope(
		scope, NULL, ofc_sema_pass_integer_logical__scope);
}
