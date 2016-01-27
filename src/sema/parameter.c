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


static ofc_sema_decl_t* ofc_sema_parameter__assign(
	ofc_sema_scope_t* scope,
	const ofc_parse_assign_t* assign)
{
	if (!scope || !assign)
		return NULL;

	ofc_sema_lhs_t* lhs = ofc_sema_lhs_local(
		scope, assign->name);
	if (!lhs) return NULL;

	ofc_sema_decl_t* decl = lhs->decl;
	ofc_sema_lhs_delete(lhs);
	if (!decl) return NULL;
	decl->is_parameter = true;

	ofc_sema_expr_t* init_expr = ofc_sema_expr(
		scope, assign->init);

	bool success = ofc_sema_decl_init(
		decl, init_expr);
	ofc_sema_expr_delete(init_expr);
	if (!success) return NULL;

	return decl;
}

bool ofc_sema_parameter(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt || (stmt->type != OFC_PARSE_STMT_PARAMETER))
		return false;

	unsigned count = stmt->parameter.list->count;
	if (count == 0)
		return false;

	unsigned i;
	for (i = 0; i < count; i++)
	{
		if (!ofc_sema_parameter__assign(
			scope, stmt->parameter.list->assign[i]))
			return false;
	}

	return true;
}
