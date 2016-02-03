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

	ofc_sparse_ref_t base_name;
	if (!ofc_parse_lhs_base_name(
		*(assign->name), &base_name))
		return NULL;

	ofc_sema_expr_t* init_expr = ofc_sema_expr(
		scope, assign->init);
	if (!init_expr) return NULL;

	const ofc_sema_type_t* type
		= ofc_sema_expr_type(init_expr);
	if (!type)
	{
		ofc_sema_expr_delete(init_expr);
		return NULL;
	}

	ofc_sema_spec_t* spec
		= ofc_sema_scope_spec_modify(
			scope, base_name);
	if (!spec)
	{
		ofc_sema_expr_delete(init_expr);
		return NULL;
	}

	if (spec->type_implicit)
	{
		spec->type = type->type;
		spec->type_implicit = false;
	}
	if (spec->kind == 0)
		spec->kind = type->kind;
	if ((spec->len == 0) && !spec->len_var)
	{
		spec->len      = type->len;
		spec->len_var  = type->len_var;
	}


	ofc_sema_lhs_t* lhs = ofc_sema_lhs_local(
		scope, assign->name);
	if (!lhs)
	{
		ofc_sema_expr_delete(init_expr);
		return NULL;
	}

	ofc_sema_decl_t* decl = lhs->decl;
	ofc_sema_lhs_delete(lhs);
	if (!decl)
	{
		ofc_sema_expr_delete(init_expr);
		return NULL;
	}
	decl->is_parameter = true;

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
