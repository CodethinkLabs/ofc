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

	ofc_sema_decl_t* decl
		= ofc_sema_scope_decl_find_create(
			scope, base_name, true);
	if (!decl) return NULL;

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

	if (decl->type_implicit)
	{
		decl->type = type;
	}
	else
	{
		if ((decl->type->kind == 0)
			&& (type->kind != 0))
		{
			const ofc_sema_type_t* ntype
				= ofc_sema_type_set_kind(
					decl->type, type->kind);
			if (!ntype)
			{
				ofc_sema_expr_delete(init_expr);
				return NULL;
			}
			decl->type = ntype;
		}

		if ((decl->type->len == 0)
			&& ((type->len != 0) || type->len_var))
		{
			const ofc_sema_type_t* ntype
				= ofc_sema_type_set_len(
					decl->type, type->len, type->len_var);
			if (!ntype)
			{
				ofc_sema_expr_delete(init_expr);
				return NULL;
			}
			decl->type = ntype;
		}
	}

	if (!ofc_sema_decl_type_finalize(decl))
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
