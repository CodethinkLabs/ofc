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

bool ofc_sema_stmt_use(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_USE))
		return false;

	ofc_sema_scope_t* mscope
		= ofc_sema_scope_find_module_name(
			scope, stmt->use.module.string);

	if (!mscope)
	{
		ofc_sparse_ref_error(stmt->use.module,
			"Cannot find module");
		return false;
	}

	ofc_sema_decl_alias_map_t* rlist = NULL;

	if (stmt->use.rename)
	{
		rlist = ofc_sema_decl_alias_map_create();

		ofc_parse_lhs_list_t* rename
			= stmt->use.rename;

		unsigned i;
		for (i = 0; i < rename->count; i++)
		{
			if (rename->lhs[i]->type
				!= OFC_PARSE_LHS_ALIAS)
				return false;

			ofc_str_ref_t tname
				= rename->lhs[i]->alias.target.string;

			ofc_sema_decl_t* tdecl
				= ofc_sema_scope_decl_find_modify(
					mscope, tname, true);

			if (!tdecl)
			{
				ofc_sparse_ref_error(
					rename->lhs[i]->alias.target,
						"No declaration in module");
				return false;
			}

			ofc_str_ref_t rname
				= rename->lhs[i]->alias.name.string;

			ofc_sema_decl_alias_t* alias
				= ofc_sema_decl_alias_create(rname, tdecl);

			if (!ofc_sema_decl_alias_map_add(rlist, alias))
			{
				ofc_sema_decl_alias_delete(alias);
				return false;
			}
		}
	}

	ofc_sema_decl_list_t* olist = NULL;

	if (stmt->use.only)
	{
		olist = ofc_sema_decl_list_create(false);

		ofc_parse_decl_list_t* only
			= stmt->use.only;

		unsigned i;
		for (i = 0; i < only->count; i++)
		{
			ofc_sparse_ref_t sparse_ref
				= only->decl[i]->lhs->variable;

			ofc_str_ref_t name
				= sparse_ref.string;

			ofc_sema_decl_t* decl
				= ofc_sema_scope_decl_find_modify(
					mscope, name, true);

			if (!decl)
			{
				ofc_sparse_ref_error(sparse_ref,
					"No declaration in module");
				return false;
			}

			if (!ofc_sema_decl_list_add(olist, decl))
				return false;
		}
	}

	ofc_sema_module_t* module
		= ofc_sema_module_create(
			mscope, olist, rlist);

	if (!module) return false;

	if (!scope->module)
		scope->module = ofc_sema_module_list_create();

	ofc_sema_module_list_add(
		scope->module, module);

	return true;
}
