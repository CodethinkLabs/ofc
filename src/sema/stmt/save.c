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


bool ofc_sema_stmt_save(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_SAVE))
		return false;

	if (!stmt->save.list)
	{
		if (scope->save)
		{
			ofc_sparse_ref_warning(stmt->src,
				"Scope already marked as SAVE");
		}

		scope->save = true;
		return true;
	}

	unsigned i;
	for (i = 0; i < stmt->save.list->count; i++)
	{
		ofc_parse_save_t* save
			= stmt->save.list->save[i];
		if (!save) continue;

		if (save->is_common)
		{
			ofc_sema_common_t* common
				= ofc_sema_scope_common_find_create(
					scope, save->common);
			/* TODO - Warn when new COMMON block is created in SAVE */
			if (!common) return NULL;

			if (!ofc_sema_common_save(common))
				return false;
		}
		else
		{
			if (!save->lhs)
				return false;

			ofc_sparse_ref_t base_name;
			if (!ofc_parse_lhs_base_name(
				*(save->lhs), &base_name))
				return false;

			ofc_sema_decl_t* decl
				= ofc_sema_scope_decl_find_create(
					scope, base_name, true);
			if (!decl) return false;

			if (decl->is_automatic)
			{
				ofc_sparse_ref_error(save->lhs->src,
					"Can't SAVE an AUTOMATIC declaration");
				return false;
			}

			if (decl->is_static)
			{
				ofc_sparse_ref_warning(save->lhs->src,
					"Redundant definition of SAVE/STATIC attribute");
			}

			decl->is_static = true;
		}
	}

	return true;
}
