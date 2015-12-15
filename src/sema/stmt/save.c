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
		/* TODO - Mark all decl's in scope as static. */
		return false;
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
					scope, save->common.string);
			/* TODO - Warn when new COMMON block is created in SAVE */
			if (!common) return NULL;

			if (!ofc_sema_common_save(common))
			{
				/* TODO - Allow this. */
				ofc_sparse_ref_error(save->common,
					"Can't mark a COMMON block as STATIC after use");
				return false;
			}
		}
		else
		{
			if (!save->lhs)
				return false;

			ofc_sparse_ref_t base_name;
			if (!ofc_parse_lhs_base_name(
				*(save->lhs), &base_name))
				return false;

			const ofc_sema_decl_t* decl
				= ofc_sema_scope_decl_find(
					scope, base_name.string, true);
			if (decl)
			{
				/* TODO - Allow application of STATIC attribute to decl. */
				ofc_sparse_ref_error(save->lhs->src,
					"Using SAVE on existing declaration not yet supported");
				return false;
			}

			ofc_sema_spec_t* spec
				= ofc_sema_scope_spec_modify(
					scope, base_name);
			if (!spec) return false;

			if (spec->is_automatic)
			{
				ofc_sparse_ref_error(save->lhs->src,
					"Can't SAVE an AUTOMATIC variable");
				return false;
			}

			if (spec->is_static)
			{
				ofc_sparse_ref_warning(save->lhs->src,
					"SAVE on STATIC variable is redundant");
			}

			spec->is_static = true;
		}
	}

	return true;
}
