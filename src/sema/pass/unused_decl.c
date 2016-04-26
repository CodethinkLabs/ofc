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


static bool ofc_sema_pass_unused_decl__scope(
	ofc_sema_scope_t* scope, void* param)
{
	(void)param;

	if (!scope)
		return false;

	/* Skip module scopes and scopes without a decl list. */
	if (scope->decl && (scope->type != OFC_SEMA_SCOPE_MODULE))
		return true;

	unsigned i;
	for (i = 0; i < scope->decl->size; i++)
	{
		ofc_sema_decl_t* decl = scope->decl->decl[i];

		if (decl
			&& (decl->type->type != OFC_SEMA_TYPE_FUNCTION)
			&& (decl->type->type != OFC_SEMA_TYPE_SUBROUTINE)
			&& !decl->is_stmt_func_arg
			&& !decl->is_argument
			&& !decl->was_written
			&& !decl->was_read
			&& !decl->common)
		{
			ofc_sema_decl_list_remove(scope->decl, decl);
		}
	}

	return true;
}

bool ofc_sema_pass_unused_decl(
	ofc_sema_scope_t* scope)
{
	if (!scope)
		return false;

	return ofc_sema_scope_foreach_scope(
		scope, NULL, ofc_sema_pass_unused_decl__scope);
}
