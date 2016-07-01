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


static bool ofc_sema_pass_unused_common__scope(
	ofc_sema_scope_t* scope, void* param)
{
	(void)param;

	if (!scope)
		return false;

	/* Skip module scopes and scopes without a common list. */
	if (!scope->common)
		return true;

	/* TODO - Remove unused COMMON blocks from scope. */

	unsigned i;
	for (i = 0; i < scope->common->count; i++)
	{
		const ofc_sema_common_t* common
			= scope->common->common[i];
		if (!common) continue;

		unsigned j;
		for (j = 0; j < common->count; j++)
		{
			const ofc_sema_decl_t* decl
				= common->decl[j];
			if (!decl) continue;

			if (decl->was_read
				|| decl->was_written)
				break;
		}
		if (j >= common->count)
		{
			ofc_sparse_ref_warning(scope->src,
				"COMMON block '%.*s' unused in scope",
				common->name.size, common->name.base);
		}
	}

	return true;
}

bool ofc_sema_pass_unused_common(
	ofc_sema_scope_t* scope)
{
	if (!scope)
		return false;

	return ofc_sema_scope_foreach_scope(
		scope, NULL, ofc_sema_pass_unused_common__scope);
}
