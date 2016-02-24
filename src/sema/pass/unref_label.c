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

static bool ofc_sema_pass_unref_label__scope(
	ofc_sema_scope_t* scope, void* param)
{
	(void)param;

	if (!scope)
		return false;

	if (scope->label)
	{
		unsigned i;
		for (i = 0; i < scope->label->size; i++)
		{
			ofc_sema_label_t* label = scope->label->label[i];

			if (label && !label->used)
				ofc_sema_label_map_remove(scope->label, label);
		}
	}
	return true;
}

bool ofc_sema_pass_unref_label(
	ofc_sema_scope_t* scope)
{
	if (!scope)
		return false;

	return ofc_sema_scope_foreach_scope(
		scope, NULL, ofc_sema_pass_unref_label__scope);
}
