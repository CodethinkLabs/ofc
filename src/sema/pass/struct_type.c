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


static bool ofc_sema_pass_struct_type__struct(
	ofc_sema_structure_t* structure, ofc_sema_scope_t* scope)
{
	if (!structure || !scope)
		return false;

	if (structure->type != OFC_SEMA_STRUCTURE_VAX_STRUCTURE)
		return true;

	if (ofc_sema_structure_is_nested(structure))
	{
		ofc_sparse_ref_warning(structure->name,
			"VAX STRUCTURE is nested an can't be converted to a TYPE");
	}
	else
	{
		if (!ofc_sema_structure_list_add(
			scope->derived_type, structure))
			return false;

		structure->type = OFC_SEMA_STRUCTURE_F90_TYPE_SEQUENCE;

		ofc_sema_structure_list_remove(
			scope->structure, structure);
	}

	return true;
}

static bool ofc_sema_pass_struct_type__scope(
	ofc_sema_scope_t* scope, void* param)
{
	(void)param;

	if (!scope)
		return false;

	if (!scope->structure)
		return true;

	return ofc_sema_structure_list_foreach(
		scope->structure, (void*)scope,
		(void*)ofc_sema_pass_struct_type__struct);
}

bool ofc_sema_pass_struct_type(
	ofc_sema_scope_t* scope)
{
	if (!scope)
		return false;

	return ofc_sema_scope_foreach_scope(
		scope, NULL, ofc_sema_pass_struct_type__scope);
}
