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


bool ofc_sema_stmt_common(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_COMMON)
		|| !stmt->common_namelist)
		return false;

	unsigned g;
	for (g = 0; g < stmt->common_namelist->count; g++)
	{
		ofc_parse_common_group_t* group
			= stmt->common_namelist->group[g];
		if (!group || !group->names)
			continue;

		ofc_sema_common_t* common
			= ofc_sema_scope_common_find_create(
				scope, group->group);
		if (!common) return false;

		unsigned i;
		for (i = 0; i < group->names->count; i++)
		{
			ofc_parse_lhs_t* lhs
				= group->names->lhs[i];
			if (!lhs) continue;

			ofc_str_ref_t base_name;
			if (!ofc_parse_lhs_base_name(
				*lhs, &base_name))
				return false;

			ofc_sema_array_t* array = NULL;
			if (lhs->type == OFC_PARSE_LHS_ARRAY)
			{
				array = ofc_sema_array(
					scope, lhs->array.index);
				if (!array)
				{
					ofc_sema_scope_error(scope, lhs->src,
						"Invalid array index in COMMON list");
					return false;
				}

				lhs = lhs->parent;
				if (!lhs)
				{
					ofc_sema_array_delete(array);
					return false;
				}
			}

			if (lhs->type != OFC_PARSE_LHS_VARIABLE)
			{
				ofc_sema_scope_error(scope, lhs->src,
					"Invalid entry in COMMON list");
				ofc_sema_array_delete(array);
				return false;
			}

			ofc_sema_spec_t* spec
				= ofc_sema_scope_spec_modify(
					scope, base_name);
			if (!spec)
			{
				ofc_sema_array_delete(array);
				return false;
			}

			if (spec->common
				&& (spec->common != common))
			{
				ofc_sema_scope_error(scope, lhs->src,
					"Specifier used in multiple COMMON blocks");
				ofc_sema_array_delete(array);
				return false;
			}

			if (array)
			{
				if (spec->array)
				{
					bool conflict = !ofc_sema_array_compare(
						spec->array, array);
					ofc_sema_array_delete(array);
					if (conflict)
					{
						ofc_sema_scope_error(scope, lhs->src,
							"Conflicting array definition in COMMON list");
						return false;
					}
				}
				else
				{
					spec->array = array;
				}
			}

			if (!spec->common)
			{
				unsigned offset = common->count;
				if (!ofc_sema_common_add(common, spec))
					return false;

				spec->common        = common;
				spec->common_offset = offset;
			}
		}
	}

	return true;
}
