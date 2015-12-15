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


bool ofc_sema_stmt_decl_attr(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt)
		return false;

	unsigned i;
	for (i = 0; i < stmt->decl_attr.count; i++)
	{
		ofc_sparse_ref_t decl_name = *stmt->decl_attr.name[i];

		const ofc_sema_decl_t* decl
			= ofc_sema_scope_decl_find(
				scope, decl_name.string, true);
		if (decl)
		{
			/* TODO - Apply attributes to decl. */
			ofc_sparse_ref_error(stmt->src,
				"Declaration attributes must be specified prior to use");
			return false;
		}

		ofc_sema_spec_t* spec
			= ofc_sema_scope_spec_modify(
				scope, decl_name);
		if (!spec)
		{
			ofc_sparse_ref_error(stmt->src,
				"No declaration for '%.*s' and no valid IMPLICIT rule",
				decl_name.string.size, decl_name.string.base);
			return false;
		}

		switch (stmt->type)
		{
			case OFC_PARSE_STMT_DECL_ATTR_AUTOMATIC:
				if (spec->is_static)
				{
					ofc_sparse_ref_error(stmt->src,
						"Specifying '%.*s' as STATIC and AUTOMATIC",
						decl_name.string.size, decl_name.string.base);
					return false;
				}
				if (spec->is_automatic)
				{
					ofc_sparse_ref_warning(stmt->src,
						"Re-declaring '%.*s' as AUTOMATIC",
						decl_name.string.size, decl_name.string.base);
				}
				spec->is_automatic = true;
				break;
			case OFC_PARSE_STMT_DECL_ATTR_STATIC:
				if (spec->is_automatic)
				{
					ofc_sparse_ref_error(stmt->src,
						"Specifying '%.*s' as AUTOMATIC and STATIC",
						decl_name.string.size, decl_name.string.base);
					return false;
				}
				if (spec->is_static)
				{
					ofc_sparse_ref_warning(stmt->src,
						"Re-declaring '%.*s' as STATIC",
						decl_name.string.size, decl_name.string.base);
				}
				spec->is_static = true;
				break;
			case OFC_PARSE_STMT_DECL_ATTR_VOLATILE:
				if (spec->is_volatile)
				{
					ofc_sparse_ref_warning(stmt->src,
						"Re-declaring '%.*s' as VOLATILE",
						decl_name.string.size, decl_name.string.base);
				}
				spec->is_volatile = true;
				break;
			case OFC_PARSE_STMT_DECL_ATTR_EXTERNAL:
				if (spec->is_intrinsic)
				{
					ofc_sparse_ref_error(stmt->src,
						"Specifying '%.*s' as INTRINSIC and EXTERNAL",
						decl_name.string.size, decl_name.string.base);
					return false;
				}
				if (spec->is_external)
				{
					ofc_sparse_ref_warning(stmt->src,
						"Re-declaring '%.*s' as EXTERNAL",
						decl_name.string.size, decl_name.string.base);
				}
				spec->is_external = true;
				break;
			case OFC_PARSE_STMT_DECL_ATTR_INTRINSIC:
				if (spec->is_external)
				{
					ofc_sparse_ref_error(stmt->src,
						"Specifying '%.*s' as EXTERNAL and INTRINSIC",
						decl_name.string.size, decl_name.string.base);
					return false;
				}
				if (spec->is_intrinsic)
				{
					ofc_sparse_ref_warning(stmt->src,
						"Re-declaring '%.*s' as INTRINSIC",
						decl_name.string.size, decl_name.string.base);
				}
				spec->is_intrinsic = true;
				break;
			default:
				return false;
		}
	}

	return true;
}
