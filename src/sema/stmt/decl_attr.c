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

		ofc_sema_decl_t* decl
			= ofc_sema_scope_decl_find_create(
				scope, decl_name, true);
		if (!decl) return false;

		switch (stmt->type)
		{
			case OFC_PARSE_STMT_DECL_ATTR_AUTOMATIC:
				if (decl->is_static)
				{
					ofc_sparse_ref_error(stmt->src,
						"Specifying '%.*s' as STATIC and AUTOMATIC",
						decl_name.string.size, decl_name.string.base);
					return false;
				}
				if (decl->is_automatic)
				{
					ofc_sparse_ref_warning(stmt->src,
						"Re-declaring '%.*s' as AUTOMATIC",
						decl_name.string.size, decl_name.string.base);
				}
				decl->is_automatic = true;
				break;
			case OFC_PARSE_STMT_DECL_ATTR_STATIC:
				if (decl->is_automatic)
				{
					ofc_sparse_ref_error(stmt->src,
						"Specifying '%.*s' as AUTOMATIC and STATIC",
						decl_name.string.size, decl_name.string.base);
					return false;
				}
				if (decl->is_static)
				{
					ofc_sparse_ref_warning(stmt->src,
						"Re-declaring '%.*s' as STATIC",
						decl_name.string.size, decl_name.string.base);
				}
				decl->is_static = true;
				break;
			case OFC_PARSE_STMT_DECL_ATTR_VOLATILE:
				if (decl->is_volatile)
				{
					ofc_sparse_ref_warning(stmt->src,
						"Re-declaring '%.*s' as VOLATILE",
						decl_name.string.size, decl_name.string.base);
				}
				decl->is_volatile = true;
				break;
			case OFC_PARSE_STMT_DECL_ATTR_EXTERNAL:
				if (decl->is_intrinsic)
				{
					ofc_sparse_ref_error(stmt->src,
						"Specifying '%.*s' as INTRINSIC and EXTERNAL",
						decl_name.string.size, decl_name.string.base);
					return false;
				}
				if (decl->is_external)
				{
					ofc_sparse_ref_warning(stmt->src,
						"Re-declaring '%.*s' as EXTERNAL",
						decl_name.string.size, decl_name.string.base);
				}
				decl->is_external = true;
				break;
			case OFC_PARSE_STMT_DECL_ATTR_INTRINSIC:
				if (decl->is_external)
				{
					ofc_sparse_ref_error(stmt->src,
						"Specifying '%.*s' as EXTERNAL and INTRINSIC",
						decl_name.string.size, decl_name.string.base);
					return false;
				}
				if (decl->is_intrinsic)
				{
					ofc_sparse_ref_warning(stmt->src,
						"Re-declaring '%.*s' as INTRINSIC",
						decl_name.string.size, decl_name.string.base);
				}
				decl->is_intrinsic = true;
				break;
			default:
				return false;
		}
	}

	return true;
}
