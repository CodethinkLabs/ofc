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


bool ofc_sema_stmt__public_private(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| ((stmt->type != OFC_PARSE_STMT_PUBLIC)
			&& (stmt->type != OFC_PARSE_STMT_PRIVATE)))
		return false;

	bool is_public;
	const char* kwstr;
	switch (stmt->type)
	{
		case OFC_PARSE_STMT_PUBLIC:
			is_public = true;
			kwstr = "PUBLIC";
			break;
		case OFC_PARSE_STMT_PRIVATE:
			is_public = false;
			kwstr = "PRIVATE";
			break;
		default:
			return false;
	}

	if (scope->type != OFC_SEMA_SCOPE_MODULE)
	{
		ofc_sparse_ref_error(stmt->src,
			"%s not allowed outside of MODULE", kwstr);
		return false;
	}

	if (!stmt->public_private.list)
	{
		if ((is_public && (scope->access == OFC_SEMA_ACCESSIBILITY_PUBLIC))
			|| (!is_public && (scope->access == OFC_SEMA_ACCESSIBILITY_PRIVATE)))
		{
			ofc_sparse_ref_warning(stmt->src,
				"Scope already marked as %s", kwstr);
		}
		else if (scope->access != OFC_SEMA_ACCESSIBILITY_DEFAULT)
		{
			ofc_sparse_ref_error(stmt->src,
				"Scope already marked as %s",
				(is_public ? "PRIVATE" : "PUBLIC"));
			return false;
		}

		if (is_public)
			scope->access = OFC_SEMA_ACCESSIBILITY_PUBLIC;
		else
			scope->access = OFC_SEMA_ACCESSIBILITY_PRIVATE;

		return true;
	}

	unsigned i;
	for (i = 0; i < stmt->public_private.list->count; i++)
	{
		ofc_sparse_ref_t base_name;
		if (!ofc_parse_lhs_base_name(
			*(stmt->public_private.list->lhs[i]), &base_name))
			return false;

		ofc_sema_decl_t* decl
			= ofc_sema_scope_decl_find_create(
				scope, base_name, true);
		if (!decl) return false;

		if ((is_public && (decl->access == OFC_SEMA_ACCESSIBILITY_PUBLIC))
			|| (!is_public && (decl->access == OFC_SEMA_ACCESSIBILITY_PRIVATE)))
		{
			ofc_sparse_ref_warning(stmt->public_private.list->lhs[i]->src,
				"Declaration already set as %s", kwstr);
		}
		else if (decl->access != OFC_SEMA_ACCESSIBILITY_DEFAULT)
		{
			ofc_sparse_ref_error(stmt->public_private.list->lhs[i]->src,
				"Declaration already set as %s",
				(is_public ? "PRIVATE" : "PUBLIC"));
			return false;
		}

		if (is_public)
			decl->access = OFC_SEMA_ACCESSIBILITY_PUBLIC;
		else
			decl->access = OFC_SEMA_ACCESSIBILITY_PRIVATE;
	}

	return true;
}

bool ofc_sema_stmt_public(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	return ofc_sema_stmt__public_private(scope, stmt);
}

bool ofc_sema_stmt_private(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	return ofc_sema_stmt__public_private(scope, stmt);
}
