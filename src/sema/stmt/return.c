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

#include <ofc/sema.h>


ofc_sema_stmt_t* ofc_sema_stmt_return(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_RETURN))
		return NULL;

	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_RETURN;
	s.alt_return = NULL;

	if (stmt->stop_pause_return.value)
	{
		s.alt_return = ofc_sema_expr(
			scope, stmt->stop_pause_return.value);
		if (!ofc_sema_expr_validate_uint(s.alt_return))
		{
			ofc_sparse_ref_error(stmt->src,
				"Alternate RETURN value must be a positive INTEGER");
			ofc_sema_expr_delete(s.alt_return);
			return NULL;
		}
	}

	ofc_sema_stmt_t* as
		= ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_delete(s.alt_return);
		return NULL;
	}

	return as;
}
