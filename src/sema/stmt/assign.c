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


ofc_sema_stmt_t* ofc_sema_stmt_assign(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_ASSIGN)
		|| ofc_sparse_ref_empty(stmt->assign.variable))
		return false;

	ofc_sema_stmt_t s;
	ofc_sema_decl_t* dest
		= ofc_sema_scope_decl_find_create(
			scope, stmt->assign.variable, false);
	if (!dest) return false;

	if (!ofc_sema_type_is_integer(dest->type))
	{
		const ofc_sema_type_t* ptype
			= ofc_sema_type_create_primitive(
				OFC_SEMA_TYPE_INTEGER, OFC_SEMA_KIND_NONE);
		if (!ptype) return NULL;

		if (!ofc_sema_decl_type_set(
			dest, ptype, stmt->assign.variable))
		{
			ofc_sparse_ref_error(stmt->src,
				"ASSIGN destination must be of type INTEGER.");
			return NULL;
		}

		ofc_sparse_ref_warning(stmt->assign.variable,
			"IMPLICIT declaration of variable in ASSIGN destination"
			" as non-INTEGER makes no sense, declaring as INTEGER.");
	}
	s.assign.dest = dest;

	s.assign.label = ofc_sema_expr_label(
		scope, stmt->assign.label);
	if (!s.assign.label) return false;

	s.type = OFC_SEMA_STMT_ASSIGN;
	s.src = stmt->src;

	ofc_sema_stmt_t* as
		= ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_delete(s.assign.label);
		return NULL;
	}

	dest->was_written = true;
	return as;
}

bool ofc_sema_stmt_assign_print(
	ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt)
{
	return (cs && stmt
		&& ofc_colstr_keyword_atomic_writez(cs, "ASSIGN")
		&& ofc_colstr_atomic_writef(cs, " ")
		&& ofc_sema_expr_print(cs, stmt->assign.label)
		&& ofc_colstr_atomic_writef(cs, " ")
		&& ofc_colstr_keyword_atomic_writez(cs, "TO")
		&& ofc_colstr_atomic_writef(cs, " ")
		&& ofc_sema_decl_print_name(cs, stmt->assign.dest));
}
