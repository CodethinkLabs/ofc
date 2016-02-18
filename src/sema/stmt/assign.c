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
		= ofc_sema_scope_decl_find_modify(
			scope, stmt->assign.variable.string, false);
	if (!dest)
	{
		ofc_sema_spec_t* spec = ofc_sema_scope_spec_modify(
			scope, stmt->assign.variable);
		if (!spec) return false;

		if (spec->type != OFC_SEMA_TYPE_INTEGER)
		{
			if (!spec->type_implicit)
			{
				ofc_sparse_ref_warning(stmt->assign.variable,
					"IMPLICIT declaration of variable in ASSIGN destination"
					" as non-INTEGER makes no sense, declaring as INTEGER.");
			}
			spec->type_implicit = false;
			spec->type = OFC_SEMA_TYPE_INTEGER;
		}

		dest = ofc_sema_decl_spec(
			scope, stmt->assign.variable, spec, NULL);
		if (!dest) return false;
	}
	s.assign.dest = dest;

	s.assign.label = ofc_sema_expr_label(
		scope, stmt->assign.label);
	if (!s.assign.label) return false;

	const ofc_sema_type_t* dtype
		= ofc_sema_decl_type(s.assign.dest);
	if (!ofc_sema_type_is_integer(dtype))
	{
		ofc_sparse_ref_error(stmt->src,
			"ASSIGN destination must be of type INTEGER.");
		ofc_sema_expr_delete(s.assign.label);
		return NULL;
	}
	s.type = OFC_SEMA_STMT_ASSIGN;
	s.src = stmt->src;

	ofc_sema_stmt_t* as
		= ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_delete(s.assign.label);
		return NULL;
	}

	dest->used = true;
	return as;
}

bool ofc_sema_stmt_assign_print(
	ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt)
{
	return (cs && stmt
		&& ofc_colstr_atomic_writef(cs, "ASSIGN")
		&& ofc_colstr_atomic_writef(cs, " ")
		&& ofc_sema_expr_print(cs, stmt->assign.label)
		&& ofc_colstr_atomic_writef(cs, " ")
		&& ofc_colstr_atomic_writef(cs, "TO")
		&& ofc_colstr_atomic_writef(cs, " ")
		&& ofc_sema_decl_print_name(cs, stmt->assign.dest));
}
