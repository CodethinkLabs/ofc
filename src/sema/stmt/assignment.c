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


bool ofc_sema_stmt_is_stmt_func(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_ASSIGNMENT)
		|| !stmt->assignment
		|| !stmt->assignment->name)
		return false;

	if (stmt->assignment->name->type
		!= OFC_PARSE_LHS_ARRAY)
		return false;

	if (!stmt->assignment->name->parent
		|| (stmt->assignment->name->parent->type
			!= OFC_PARSE_LHS_VARIABLE))
		return false;

	ofc_sparse_ref_t base_name;
	if (!ofc_parse_lhs_base_name(
		*(stmt->assignment->name), &base_name))
		return NULL;

	ofc_sema_spec_t* spec
		= ofc_sema_scope_spec_find_final(
			scope, base_name);
	if (spec)
	{
		bool is_array = (spec->array != NULL);
		ofc_sema_spec_delete(spec);
		if (is_array) return false;
	}

	const ofc_sema_decl_t* decl
		= ofc_sema_scope_decl_find(
			scope, base_name.string, false);
	return (decl == NULL);
}

ofc_sema_stmt_t* ofc_sema_stmt_assignment(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_ASSIGNMENT)
		|| !stmt->assignment
		|| !stmt->assignment->name
		|| !stmt->assignment->init)
		return NULL;

	ofc_sema_stmt_t s;
	s.assignment.dest = ofc_sema_lhs(
		scope, stmt->assignment->name);
	if (!s.assignment.dest) return NULL;

	s.assignment.expr = ofc_sema_expr(
		scope, stmt->assignment->init);
	if (!s.assignment.expr)
	{
		ofc_sema_lhs_delete(s.assignment.dest);
		return NULL;
	}

	const ofc_sema_type_t* dtype
		= ofc_sema_lhs_type(s.assignment.dest);
	if (!ofc_sema_type_compare(dtype,
		ofc_sema_expr_type(s.assignment.expr)))
	{
		ofc_sema_expr_t* cast
			= ofc_sema_expr_cast(
				s.assignment.expr, dtype);
		if (!cast)
		{
			const ofc_sema_type_t* expr_type =
				ofc_sema_expr_type(s.assignment.expr);
			ofc_sparse_ref_error(stmt->src,
				"Expression type %s doesn't match lhs type %s",
				ofc_sema_type_str_rep(expr_type),
				ofc_sema_type_str_rep(dtype));
			ofc_sema_expr_delete(s.assignment.expr);
			ofc_sema_lhs_delete(s.assignment.dest);
			return NULL;
		}
		s.assignment.expr = cast;
	}
	s.type = OFC_SEMA_STMT_ASSIGNMENT;
	s.src = stmt->src;

	ofc_sema_stmt_t* as
		= ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_delete(s.assignment.expr);
		ofc_sema_lhs_delete(s.assignment.dest);
		return NULL;
	}

	if (!ofc_sema_lhs_mark_used(
		s.assignment.dest))
	{
		ofc_sema_stmt_delete(as);
		return NULL;
	}

	return as;
}

bool ofc_sema_stmt_assignment_print(
	ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt)
{
	if (!cs || !stmt) return false;

	if (!ofc_sema_lhs_print(cs, stmt->assignment.dest))
		return false;
	if (!ofc_colstr_atomic_writef(cs, " = "))
		return false;
	if (!ofc_sema_expr_print(cs, stmt->assignment.expr))
		return false;

	return true;
}
