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


static ofc_sema_stmt_t* ofc_sema_stmt_go_to__assigned(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_GO_TO;
	s.go_to.label = ofc_sema_expr_label(
		scope, stmt->go_to_list.cond);
	if (!s.go_to.label) return NULL;

	s.go_to.allow = ofc_sema_expr_list_label(
		scope, stmt->go_to_list.label);
	if (!s.go_to.allow)
	{
		ofc_sema_expr_delete(s.go_to.label);
		return NULL;
	}

	unsigned i;
	for (i = 0; i < s.go_to.allow->count; i++)
	{
		ofc_sema_expr_t* expr
			= s.go_to.allow->expr[i];

		if (!ofc_sema_expr_is_constant(expr))
		{
			ofc_sparse_ref_error(expr->src,
				"Assigned GO TO allow list entries must be constant");
			ofc_sema_expr_list_delete(s.go_to.allow);
			ofc_sema_expr_delete(s.go_to.label);
			return NULL;
		}
	}

	if (ofc_sema_expr_is_constant(s.go_to.label))
	{
		const ofc_sema_typeval_t* label_ctv
			= ofc_sema_expr_constant(s.go_to.label);

		bool match = false;
		for (i = 0; i < s.go_to.allow->count; i++)
		{
			ofc_sema_expr_t* expr
				= s.go_to.allow->expr[i];

			const ofc_sema_typeval_t* ctv
				= ofc_sema_expr_constant(expr);

			if (ofc_sema_typeval_compare(label_ctv, ctv))
			{
				match = true;
				break;
			}
		}

		if (!match)
		{
			ofc_sparse_ref_error(s.go_to.label->src,
				"Assigned GO TO target not in allow list");
			ofc_sema_expr_list_delete(s.go_to.allow);
			ofc_sema_expr_delete(s.go_to.label);
			return NULL;
		}

		ofc_sparse_ref_warning(s.go_to.label->src,
			"Using assigned GO TO for a constant label makes little sense");
	}

	ofc_sema_stmt_t* as
		= ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_list_delete(s.go_to.allow);
		ofc_sema_expr_delete(s.go_to.label);
		return NULL;
	}

	return as;
}

static ofc_sema_stmt_t* ofc_sema_stmt_go_to__computed(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_GO_TO_COMPUTED;
	s.go_to_comp.cond = ofc_sema_expr(
		scope, stmt->go_to_list.cond);
	if (!s.go_to_comp.cond) return NULL;

	const ofc_sema_type_t* type
		= ofc_sema_expr_type(s.go_to_comp.cond);
	if (!ofc_sema_type_is_scalar(type))
	{
		ofc_sparse_ref_error(s.go_to_comp.cond->src,
			"Computed GO TO value must be scalar");
		ofc_sema_expr_delete(s.go_to_comp.cond);
		return NULL;
	}

	if (!ofc_sema_type_is_integer(type))
	{
		ofc_sema_expr_t* cast
			= ofc_sema_expr_cast(s.go_to_comp.cond,
				ofc_sema_type_integer_default());
		if (!cast)
		{
			ofc_sema_expr_delete(s.go_to_comp.cond);
			return NULL;
		}
		s.go_to_comp.cond = cast;
	}

	s.go_to_comp.label = ofc_sema_expr_list_label(
		scope, stmt->go_to_list.label);
	if (!s.go_to_comp.label)
	{
		ofc_sema_expr_delete(s.go_to_comp.cond);
		return NULL;
	}

	unsigned i;
	for (i = 0; i < s.go_to_comp.label->count; i++)
	{
		ofc_sema_expr_t* expr
			= s.go_to_comp.label->expr[i];

		if (!ofc_sema_expr_is_constant(expr))
		{
			ofc_sparse_ref_warning(expr->src,
				"Computed GO TO label list entry should be constant");
		}
	}

	if (ofc_sema_expr_is_constant(s.go_to_comp.cond))
	{
		ofc_sparse_ref_warning(s.go_to_comp.cond->src,
			"Using computed GO TO for a constant value makes little sense");
	}

	ofc_sema_stmt_t* as
		= ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_list_delete(s.go_to_comp.label);
		ofc_sema_expr_delete(s.go_to_comp.cond);
		return NULL;
	}

	return as;
}


ofc_sema_stmt_t* ofc_sema_stmt_go_to(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt)
		return NULL;

	switch (stmt->type)
	{
		case OFC_PARSE_STMT_GO_TO:
			break;
		case OFC_PARSE_STMT_GO_TO_ASSIGNED:
			return ofc_sema_stmt_go_to__assigned(scope, stmt);
		case OFC_PARSE_STMT_GO_TO_COMPUTED:
			return ofc_sema_stmt_go_to__computed(scope, stmt);
		default:
			return NULL;
	}

	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_GO_TO;
	s.go_to.label = ofc_sema_expr_label(
		scope, stmt->go_to.label);
	if (!s.go_to.label) return NULL;

	if (!ofc_sema_expr_is_constant(s.go_to.label))
	{
		ofc_sparse_ref_warning(s.go_to.label->src,
			"Should use assigned GO TO when target isn't a constant label");
	}

	s.go_to.allow = NULL;

	ofc_sema_stmt_t* as
		= ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_delete(s.go_to.label);
		return NULL;
	}

	return as;
}

bool ofc_sema_go_to_print(
	ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt)
{
	if (!cs || (stmt->type != OFC_SEMA_STMT_GO_TO))
		return false;

	if (!ofc_colstr_keyword_atomic_writez(cs, "GO TO")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_sema_expr_print(cs, stmt->go_to.label))
		return false;

	if (stmt->go_to.allow)
	{
		if (!ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_atomic_writef(cs, "(")
			|| !ofc_sema_expr_list_print(cs, stmt->go_to.allow)
			|| !ofc_colstr_atomic_writef(cs, ")"))
			return false;
	}

	return true;
}

bool ofc_sema_go_to_computed_print(
	ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt)
{
	if (!cs || (stmt->type != OFC_SEMA_STMT_GO_TO_COMPUTED))
		return false;

	if (!ofc_colstr_keyword_atomic_writez(cs, "GO TO")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_colstr_atomic_writef(cs, "(")
		|| !ofc_sema_expr_list_print(cs, stmt->go_to_comp.label)
		|| !ofc_colstr_atomic_writef(cs, ")")
		|| !ofc_colstr_atomic_writef(cs, ",")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_sema_expr_print(cs, stmt->go_to_comp.cond))
		return false;

	return true;
}
