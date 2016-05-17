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

static ofc_sema_stmt_t* ofc_sema_stmt_if__computed(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt
		|| (stmt->type != OFC_PARSE_STMT_IF_COMPUTED)
		|| !stmt->if_comp.cond)
		return NULL;

	if (!stmt->if_comp.label
		|| (stmt->if_comp.label->count < 3))
	{
		ofc_sparse_ref_error(stmt->src,
			"Not enough targets in arithmetic IF statement.");
		return NULL;
	}
	else if (stmt->if_comp.label->count > 3)
	{
		ofc_sparse_ref_error(stmt->src,
			"Too many targets in arithmetic IF statement.");
		return NULL;
	}

	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_IF_COMPUTED;
	s.if_comp.cond = ofc_sema_expr(
		scope, stmt->if_comp.cond);

	if (!s.if_comp.cond)
		return NULL;

	const ofc_sema_type_t* type
		= ofc_sema_expr_type(s.if_comp.cond);

	if (!ofc_sema_type_is_scalar(type))
	{
		ofc_sparse_ref_error(stmt->if_stmt.cond->src,
			"IF condition must be a scalar type.");

		ofc_sema_expr_delete(s.if_comp.cond);
		return NULL;
	}

	s.if_comp.label = ofc_sema_expr_list_create();
	if (!s.if_comp.label)
	{
		ofc_sema_expr_delete(s.if_comp.cond);
		return NULL;
	}

	unsigned i;
	for (i = 0; i < stmt->if_comp.label->count; i++)
	{
		ofc_sema_expr_t* label = ofc_sema_expr_label(
			scope, stmt->if_comp.label->expr[i]);

		if (!ofc_sema_expr_list_add(
			s.if_comp.label, label))
		{
			ofc_sema_expr_delete(label);
			ofc_sema_expr_list_delete(s.if_comp.label);
			ofc_sema_expr_delete(s.if_comp.cond);
			return NULL;
		}
	}

	ofc_sema_stmt_t* as = ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_list_delete(s.if_comp.label);
		ofc_sema_expr_delete(s.if_comp.cond);
		return NULL;
	}

	return as;
}

static ofc_sema_stmt_t* ofc_sema_stmt_if__statement(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt
		|| (stmt->type != OFC_PARSE_STMT_IF_STATEMENT)
		|| !stmt->if_stmt.cond
		|| !stmt->if_stmt.stmt)
		return NULL;

	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_IF_STATEMENT;

	s.if_stmt.cond = ofc_sema_expr(
		scope, stmt->if_stmt.cond);
	if (!s.if_stmt.cond)
		return NULL;

	const ofc_sema_type_t* type
		= ofc_sema_expr_type(s.if_stmt.cond);

	if (!ofc_sema_type_is_logical(type))
	{
		ofc_sema_expr_t* cast = ofc_sema_expr_cast(
			s.if_stmt.cond, ofc_sema_type_logical_default());

		if (!cast)
		{
			ofc_sparse_ref_error(stmt->if_stmt.cond->src,
				"IF condition type must be LOGICAL.");

			ofc_sema_expr_delete(s.if_stmt.cond);
			return NULL;
		}

		s.if_stmt.cond = cast;
	}

	s.if_stmt.stmt = ofc_sema_stmt(
		scope, stmt->if_stmt.stmt);
	if (!s.if_stmt.stmt)
	{
		ofc_sema_expr_delete(s.if_stmt.cond);
		return NULL;
	}

	ofc_sema_stmt_t* as = ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_delete(s.if_stmt.cond);
		ofc_sema_stmt_delete(s.if_stmt.stmt);
		return NULL;
	}

	return as;
}

static ofc_sema_stmt_t* ofc_sema_stmt_if__then(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt
		|| (stmt->type != OFC_PARSE_STMT_IF_THEN)
		|| !stmt->if_then.cond)
		return NULL;

	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_IF_THEN;

	s.if_then.cond = ofc_sema_expr(
		scope, stmt->if_then.cond);
	if (!s.if_then.cond)
		return NULL;

	const ofc_sema_type_t* type
		= ofc_sema_expr_type(s.if_then.cond);
	if (!ofc_sema_type_is_logical(type))
	{
		ofc_sema_expr_t* cast = ofc_sema_expr_cast(
			s.if_then.cond, ofc_sema_type_logical_default());

		if (!cast)
		{
			ofc_sparse_ref_error(stmt->if_then.cond->src,
				"IF condition type must be LOGICAL.");

			ofc_sema_expr_delete(s.if_then.cond);
			return NULL;
		}

		s.if_then.cond = cast;
	}

	s.if_then.block_then = NULL;
	s.if_then.block_else = NULL;

	ofc_sema_stmt_t* as = ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_delete(s.if_then.cond);
		ofc_sema_stmt_list_delete(s.if_then.block_then);
		ofc_sema_stmt_list_delete(s.if_then.block_else);
		return NULL;
	}

	if (stmt->if_then.end_if_has_label
		&& !ofc_sema_label_map_add_end_block(
			scope->label, stmt->if_then.end_if_label, as))
	{
		ofc_sema_stmt_delete(as);
		return NULL;
	}

	if (stmt->if_then.block_then)
	{
		as->if_then.block_then = ofc_sema_stmt_list(
			scope, as, stmt->if_then.block_then);
		if (!as->if_then.block_then)
		{
			ofc_sema_stmt_delete(as);
			return NULL;
		}
	}
	else
	{
		ofc_sparse_ref_warning(stmt->src,
			"Empty IF THEN block");
	}

	if (stmt->if_then.block_else)
	{
		as->if_then.block_else = ofc_sema_stmt_list(
			scope, as, stmt->if_then.block_else);
		if (!as->if_then.block_else)
		{
			ofc_sema_stmt_delete(as);
			return NULL;
		}
	}

	return as;
}

ofc_sema_stmt_t* ofc_sema_stmt_if(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt)
		return NULL;

	switch(stmt->type)
	{
		case OFC_PARSE_STMT_IF_COMPUTED:
			return ofc_sema_stmt_if__computed(scope, stmt);
		case OFC_PARSE_STMT_IF_STATEMENT:
			return ofc_sema_stmt_if__statement(scope, stmt);
		case OFC_PARSE_STMT_IF_THEN:
			return ofc_sema_stmt_if__then(scope, stmt);
		default:
			break;
	}

	return NULL;
}

bool ofc_sema_stmt_if_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_sema_stmt_t* stmt)
{
	if (!cs || !stmt) return false;

	if (!ofc_colstr_atomic_writef(cs, "IF")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_colstr_atomic_writef(cs, "(")
		|| !ofc_sema_expr_print(cs, stmt->if_stmt.cond)
		|| !ofc_colstr_atomic_writef(cs, ")")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_sema_stmt_print(cs, indent, NULL,
			stmt->if_stmt.stmt))
		return false;

	return true;
}

bool ofc_sema_stmt_if_comp_print(ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt)
{
	if (!cs || !stmt) return false;

	if (!ofc_colstr_atomic_writef(cs, "IF")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_colstr_atomic_writef(cs, "(")
		|| !ofc_sema_expr_print(cs, stmt->if_comp.cond)
		|| !ofc_colstr_atomic_writef(cs, ")")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_sema_expr_list_print(cs, stmt->if_comp.label))
		return false;

	return true;
}

bool ofc_sema_stmt_if_then_print(
	ofc_colstr_t* cs, unsigned indent,
	ofc_sema_label_map_t* label_map,
	const ofc_sema_stmt_t* stmt)
{
	if (!cs || !stmt) return false;

	if (!ofc_colstr_atomic_writef(cs, "IF")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_colstr_atomic_writef(cs, "(")
		|| !ofc_sema_expr_print(cs, stmt->if_then.cond)
		|| !ofc_colstr_atomic_writef(cs, ")")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_colstr_atomic_writef(cs, "THEN"))
		return false;

	if (stmt->if_then.block_then)
	{
		if (!ofc_sema_stmt_list_print(cs, (indent + 1),
			label_map, stmt->if_then.block_then))
				return false;
	}

	const ofc_sema_label_t* label
		= ofc_sema_label_map_find_end_block(
			label_map, stmt);
	const unsigned* ulabel = NULL;
	if (label) ulabel = &label->number;

	bool hide_end = false;
	if (stmt->if_then.block_else)
	{
		if (!ofc_colstr_newline(cs, indent, NULL)
			|| !ofc_colstr_atomic_writef(cs, "ELSE"))
			return false;

		if ((stmt->if_then.block_else->count == 1)
			&& stmt->if_then.block_else->stmt[0]
			&& (stmt->if_then.block_else->stmt[0]->type
				== OFC_SEMA_STMT_IF_THEN)
			&& !ulabel)
		{
			if (!ofc_colstr_atomic_writef(cs, " "))
				return false;

			if (!ofc_sema_stmt_if_then_print(
				cs, indent, label_map,
				stmt->if_then.block_else->stmt[0]))
				return false;
			hide_end = true;
		}
		else
		{
			if (!ofc_sema_stmt_list_print(cs, (indent + 1),
				label_map, stmt->if_then.block_else))
				return false;
		}
	}

	if (!hide_end)
	{
		if (!ofc_colstr_newline(cs, indent, ulabel)
			|| !ofc_colstr_atomic_writef(cs, "END IF"))
			return false;
	}

	return true;
}
