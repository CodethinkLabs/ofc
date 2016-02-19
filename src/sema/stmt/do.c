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


static bool ofc_sema_stmt__loop_control(
	ofc_sema_scope_t* scope,
	const ofc_parse_assign_t* parse_init,
	const ofc_parse_expr_t*   parse_last,
	const ofc_parse_expr_t*   parse_step,
	ofc_sema_lhs_t**  sema_iter,
	ofc_sema_expr_t** sema_init,
	ofc_sema_expr_t** sema_last,
	ofc_sema_expr_t** sema_step)
{
	*sema_iter = ofc_sema_lhs(
		scope, parse_init->name);
	if (!*sema_iter) return false;

	if (!ofc_sema_lhs_mark_used(*sema_iter))
	{
		ofc_sema_lhs_delete(*sema_iter);
		return false;
	}

	const ofc_sema_type_t* dtype
		= ofc_sema_lhs_type(*sema_iter);
	if (!ofc_sema_type_is_scalar(dtype))
	{
		ofc_sparse_ref_error(parse_init->name->src,
			"DO loop iterator must be a scalar type.");
		ofc_sema_lhs_delete(*sema_iter);
		return false;
	}

	if (!ofc_sema_type_is_integer(dtype))
	{
		ofc_sparse_ref_warning(parse_init->name->src,
			"Using REAL in DO loop iterator.");
	}

	*sema_init = ofc_sema_expr(
		scope, parse_init->init);
	if (!*sema_init)
	{
		ofc_sema_lhs_delete(*sema_iter);
		return false;
	}

	if (!ofc_sema_type_compatible(dtype,
		ofc_sema_expr_type(*sema_init)))
	{
		ofc_sema_expr_t* cast
			= ofc_sema_expr_cast(*sema_init, dtype);
		if (!cast)
		{
			const ofc_sema_type_t* expr_type
				= ofc_sema_expr_type(*sema_init);
			ofc_sparse_ref_error(parse_init->init->src,
				"Expression type %s doesn't match lhs type %s",
				ofc_sema_type_str_rep(expr_type),
				ofc_sema_type_str_rep(dtype));
			ofc_sema_expr_delete(*sema_init);
			ofc_sema_lhs_delete(*sema_iter);
			return false;
		}
		*sema_init = cast;
	}

	*sema_last = ofc_sema_expr(
		scope, parse_last);
	if (!*sema_last)
	{
		ofc_sema_expr_delete(*sema_init);
		ofc_sema_lhs_delete(*sema_iter);
		return false;
	}

	if (!ofc_sema_type_compatible(dtype,
		ofc_sema_expr_type(*sema_last)))
	{
		ofc_sema_expr_t* cast
			= ofc_sema_expr_cast(*sema_last, dtype);
		if (!cast)
		{
			const ofc_sema_type_t* expr_type =
				ofc_sema_expr_type(*sema_last);
			ofc_sparse_ref_error(parse_last->src,
				"Expression type %s doesn't match lhs type %s",
				ofc_sema_type_str_rep(expr_type),
				ofc_sema_type_str_rep(dtype));
			ofc_sema_expr_delete(*sema_init);
			ofc_sema_expr_delete(*sema_last);
			ofc_sema_lhs_delete(*sema_iter);
			return false;
		}
		*sema_last = cast;
	}

	*sema_step = NULL;
	if (parse_step)
	{
		*sema_step = ofc_sema_expr(
			scope, parse_step);
		if (!*sema_step)
		{
			ofc_sema_expr_delete(*sema_init);
			ofc_sema_expr_delete(*sema_last);
			ofc_sema_lhs_delete(*sema_iter);
			return false;
		}

		if (!ofc_sema_type_compatible(dtype,
			ofc_sema_expr_type(*sema_step)))
		{
			ofc_sema_expr_t* cast
				= ofc_sema_expr_cast(*sema_step, dtype);
			if (!cast)
			{
				const ofc_sema_type_t* expr_type =
					ofc_sema_expr_type(*sema_step);
				ofc_sparse_ref_error(parse_step->src,
					"Expression type %s doesn't match lhs type %s",
					ofc_sema_type_str_rep(expr_type),
					ofc_sema_type_str_rep(dtype));
				ofc_sema_expr_delete(*sema_step);
				ofc_sema_expr_delete(*sema_init);
				ofc_sema_expr_delete(*sema_last);
				ofc_sema_lhs_delete(*sema_iter);
				return false;
			}
			*sema_step = cast;
		}
	}

	return true;
}

static ofc_sema_stmt_t* ofc_sema_stmt_do__label(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt
		|| (stmt->type != OFC_PARSE_STMT_DO_LABEL)
		|| !stmt->do_label.init
		|| !stmt->do_label.last
		|| !stmt->do_label.end_label)
		return NULL;

	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_DO_LABEL;
	s.do_label.iter = NULL;
	s.do_label.init = NULL;
	s.do_label.last = NULL;
	s.do_label.step = NULL;

	s.do_label.end_label = ofc_sema_expr_label(
		scope, stmt->do_label.end_label);
	if (!s.do_label.end_label) return NULL;

	if (!ofc_sema_stmt__loop_control(
		scope, stmt->do_label.init,
		stmt->do_label.last, stmt->do_label.step,
		&s.do_label.iter, &s.do_label.init,
		&s.do_label.last, &s.do_label.step))
	{
		ofc_sema_expr_delete(s.do_label.end_label);
		return NULL;
	}

	ofc_sema_stmt_t* as = ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_delete(s.do_label.end_label);
		ofc_sema_expr_delete(s.do_label.init);
		ofc_sema_expr_delete(s.do_label.last);
		ofc_sema_expr_delete(s.do_label.step);
		return NULL;
	}

	return as;
}

static ofc_sema_stmt_t* ofc_sema_stmt_do__block(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt
		|| (stmt->type != OFC_PARSE_STMT_DO_BLOCK)
		|| !stmt->do_block.init
		|| !stmt->do_block.last)
		return NULL;

	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_DO_BLOCK;
	s.do_block.iter  = NULL;
	s.do_block.init  = NULL;
	s.do_block.last  = NULL;
	s.do_block.step  = NULL;
	s.do_block.block = NULL;

	if (!ofc_sema_stmt__loop_control(
		scope, stmt->do_block.init,
		stmt->do_block.last, stmt->do_block.step,
		&s.do_block.iter, &s.do_block.init,
		&s.do_block.last, &s.do_block.step))
		return NULL;

	ofc_sema_stmt_t* as = ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_stmt_list_delete(s.do_block.block);
		ofc_sema_expr_delete(s.do_block.init);
		ofc_sema_expr_delete(s.do_block.last);
		ofc_sema_expr_delete(s.do_block.step);
		return NULL;
	}

	if (stmt->do_block.end_do_has_label
		&& !ofc_sema_label_map_add_end_block(
			scope->label, stmt->do_block.end_do_label, as))
	{
		ofc_sema_stmt_delete(as);
		return NULL;
	}

	if (stmt->do_block.block)
	{
		as->do_block.block
			= ofc_sema_stmt_list(
				scope, as, stmt->do_block.block);
		if (!as->do_block.block)
		{
			ofc_sema_stmt_delete(as);
			return NULL;
		}
	}

	return as;
}

static ofc_sema_stmt_t* ofc_sema_stmt_do_while__label(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{

	if (!stmt
		|| (stmt->type != OFC_PARSE_STMT_DO_WHILE)
		|| !stmt->do_while_block.cond)
		return NULL;

	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_DO_WHILE;

	s.do_while.end_label = ofc_sema_expr_label(
		scope, stmt->do_while.end_label);
	if (!s.do_while.end_label) return NULL;

	s.do_while.cond = ofc_sema_expr(
		scope, stmt->do_while.cond);
	if (!s.do_while.cond)
		return NULL;

	const ofc_sema_type_t* type
		= ofc_sema_expr_type(s.do_while.cond);
	if (!ofc_sema_type_is_logical(type))
	{
		ofc_sparse_ref_error(stmt->do_while.cond->src,
			"IF condition type must be LOGICAL.");

		ofc_sema_expr_delete(s.do_while.cond);
		return NULL;
	}

	ofc_sema_stmt_t* as = ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_delete(s.do_while.cond);
		return NULL;
	}

	return as;
}

static ofc_sema_stmt_t* ofc_sema_stmt_do_while__block(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{

	if (!stmt
		|| (stmt->type != OFC_PARSE_STMT_DO_WHILE_BLOCK)
		|| !stmt->do_while_block.cond)
		return NULL;

	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_DO_WHILE_BLOCK;

	s.do_while_block.cond = ofc_sema_expr(
		scope, stmt->do_while_block.cond);
	if (!s.do_while_block.cond)
		return NULL;

	const ofc_sema_type_t* type
		= ofc_sema_expr_type(s.do_while_block.cond);
	if (!ofc_sema_type_is_logical(type))
	{
		ofc_sparse_ref_error(stmt->do_while_block.cond->src,
			"IF condition type must be LOGICAL.");

		ofc_sema_expr_delete(s.do_while_block.cond);
		return NULL;
	}

	s.do_while_block.block = NULL;

	ofc_sema_stmt_t* as = ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_stmt_list_delete(s.do_while_block.block);
		ofc_sema_expr_delete(s.do_while_block.cond);
		return NULL;
	}

	if (stmt->do_while_block.end_do_has_label
		&& !ofc_sema_label_map_add_end_block(
			scope->label, stmt->do_while_block.end_do_label, as))
	{
		ofc_sema_stmt_delete(as);
		return NULL;
	}

	if (stmt->do_while_block.block)
	{
		as->do_while_block.block
			= ofc_sema_stmt_list(
				scope, as, stmt->do_while_block.block);

		if (!as->do_while_block.block)
		{
			ofc_sema_stmt_delete(as);
			return NULL;
		}
	}

	return as;
}

ofc_sema_stmt_t* ofc_sema_stmt_do(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt)
		return NULL;

	switch(stmt->type)
	{
		case OFC_PARSE_STMT_DO_LABEL:
			return ofc_sema_stmt_do__label(scope, stmt);
		case OFC_PARSE_STMT_DO_BLOCK:
			return ofc_sema_stmt_do__block(scope, stmt);
		case OFC_PARSE_STMT_DO_WHILE:
			return ofc_sema_stmt_do_while__label(scope, stmt);
		case OFC_PARSE_STMT_DO_WHILE_BLOCK:
			return ofc_sema_stmt_do_while__block(scope, stmt);

		default:
			break;
	}

	return NULL;
}

bool ofc_sema_stmt_do_label_print(
	ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt)
{
	if (!cs || (stmt->type != OFC_SEMA_STMT_DO_LABEL))
		return false;

	if (!ofc_colstr_atomic_writef(cs, "DO ")
		|| !ofc_sema_expr_print(cs, stmt->do_label.end_label)
		|| !ofc_colstr_atomic_writef(cs, ",")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_sema_lhs_print(cs, stmt->do_label.iter)
		|| !ofc_colstr_atomic_writef(cs, " =")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_sema_expr_print(cs, stmt->do_label.init))
		return false;

	if (stmt->do_label.last)
	{
		if (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_sema_expr_print(cs, stmt->do_label.last))
			return false;
	}

	if (stmt->do_label.step)
	{
		if (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_sema_expr_print(cs, stmt->do_label.step))
			return false;
	}

	return true;
}

bool ofc_sema_stmt_do_block_print(
	ofc_colstr_t* cs, unsigned indent,
	ofc_sema_label_map_t* label_map,
	const ofc_sema_stmt_t* stmt)
{
	if (!cs || (stmt->type != OFC_SEMA_STMT_DO_BLOCK))
		return false;

	if (!ofc_colstr_atomic_writef(cs, "DO ")
		|| !ofc_sema_lhs_print(cs, stmt->do_block.iter)
		|| !ofc_colstr_atomic_writef(cs, "=")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_sema_expr_print(cs, stmt->do_block.init))
		return false;

	if (stmt->do_block.last)
	{
		if (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_sema_expr_print(cs, stmt->do_block.last))
			return false;
	}
	if (stmt->do_block.step)
	{
		if (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_sema_expr_print(cs, stmt->do_block.step))
			return false;
	}
	if (!ofc_sema_stmt_list_print(
		cs, (indent + 1), label_map,
		stmt->do_block.block))
		return false;

	const ofc_sema_label_t* label
		= ofc_sema_label_map_find_end_block(
			label_map, stmt);
	const unsigned* ulabel = NULL;
	if (label) ulabel = &label->number;

	if (!ofc_colstr_newline(cs, indent, ulabel)
		|| !ofc_colstr_atomic_writef(cs, "END DO"))
		return false;

	return true;
}

bool ofc_sema_stmt_do_while_print(
	ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt)
{
	if (!cs || (stmt->type != OFC_SEMA_STMT_DO_WHILE))
		return false;

	if (!ofc_colstr_atomic_writef(cs, "DO WHILE")
		|| !ofc_sema_expr_print(cs, stmt->do_while.end_label)
		|| !ofc_sema_expr_print(cs, stmt->do_while.cond))
		return false;

	return true;
}

bool ofc_sema_stmt_do_while_block_print(
	ofc_colstr_t* cs, unsigned indent,
	ofc_sema_label_map_t* label_map,
	const ofc_sema_stmt_t* stmt)
{
	if (!cs || (stmt->type != OFC_SEMA_STMT_DO_WHILE_BLOCK))
		return false;

	if (!ofc_colstr_atomic_writef(cs, "DO WHILE")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_colstr_atomic_writef(cs, "(")
		|| !ofc_sema_expr_print(cs, stmt->do_while_block.cond)
		|| !ofc_colstr_atomic_writef(cs, ")")
		|| !ofc_sema_stmt_list_print(
			cs, (indent + 1), label_map,
			stmt->do_while_block.block))
		return false;

	const ofc_sema_label_t* label
		= ofc_sema_label_map_find_end_block(
			label_map, stmt);
	const unsigned* ulabel = NULL;
	if (label) ulabel = &label->number;

	if (!ofc_colstr_newline(cs, indent, ulabel)
		|| !ofc_colstr_atomic_writef(cs, "END DO"))
		return false;

	return true;
}
