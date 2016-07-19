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

ofc_sema_stmt_t* ofc_sema_stmt_select_case(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_SELECT_CASE)
		|| !stmt->select_case.case_expr)
		return NULL;

	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_SELECT_CASE;
	s.select_case.case_expr  = NULL;
	s.select_case.count      = 0;
	s.select_case.case_value = NULL;
	s.select_case.case_block = NULL;

	s.select_case.case_expr = ofc_sema_expr(
		scope, stmt->select_case.case_expr);
	if (!s.select_case.case_expr)
		return NULL;

	const ofc_sema_type_t* type
		= ofc_sema_expr_type(s.select_case.case_expr);

	if (ofc_sema_type_is_scalar(type))
	{
		if (!ofc_sema_type_is_integer(type))
		{
			ofc_sema_expr_t* cast = ofc_sema_expr_cast(
				s.select_case.case_expr,
				ofc_sema_type_integer_default());

			if (!cast)
			{
				ofc_sema_expr_delete(
					s.select_case.case_expr);
				return NULL;
			}

			s.select_case.case_expr = cast;
		}
	}
	else if (!ofc_sema_type_is_character(type)
		|| !ofc_sema_type_is_logical(type))
	{
		ofc_sparse_ref_error(s.select_case.case_expr->src,
			"SELECT CASE value has to be of type INTEGER, CHARACTER or LOGICAL");
		ofc_sema_expr_delete(s.select_case.case_expr);
		return NULL;
	}

	type = ofc_sema_expr_type(s.select_case.case_expr);

	ofc_sema_stmt_t* as = ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_delete(s.select_case.case_expr);
		return NULL;
	}

	bool has_default = false;
	unsigned i;
	for (i = 0; i < stmt->select_case.count; i++)
	{
		ofc_sema_range_list_t** nrange
			= (ofc_sema_range_list_t**)realloc(as->select_case.case_value,
				(sizeof(ofc_sema_range_list_t*) * (as->select_case.count + 1)));
		if (!nrange)
		{
			ofc_sema_stmt_delete(as);
			return NULL;
		}
		as->select_case.case_value = nrange;

		ofc_sema_stmt_list_t** nblock
			= (ofc_sema_stmt_list_t**)realloc(as->select_case.case_block,
				(sizeof(ofc_sema_stmt_list_t*) * (as->select_case.count + 1)));
		if (!nblock)
		{
			ofc_sema_stmt_delete(as);
			return NULL;
		}
		as->select_case.case_block = nblock;

		as->select_case.count++;

		as->select_case.case_value[i] = NULL;
		as->select_case.case_block[i] = NULL;
		bool is_default = true;

		if (stmt->select_case.case_value[i])
		{
			as->select_case.case_value[i]
				= ofc_sema_range_list(scope,
					stmt->select_case.case_value[i]);
			if (!as->select_case.case_value[i])
			{
				ofc_sema_stmt_delete(as);
				return NULL;
			}

			is_default = false;
		}

		if (has_default && is_default)
		{
			ofc_sparse_ref_error(as->select_case.case_value[i]->src,
				"SELECT CASE can only have one DEFAULT case");
			ofc_sema_stmt_delete(as);
			return NULL;
		}

		has_default = is_default;

		if (!is_default)
		{
			/* Check that range values are known at compile time. */
			unsigned j;
			for (j = 0; j < as->select_case.case_value[i]->count; j++)
			{
				if (as->select_case.case_value[i]->range[j]->first
					&& !ofc_sema_expr_is_constant(
						as->select_case.case_value[i]->range[j]->first))
				{
					ofc_sparse_ref_error(as->select_case.case_value[i]->range[j]->first->src,
						"Range must be constant");
					ofc_sema_stmt_delete(as);
					return NULL;
				}
				if (as->select_case.case_value[i]->range[j]->last
					&& !ofc_sema_expr_is_constant(
						as->select_case.case_value[i]->range[j]->last))
				{
					ofc_sparse_ref_error(as->select_case.case_value[i]->range[j]->last->src,
						"Range must be constant");
					ofc_sema_stmt_delete(as);
					return NULL;
				}

				if (!as->select_case.case_value[i]->range[j]->first
					&& !as->select_case.case_value[i]->range[j]->last)
				{
					ofc_sparse_ref_error(as->select_case.case_value[i]->range[j]->src,
						"Range can't be empty");
					ofc_sema_stmt_delete(as);
					return NULL;
				}

				/* Check that types are compatible. */
				const ofc_sema_type_t* type_first = ofc_sema_expr_type(
						as->select_case.case_value[i]->range[j]->first);
				const ofc_sema_type_t* type_last  = ofc_sema_expr_type(
						as->select_case.case_value[i]->range[j]->last);

				if (type_first && !ofc_sema_type_compatible(type, type_first))
				{
					ofc_sema_expr_t* cast = ofc_sema_expr_cast(
						as->select_case.case_value[i]->range[j]->first, type);

					if (!cast)
					{
						ofc_sparse_ref_error(as->select_case.case_value[i]->range[j]->first->src,
							"Type of CASE selector value not compatible with case index value");
						ofc_sema_stmt_delete(as);
						return NULL;
					}

					as->select_case.case_value[i]->range[j]->first = cast;
				}

				if (type_last && !ofc_sema_type_compatible(type, type_last))
				{
					ofc_sema_expr_t* cast = ofc_sema_expr_cast(
						as->select_case.case_value[i]->range[j]->last, type);

					if (!cast)
					{
						ofc_sparse_ref_error(as->select_case.case_value[i]->range[j]->last->src,
							"Type of CASE selector value not compatible with case index value");
						ofc_sema_stmt_delete(as);
						return NULL;
					}

					as->select_case.case_value[i]->range[j]->last = cast;
				}
			}

			/* Check that ranges don't overlap. */
			unsigned l;
			for (l = 0; l < i; l++)
			{
				if (ofc_sema_range_list_intersects(
					as->select_case.case_value[l],
					as->select_case.case_value[i]))
				{
					ofc_sparse_ref_error(as->select_case.case_value[i]->src,
						"CASE range overlaps previous range");
					ofc_sema_stmt_delete(as);
					return NULL;
				}
			}
		}

		if (stmt->select_case.case_block[i])
		{
			as->select_case.case_block[i]
				= ofc_sema_stmt_list(scope, as,
					stmt->select_case.case_block[i]);
			if (!as->select_case.case_block[i])
			{
				ofc_sema_stmt_delete(as);
				return NULL;
			}
		}
	}

	if (stmt->select_case.end_select_case_has_label
		&& !ofc_sema_label_map_add_end_block(
			scope->label, stmt->select_case.end_select_case_label, as))
	{
		ofc_sema_stmt_delete(as);
		return NULL;
	}


	return as;
}


bool ofc_sema_stmt_select_case_print(
	ofc_colstr_t* cs, unsigned indent,
	ofc_sema_label_map_t* label_map,
	const ofc_sema_stmt_t* stmt)
{
	if (!cs || !stmt
		|| (stmt->type != OFC_SEMA_STMT_SELECT_CASE))
		return false;

	if (!ofc_colstr_keyword_atomic_writef(cs, "SELECT")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_colstr_keyword_atomic_writef(cs, "CASE")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_colstr_atomic_writef(cs, "("))
		return false;
	if (stmt->select_case.case_expr
		&& !ofc_sema_expr_print(cs, stmt->select_case.case_expr))
		return false;
	if (!ofc_colstr_atomic_writef(cs, ")"))
		return false;

	unsigned i;
	for (i = 0; i < stmt->select_case.count; i++)
	{
		if (!ofc_colstr_newline(cs, (indent + 1), NULL))
			return false;

		if (!ofc_colstr_keyword_atomic_writef(cs, "CASE")
			|| !ofc_colstr_atomic_writef(cs, " "))
			return false;

		if (!stmt->select_case.case_value[i])
		{
			if (!ofc_colstr_keyword_atomic_writef(cs, "DEFAULT"))
				return false;
		}
		else
		{
			if (!ofc_sema_range_list_print(
				cs, stmt->select_case.case_value[i]))
				return false;
		}

		if (stmt->select_case.case_block[i]
			&& !ofc_sema_stmt_list_print(
				cs, (indent + 2), label_map,
				stmt->select_case.case_block[i]))
			return false;
	}

	const ofc_sema_label_t* label
		= ofc_sema_label_map_find_end_block(
			label_map, stmt);
	const unsigned* ulabel = NULL;
	if (label) ulabel = &label->number;

	if (!ofc_colstr_newline(cs, indent, ulabel)
		|| !ofc_colstr_keyword_atomic_writef(cs, "END SELECT"))
		return false;

	return true;
}
