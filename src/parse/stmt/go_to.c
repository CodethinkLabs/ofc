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

#include "ofc/parse.h"

static unsigned ofc_parse_stmt_go_to_unconditional(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned i;
	stmt->go_to.label
		= ofc_parse_expr_integer_variable(
			src, ptr, debug, &i);
	if (!stmt->go_to.label)
		return 0;

	stmt->type = OFC_PARSE_STMT_GO_TO;
	return i;
}

static unsigned ofc_parse_stmt_go_to_assigned(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = 0;

	unsigned len;
	stmt->go_to_list.cond
		= ofc_parse_expr_integer_variable(
			src, &ptr[i], debug, &len);
	if (!stmt->go_to_list.cond)
		return 0;
	i += len;

	if (ptr[i] == ',')
		i += 1;

	if (ptr[i++] != '(')
	{
		ofc_parse_expr_delete(
			stmt->go_to_list.cond);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}

	stmt->go_to_list.label
		= ofc_parse_expr_list(
			src, &ptr[i], debug, &len);
	if (!stmt->go_to_list.label)
	{
		ofc_parse_expr_delete(
			stmt->go_to_list.cond);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	if (ptr[i++] != ')')
	{
		ofc_parse_expr_list_delete(
			stmt->go_to_list.label);
		ofc_parse_expr_delete(
			stmt->go_to_list.cond);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}

	stmt->type = OFC_PARSE_STMT_GO_TO_ASSIGNED;
	return i;
}

static unsigned ofc_parse_stmt_go_to_computed(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned i = 0;

	if (ptr[i++] != '(')
		return 0;

	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned len;
	stmt->go_to_list.label
		= ofc_parse_expr_list(
			src, &ptr[i], debug, &len);
	if (!stmt->go_to_list.label)
		return 0;
	i += len;

	if (ptr[i++] != ')')
	{
		ofc_parse_expr_list_delete(
			stmt->go_to_list.label);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}

	if (ptr[i] == ',')
		i += 1;

	stmt->go_to_list.cond = ofc_parse_expr(
		src, &ptr[i], debug, &len);
	if (!stmt->go_to_list.cond)
	{
		ofc_parse_expr_list_delete(
			stmt->go_to_list.label);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	stmt->type = OFC_PARSE_STMT_GO_TO_COMPUTED;
	return i;
}

unsigned ofc_parse_stmt_go_to(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = ofc_parse_keyword(
		src, ptr, debug, OFC_PARSE_KEYWORD_GO_TO);
	if (i == 0) return 0;

	unsigned len = 0;
	if (len == 0) len = ofc_parse_stmt_go_to_assigned(src, &ptr[i], debug, stmt);
	if (len == 0) len = ofc_parse_stmt_go_to_computed(src, &ptr[i], debug, stmt);
	if (len == 0) len = ofc_parse_stmt_go_to_unconditional(src, &ptr[i], debug, stmt);

	if (len == 0)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	return i;
}



static bool ofc_parse_stmt_go_to_assigned_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
    return (ofc_colstr_keyword_atomic_writez(cs, "GO TO")
		&& ofc_colstr_atomic_writef(cs, " ")
		&& ofc_parse_expr_print(cs, stmt->go_to_list.cond)
		&& ofc_colstr_atomic_writef(cs, ", (")
		&& ofc_parse_expr_list_print(cs, stmt->go_to_list.label)
		&& ofc_colstr_atomic_writef(cs, ")"));
}

static bool ofc_parse_stmt_go_to_computed_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
return (ofc_colstr_keyword_atomic_writez(cs, "GO TO")
		&& ofc_colstr_atomic_writef(cs, " ")
		&& ofc_colstr_atomic_writef(cs, "(")
		&& ofc_parse_expr_list_print(cs, stmt->go_to_list.label)
		&& ofc_colstr_atomic_writef(cs, "), ")
		&& ofc_parse_expr_print(cs, stmt->go_to_list.cond));
}

static bool ofc_parse_stmt_go_to_unconditional_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
	return (ofc_colstr_keyword_atomic_writez(cs, "GO TO")
		&& ofc_colstr_atomic_writef(cs, " ")
		&& ofc_parse_expr_print(cs, stmt->go_to.label));
}

bool ofc_parse_stmt_go_to_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	switch (stmt->type)
	{
		case OFC_PARSE_STMT_GO_TO_ASSIGNED:
			return ofc_parse_stmt_go_to_assigned_print(cs, stmt);
		case OFC_PARSE_STMT_GO_TO_COMPUTED:
			return ofc_parse_stmt_go_to_computed_print(cs, stmt);
		case OFC_PARSE_STMT_GO_TO:
			return ofc_parse_stmt_go_to_unconditional_print(cs, stmt);
		default:
			break;
	}

	return false;
}
