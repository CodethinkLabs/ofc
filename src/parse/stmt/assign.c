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


unsigned ofc_parse_stmt_assign(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = ofc_parse_keyword(
		src, ptr, debug, OFC_PARSE_KEYWORD_ASSIGN);
	if (i == 0) return 0;

	unsigned len;
	stmt->assign.label = ofc_parse_expr_integer(
		src, &ptr[i], debug, &len);
	if (!stmt->assign.label)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	len = ofc_parse_keyword(
		src, &ptr[i], debug, OFC_PARSE_KEYWORD_TO);
	if (len == 0)
	{
		ofc_parse_debug_rewind(debug, dpos);
		ofc_parse_expr_delete(stmt->assign.label);
		return 0;
	}
	i += 2;

	len = ofc_parse_name(
		src, &ptr[i], debug,
		&stmt->assign.variable);
	if (len == 0)
	{
		ofc_parse_debug_rewind(debug, dpos);
		ofc_parse_expr_delete(stmt->assign.label);
		return 0;
	}
	i += len;

	stmt->type = OFC_PARSE_STMT_ASSIGN;
	return i;
}

bool ofc_parse_stmt_assign_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	return (ofc_colstr_keyword_atomic_writef(cs, "ASSIGN")
		&& ofc_colstr_atomic_writef(cs, " ")
		&& ofc_parse_expr_print(cs, stmt->assign.label)
		&& ofc_colstr_atomic_writef(cs, " ")
		&& ofc_colstr_keyword_atomic_writef(cs, "TO")
		&& ofc_colstr_atomic_writef(cs, " ")
		&& ofc_sparse_ref_print(cs, stmt->assign.variable));
}
