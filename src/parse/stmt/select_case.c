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

#include "ofc/parse.h"

static unsigned ofc_parse_stmt_select_case__case(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i;

	i = ofc_parse_keyword(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_CASE);
	if (i == 0) return 0;

	unsigned len;
	unsigned l;
	l = ofc_parse_keyword(
		src, &ptr[i], debug,
		OFC_PARSE_KEYWORD_DEFAULT);

	ofc_parse_array_index_t* array = NULL;
	if (l > 0)
	{
		i += l;
	}
	else
	{
		array = ofc_parse_array_index(
			src, &ptr[i], debug, &len);
		if (!array)
		{
			ofc_parse_debug_rewind(debug, dpos);
			return 0;
		}
		i += len;
	}

	if (!ofc_is_end_statement(&ptr[i], &len))
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	unsigned indx = stmt->select_case.count;

	ofc_parse_array_index_t** nindex
		= (ofc_parse_array_index_t**)realloc(stmt->select_case.case_value,
			(sizeof(ofc_parse_array_index_t*) * (indx + 1)));
	if (!nindex)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	stmt->select_case.case_value = nindex;
	stmt->select_case.case_value[indx] = array;

	ofc_parse_stmt_list_t** nblock
		= (ofc_parse_stmt_list_t**)realloc(stmt->select_case.case_block,
			(sizeof(ofc_parse_stmt_list_t*) * (indx + 1)));
	if (!nblock)
	{
		ofc_parse_array_index_delete(array);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}

	stmt->select_case.case_block = nblock;
	stmt->select_case.case_block[indx]
		= ofc_parse_stmt_list(src, &ptr[i], debug, &len);
	if (stmt->select_case.case_block[indx])
	{
		if (ofc_parse_stmt_list_contains_error(
			stmt->select_case.case_block[indx]))
		{
			/* Don't rewind cause we want to report the error. */
			ofc_parse_array_index_delete(array);
			ofc_parse_stmt_list_delete(
				stmt->select_case.case_block[indx]);
			return 0;
		}

		i += len;
	}
	stmt->select_case.count += 1;

	return i;
}

unsigned ofc_parse_stmt_select_case(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = ofc_parse_keyword(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_SELECT);
	if (i == 0) return 0;

	unsigned l = ofc_parse_keyword(
		src, &ptr[i], debug,
		OFC_PARSE_KEYWORD_CASE);
	if (l == 0) return 0;

	i += l;

	if (ptr[i++] != '(')
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}

	unsigned len;
	stmt->select_case.case_expr
		= ofc_parse_expr(src, &ptr[i], debug, &len);
	if (!stmt->select_case.case_expr)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	if (ptr[i++] != ')')
	{
		ofc_parse_expr_delete(
			stmt->select_case.case_expr);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}

	if (!ofc_is_end_statement(&ptr[i], &len))
	{
		ofc_parse_expr_delete(
			stmt->select_case.case_expr);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	stmt->type = OFC_PARSE_STMT_SELECT_CASE;
	stmt->select_case.count = 0;
	stmt->select_case.case_value = NULL;
	stmt->select_case.case_block = NULL;

	l = ofc_parse_keyword(
		src, &ptr[i], debug,
		OFC_PARSE_KEYWORD_CASE);
	while (l > 0)
	{
		unsigned k
			= ofc_parse_stmt_select_case__case(
				src, &ptr[i], debug, stmt);
		if (k == 0)
		{
			ofc_parse_expr_delete(
				stmt->select_case.case_expr);
			ofc_parse_list_delete(
				stmt->select_case.count,
				(void**)stmt->select_case.case_value,
				(void*)ofc_parse_array_index_delete);
			ofc_parse_list_delete(
				stmt->select_case.count,
				(void**)stmt->select_case.case_block,
				(void*)ofc_parse_stmt_list_delete);
				ofc_parse_debug_rewind(debug, dpos);

			return 0;
		}

		i += k;

		l = ofc_parse_keyword(
			src, &ptr[i], debug,
			OFC_PARSE_KEYWORD_CASE);
	}

	len = ofc_parse_keyword_end(
		src, &ptr[i], debug,
		OFC_PARSE_KEYWORD_SELECT, false);
	if (len == 0)
	{
		ofc_sparse_error_ptr(src, &ptr[i],
			"Invalid statement in SELECT CASE body");

		ofc_parse_expr_delete(
			stmt->select_case.case_expr);
		ofc_parse_list_delete(
			stmt->select_case.count,
			(void**)stmt->select_case.case_value,
			(void*)ofc_parse_array_index_delete);
		ofc_parse_list_delete(
			stmt->select_case.count,
			(void**)stmt->select_case.case_block,
			(void*)ofc_parse_stmt_list_delete);

		stmt->type = OFC_PARSE_STMT_ERROR;
	}

	stmt->select_case.end_select_case_has_label
		= ofc_sparse_label_find(src, &ptr[i],
			&stmt->select_case.end_select_case_label);

	i += len;

	return i;
}

bool ofc_parse_stmt_select_case_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt || (stmt->type != OFC_PARSE_STMT_SELECT_CASE))
		return false;

	if (!ofc_colstr_keyword_atomic_writez(cs, "SELECT")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_colstr_keyword_atomic_writez(cs, "CASE")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_colstr_atomic_writef(cs, "("))
		return false;
	if (stmt->select_case.case_expr
		&& !ofc_parse_expr_print(cs, stmt->select_case.case_expr))
		return false;
	if (!ofc_colstr_atomic_writef(cs, ")"))
		return false;

	unsigned i;
	for (i = 0; i < stmt->select_case.count; i++)
	{
		if (!ofc_colstr_newline(cs, (indent + 1), NULL))
			return false;

		if (!ofc_colstr_keyword_atomic_writez(cs, "CASE")
			|| !ofc_colstr_atomic_writef(cs, " "))
			return false;

		if (!stmt->select_case.case_value[i])
		{
			if (!ofc_colstr_keyword_atomic_writez(cs, "DEFAULT"))
				return false;
		}
		else
		{
			if (!ofc_parse_array_index_print(cs,
				stmt->select_case.case_value[i], false))
				return false;
		}

		if (stmt->select_case.case_block[i]
			&& !ofc_parse_stmt_list_print(
				cs, (indent + 2), stmt->select_case.case_block[i]))
			return false;
	}

	const unsigned* ulabel = (stmt->select_case.end_select_case_has_label
		? &stmt->select_case.end_select_case_label : NULL);
	if (!ofc_colstr_newline(cs, indent, ulabel))
			return false;

	if (!ofc_colstr_keyword_atomic_writez(cs, "END SELECT"))
		return false;

	return true;
}
