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

static unsigned ofc_parse_stmt_if__computed(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_expr_t* cond,
	ofc_parse_stmt_t* stmt)
{
	unsigned i;
	stmt->if_comp.label
		= ofc_parse_expr_list(src, ptr, debug, &i);
	if (!stmt->if_comp.label) return 0;

	stmt->type = OFC_PARSE_STMT_IF_COMPUTED;
	stmt->if_comp.cond = cond;
	return i;
}

static unsigned ofc_parse_stmt_if__statement(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_expr_t* cond,
	ofc_parse_stmt_t* stmt)
{
	unsigned i;
	stmt->if_stmt.stmt = ofc_parse_stmt(
		NULL, src, ptr, debug, &i);
	if (!stmt->if_stmt.stmt)
		return 0;

	/* Don't absorb the end of statement here. */
	if (ptr[i] != '\0')
		i -= 1;

	stmt->type = OFC_PARSE_STMT_IF_STATEMENT;
	stmt->if_stmt.cond = cond;
	return i;
}

static unsigned ofc_parse_stmt_if__then(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_expr_t* cond,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i;

	i = ofc_parse_keyword(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_THEN);
	if (i == 0) return 0;

	unsigned len;
	/* TODO - Make this optional? */
	if (!ofc_is_end_statement(&ptr[i], &len))
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	stmt->if_then.block_then
		= ofc_parse_stmt_list(src, &ptr[i], debug, &len);
	if (stmt->if_then.block_then)
	{
		if (ofc_parse_stmt_list_contains_error(
			stmt->if_then.block_then))
		{
			/* Don't rewind cause we want to report the error. */
			ofc_parse_stmt_list_delete(
				stmt->if_then.block_then);
			return 0;
		}

		i += len;
	}

	bool expect_end = true;

	stmt->if_then.block_else = NULL;
	len = ofc_parse_keyword(
		src, &ptr[i], debug,
		OFC_PARSE_KEYWORD_ELSE);
	bool has_else = (len > 0);
	if (has_else)
	{
		i += len;

		ofc_parse_stmt_t* stmt_else
			= ofc_parse_stmt(NULL, src, &ptr[i], debug, &len);
		if (stmt_else
			&& (stmt_else->type != OFC_PARSE_STMT_IF_THEN))
		{
			ofc_parse_stmt_delete(stmt_else);
			len = 0;
		}

		if (len > 0)
		{
			/* Don't absorb the end of statement here. */
			if (ptr[i + len] != '\0')
				len -= 1;

			i += len;
			expect_end = false;

			stmt->if_then.block_else
				= (ofc_parse_stmt_list_t*)malloc(
					sizeof(ofc_parse_stmt_list_t));
			if (!stmt->if_then.block_else)
			{
				ofc_parse_stmt_delete(stmt_else);
				ofc_parse_stmt_list_delete(stmt->if_then.block_then);
				ofc_parse_debug_rewind(debug, dpos);
				return 0;
			}

			stmt->if_then.block_else->stmt
				= (ofc_parse_stmt_t**)malloc(
					sizeof(ofc_parse_stmt_t*));
			if (!stmt->if_then.block_else->stmt)
			{
				free(stmt->if_then.block_else);
				ofc_parse_stmt_delete(stmt_else);
				ofc_parse_stmt_list_delete(stmt->if_then.block_then);
				ofc_parse_debug_rewind(debug, dpos);
				return 0;
			}

			stmt->if_then.block_else->count = 1;
			stmt->if_then.block_else->stmt[0] = stmt_else;
		}
		else
		{
			/* TODO - Make this optional? */
			if (!ofc_is_end_statement(&ptr[i], &len))
			{
				ofc_parse_debug_rewind(debug, dpos);
				return 0;
			}
			i += len;

			stmt->if_then.block_else
				= ofc_parse_stmt_list(src, &ptr[i], debug, &len);
			if (stmt->if_then.block_else)
			{
				if (ofc_parse_stmt_list_contains_error(
					stmt->if_then.block_else))
				{
					/* Don't rewind cause we want to report the error. */
					ofc_parse_stmt_list_delete(
						stmt->if_then.block_else);
					ofc_parse_stmt_list_delete(
						stmt->if_then.block_then);
					return 0;
				}

				i += len;
			}
		}
	}

	if (expect_end)
	{
		len = ofc_parse_keyword_end(
			src, &ptr[i], debug,
			OFC_PARSE_KEYWORD_IF, false);
		if (len == 0)
		{
			ofc_sparse_error_ptr(src, &ptr[i],
				"Invalid statement in %s body",
				(has_else ? "ELSE" : "IF THEN"));

			ofc_parse_stmt_list_delete(
				stmt->if_then.block_else);
			ofc_parse_stmt_list_delete(
				stmt->if_then.block_then);

			stmt->type = OFC_PARSE_STMT_ERROR;
			return i;
		}

		stmt->if_then.end_if_has_label = ofc_sparse_label_find(
			src, &ptr[i], &stmt->if_then.end_if_label);

		i += len;
	}

	stmt->type = OFC_PARSE_STMT_IF_THEN;
	stmt->if_then.cond = cond;
	return i;
}


unsigned ofc_parse_stmt_if(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = ofc_parse_keyword(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_IF);
	if (i == 0) return 0;

	if (ptr[i++] != '(')
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}

	unsigned len;
	ofc_parse_expr_t* cond = ofc_parse_expr(
		src, &ptr[i], debug, &len);
	if (!cond)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	if (ptr[i++] != ')')
	{
		ofc_parse_expr_delete(cond);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}

	len = ofc_parse_stmt_if__then(src, &ptr[i], debug, cond, stmt);
	if (len == 0)
		len = ofc_parse_stmt_if__statement(src, &ptr[i], debug, cond, stmt);
	if (len == 0)
		len = ofc_parse_stmt_if__computed(src, &ptr[i], debug, cond, stmt);

	if (len == 0)
	{
		ofc_parse_expr_delete(cond);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	else if (stmt->type == OFC_PARSE_STMT_ERROR)
	{
		ofc_parse_expr_delete(cond);
	}
	i += len;

	return i;
}

bool ofc_parse_stmt_if_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	if (stmt->type == OFC_PARSE_STMT_IF_COMPUTED)
	{
		if (!ofc_colstr_keyword_atomic_writez(cs, "IF")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_atomic_writef(cs, "(")
			|| !ofc_parse_expr_print(cs, stmt->if_comp.cond)
			|| !ofc_colstr_atomic_writef(cs, ")")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_parse_expr_list_print(cs, stmt->if_comp.label))
			return false;
	}
	else if (stmt->type == OFC_PARSE_STMT_IF_STATEMENT)
	{
		if (!ofc_colstr_keyword_atomic_writez(cs, "IF")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_atomic_writef(cs, "(")
			|| !ofc_parse_expr_print(cs, stmt->if_stmt.cond)
			|| !ofc_colstr_atomic_writef(cs, ")")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_parse_stmt_print(cs, indent, stmt->if_stmt.stmt))
			return false;
	}
	else if (stmt->type == OFC_PARSE_STMT_IF_THEN)
	{
		if (!ofc_colstr_keyword_atomic_writez(cs, "IF")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_atomic_writef(cs, "(")
			|| !ofc_parse_expr_print(cs, stmt->if_then.cond)
			|| !ofc_colstr_atomic_writef(cs, ")")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_keyword_atomic_writez(cs, "THEN"))
			return false;

		if (stmt->if_then.block_then
			&& !ofc_parse_stmt_list_print(
				cs, (indent + 1), stmt->if_then.block_then))
			return false;

		if (stmt->if_then.block_else)
		{
			if (!ofc_colstr_newline(cs, indent, NULL))
				return false;

			unsigned i;
			for (i = 0; i < indent; i++)
			{
				if (!ofc_colstr_atomic_writef(cs, "  "))
					return false;
			}

			if (!ofc_colstr_keyword_atomic_writez(cs, "ELSE"))
				return false;

			if (!ofc_parse_stmt_list_print(
				cs, (indent + 1), stmt->if_then.block_else))
				return false;
		}

		const unsigned* ulabel = (stmt->if_then.end_if_has_label
			? &stmt->if_then.end_if_label : NULL);
		if (!ofc_colstr_newline(cs, indent, ulabel))
				return false;

		if (!ofc_colstr_keyword_atomic_writez(cs, "END IF"))
			return false;
	}
	else
	{
		return false;
	}

	return true;
}
