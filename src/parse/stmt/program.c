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


unsigned ofc_parse_stmt_program__body(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_keyword_e keyword,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = 0;
	stmt->program.body = ofc_parse_stmt_list(src, ptr, debug, &i);
	if (stmt->program.body)
	{
		if (ofc_parse_stmt_list_contains_error(
			stmt->program.body))
		{
			/* Don't rewind cause we want to report the error. */
			ofc_parse_stmt_list_delete(
				stmt->program.body);
			return 0;
		}
	}
	else
	{
		ofc_parse_debug_warning(debug,
			ofc_sparse_ref(src, &ptr[i], 0),
			"Empty %s body", ofc_parse_keyword_name(keyword));
	}

	stmt->program.end_label = 0;
	stmt->program.end_has_label
		= ofc_sparse_label_find(src, &ptr[i],
			&stmt->program.end_label);

	unsigned len = ofc_parse_keyword_end_named(
		src, &ptr[i], debug,
		keyword, false,
		&stmt->program.name);
	if (len == 0)
	{
		ofc_sparse_error_ptr(src, &ptr[i],
			"Invalid statement in %s body",
			ofc_parse_keyword_name(keyword));
		ofc_parse_stmt_list_delete(stmt->program.body);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	return i;
}


unsigned ofc_parse_stmt_program(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	stmt->program.name = OFC_SPARSE_REF_EMPTY;
	unsigned i = ofc_parse_keyword_named(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_PROGRAM,
		&stmt->program.name);
	if (i == 0) return 0;

	unsigned len;
	if (!ofc_is_end_statement(&ptr[i], &len))
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	len = ofc_parse_stmt_program__body(
		src, &ptr[i], debug,
		OFC_PARSE_KEYWORD_PROGRAM, stmt);
	if (len == 0)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	stmt->program.type = NULL;
	stmt->program.args = NULL;

	stmt->type = OFC_PARSE_STMT_PROGRAM;

	return i;
}

unsigned ofc_parse_stmt_program_end(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_list_t* list)
{
	if (!list)
		return 0;

	unsigned i = ofc_parse_keyword_end(
		src, ptr, debug, OFC_PARSE_KEYWORD_PROGRAM, false);
	if (i == 0) return 0;

	ofc_parse_stmt_t* stmt
		= (ofc_parse_stmt_t*)malloc(
			sizeof(ofc_parse_stmt_t));
	if(!stmt) return 0;

	ofc_parse_stmt_list_t* body
		= ofc_parse_stmt_list_create();
	if (!body)
	{
		free(stmt);
		return 0;
	}

	stmt->type = OFC_PARSE_STMT_PROGRAM;
	stmt->label = 0;
	stmt->src = ofc_sparse_ref(src, ptr, i);

	if (list->count > 0)
		ofc_sparse_ref_bridge(list->stmt[0]->src, stmt->src, &stmt->src);

	stmt->program.name = OFC_SPARSE_REF_EMPTY;
	stmt->program.type = NULL;
	stmt->program.args = NULL;
	stmt->program.body = body;

	/* TODO - Allow handling of label 0? */
	stmt->program.end_label = 0;
	stmt->program.end_has_label
		= ofc_sparse_label_find(
			src, ptr, &stmt->program.end_label);

	if (!ofc_parse_stmt_list_add(body, stmt))
	{
		free(stmt);
		ofc_parse_stmt_list_delete(body);
		return 0;
	}

	{
		ofc_parse_stmt_list_t swap = *list;
		*list = *body;
		*body = swap;
	}

	return i;
}


unsigned ofc_parse_stmt_subroutine(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	stmt->program.name = OFC_SPARSE_REF_EMPTY;
	unsigned i = ofc_parse_keyword_named(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_SUBROUTINE,
		&stmt->program.name);
	if (i == 0) return 0;

	stmt->program.args = NULL;
	if (ptr[i] == '(')
	{
		i += 1;

		unsigned len;
		stmt->program.args = ofc_parse_call_arg_list(
			src, &ptr[i], debug, &len);
		if (stmt->program.args) i += len;

		if (ptr[i++] != ')')
		{
			ofc_parse_call_arg_list_delete(stmt->program.args);
			ofc_parse_debug_rewind(debug, dpos);
			return 0;
		}
	}

	unsigned len;
	if (!ofc_is_end_statement(&ptr[i], &len))
	{
		ofc_parse_call_arg_list_delete(stmt->program.args);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	len = ofc_parse_stmt_program__body(
		src, &ptr[i], debug,
		OFC_PARSE_KEYWORD_SUBROUTINE, stmt);
	if (len == 0)
	{
		ofc_parse_call_arg_list_delete(stmt->program.args);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	stmt->program.type = NULL;

	stmt->type = OFC_PARSE_STMT_SUBROUTINE;
	return i;
}


unsigned ofc_parse_stmt_function(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = 0;
	stmt->program.type = ofc_parse_type(
		src, ptr, debug, &i);

	stmt->program.name = OFC_SPARSE_REF_EMPTY;
	unsigned len = ofc_parse_keyword_named(
		src, &ptr[i], debug, OFC_PARSE_KEYWORD_FUNCTION,
		&stmt->program.name);
	if (len == 0)
	{
		ofc_parse_type_delete(stmt->program.type);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	if (ptr[i] == '*')
	{
		/* TODO - Allow star kind for function */

		ofc_parse_type_delete(stmt->program.type);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}

	if (ptr[i++] != '(')
	{
		ofc_parse_type_delete(stmt->program.type);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}

	stmt->program.args = ofc_parse_call_arg_list(
		src, &ptr[i], debug, &len);
	if (stmt->program.args) i += len;

	if (ptr[i++] != ')')
	{
		ofc_parse_call_arg_list_delete(stmt->program.args);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}

	if (!ofc_is_end_statement(&ptr[i], &len))
	{
		ofc_parse_type_delete(stmt->program.type);
		ofc_parse_call_arg_list_delete(stmt->program.args);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	len = ofc_parse_stmt_program__body(
		src, &ptr[i], debug,
		OFC_PARSE_KEYWORD_FUNCTION, stmt);
	if (len == 0)
	{
		ofc_parse_type_delete(stmt->program.type);
		ofc_parse_call_arg_list_delete(stmt->program.args);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	stmt->type = OFC_PARSE_STMT_FUNCTION;
	return i;
}


unsigned ofc_parse_stmt_module(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	stmt->program.name = OFC_SPARSE_REF_EMPTY;
	unsigned i = ofc_parse_keyword_named(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_MODULE,
		&stmt->program.name);
	if (i == 0) return 0;

	if (ptr[i] == '\n')
		i++;

	unsigned len = ofc_parse_stmt_program__body(
		src, &ptr[i], debug,
		OFC_PARSE_KEYWORD_MODULE, stmt);
	if (len == 0)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	stmt->program.type = NULL;
	stmt->program.args = NULL;

	stmt->type = OFC_PARSE_STMT_MODULE;

	return i;

}


unsigned ofc_parse_stmt_block_data(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	stmt->program.name = OFC_SPARSE_REF_EMPTY;
	unsigned i = ofc_parse_keyword_named(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_BLOCK_DATA,
		&stmt->program.name);
	if (i == 0) return 0;

	unsigned len;
	if (!ofc_is_end_statement(&ptr[i], &len))
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	len = ofc_parse_stmt_program__body(
		src, &ptr[i], debug,
		OFC_PARSE_KEYWORD_BLOCK_DATA, stmt);
	if (len == 0)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	stmt->program.type = NULL;
	stmt->program.args = NULL;

	stmt->type = OFC_PARSE_STMT_BLOCK_DATA;

	return i;
}



bool ofc_parse_stmt_program_print(
	ofc_colstr_t* cs, unsigned indent, const ofc_parse_stmt_t* stmt)
{
	if (!stmt) return false;

	bool implicit = false;
	switch (stmt->type)
	{
		case OFC_PARSE_STMT_PROGRAM:
			implicit = ofc_sparse_ref_empty(stmt->program.name);
			break;
		case OFC_PARSE_STMT_SUBROUTINE:
		case OFC_PARSE_STMT_FUNCTION:
		case OFC_PARSE_STMT_MODULE:
		case OFC_PARSE_STMT_BLOCK_DATA:
			break;
		default:
			return false;
	}

	const char* kwstr;
	bool has_args = false;
	switch (stmt->type)
	{
		case OFC_PARSE_STMT_PROGRAM:
			kwstr = "PROGRAM";
			break;
		case OFC_PARSE_STMT_SUBROUTINE:
			kwstr = "SUBROUTINE";
			has_args = true;
			break;
		case OFC_PARSE_STMT_FUNCTION:
			kwstr = "FUNCTION";
			has_args = true;
			break;
		case OFC_PARSE_STMT_MODULE:
			kwstr = "MODULE";
			break;
		case OFC_PARSE_STMT_BLOCK_DATA:
			kwstr = "BLOCK DATA";
			break;
		default:
			return false;
	}

	if (implicit)
	{
		/* TODO - Get rid of this and the extra new-line that's printed. */
		if (!ofc_colstr_atomic_writef(cs, "! Implicit PROGRAM"))
			return false;
	}
	else
	{
		if (stmt->program.type)
		{
			if (!ofc_parse_type_print(cs, stmt->program.type, false)
				|| !ofc_colstr_atomic_writef(cs, " "))
				return false;
		}

		if (!ofc_colstr_atomic_writef(cs, "%s", kwstr))
					return false;

		if (!ofc_sparse_ref_empty(stmt->program.name))
		{
			if (!ofc_colstr_atomic_writef(cs, " ")
				|| !ofc_sparse_ref_print(cs, stmt->program.name))
				return false;
		}

		if (has_args)
		{
			if (!ofc_colstr_atomic_writef(cs, "("))
				return false;

			if (stmt->program.args
				&& !ofc_parse_call_arg_list_print(
					cs, stmt->program.args))
				return false;

			if (!ofc_colstr_atomic_writef(cs, ")"))
				return false;
		}
	}

	if (stmt->program.body
		&& !ofc_parse_stmt_list_print(
			cs, (indent + 1), stmt->program.body))
		return false;

	const unsigned* ulabel = (stmt->program.end_has_label
		? &stmt->program.end_label : NULL);
	if (!ofc_colstr_newline(cs, indent, ulabel))
		return false;

	unsigned i;
	for (i = 0; i < indent; i++)
	{
		if (!ofc_colstr_atomic_writef(cs, "  "))
			return false;
	}

	if (!ofc_colstr_atomic_writef(cs, "END %s", kwstr))
		return false;

	if (!ofc_sparse_ref_empty(stmt->program.name))
	{
		if (!ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_sparse_ref_print(cs, stmt->program.name))
			return false;
	}

	return true;
}
