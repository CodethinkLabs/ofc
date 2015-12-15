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

/* This is hacky, but it means we can suppress redeclarations of the same program. */
static ofc_str_ref_t ofc_parse_stmt_program__current = OFC_STR_REF_EMPTY;

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

	if (!ofc_sparse_ref_empty(stmt->program.name))
	{
		ofc_lang_opts_t opts = ofc_sparse_lang_opts(src);
		if (opts.case_sensitive
			? ofc_str_ref_equal(stmt->program.name.string,
				ofc_parse_stmt_program__current)
			: ofc_str_ref_equal_ci(stmt->program.name.string,
				ofc_parse_stmt_program__current))
		{
			stmt->type = OFC_PARSE_STMT_EMPTY;
			return i;
		}
	}

	unsigned len;
	if (!ofc_is_end_statement(&ptr[i], &len))
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	ofc_str_ref_t prev_program_name = ofc_parse_stmt_program__current;
	ofc_parse_stmt_program__current = stmt->program.name.string;

	len = ofc_parse_stmt_program__body(
		src, &ptr[i], debug,
		OFC_PARSE_KEYWORD_PROGRAM, stmt);
	if (len == 0)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	ofc_parse_stmt_program__current = prev_program_name;

	stmt->program.type = NULL;
	stmt->program.args = NULL;

	stmt->type = OFC_PARSE_STMT_PROGRAM;

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


/* This is hacky, but it means we can suppress redeclarations of the same block_data. */
static ofc_str_ref_t ofc_parse_stmt_block_data__current = OFC_STR_REF_EMPTY;

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

	if (!ofc_sparse_ref_empty(stmt->program.name))
	{
		ofc_lang_opts_t opts = ofc_sparse_lang_opts(src);
		if (opts.case_sensitive
			? ofc_str_ref_equal(stmt->program.name.string,
				ofc_parse_stmt_block_data__current)
			: ofc_str_ref_equal_ci(stmt->program.name.string,
				ofc_parse_stmt_block_data__current))
		{
			stmt->type = OFC_PARSE_STMT_EMPTY;
			return i;
		}
	}

	unsigned len;
	if (!ofc_is_end_statement(&ptr[i], &len))
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	ofc_str_ref_t prev_block_data_name = ofc_parse_stmt_block_data__current;
	ofc_parse_stmt_block_data__current = stmt->program.name.string;

	len = ofc_parse_stmt_program__body(
		src, &ptr[i], debug,
		OFC_PARSE_KEYWORD_BLOCK_DATA, stmt);
	if (len == 0)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	ofc_parse_stmt_block_data__current = prev_block_data_name;

	stmt->program.type = NULL;
	stmt->program.args = NULL;

	stmt->type = OFC_PARSE_STMT_BLOCK_DATA;

	return i;
}



bool ofc_parse_stmt_program_print(
	ofc_colstr_t* cs,  unsigned indent, const ofc_parse_stmt_t* stmt)
{
	if (!stmt) return false;

	switch (stmt->type)
	{
		case OFC_PARSE_STMT_PROGRAM:
		case OFC_PARSE_STMT_SUBROUTINE:
		case OFC_PARSE_STMT_FUNCTION:
		case OFC_PARSE_STMT_BLOCK_DATA:
			break;
		default:
			return false;
	}

	if (stmt->program.type)
	{
		if (!ofc_parse_type_print(cs, stmt->program.type, false)
			|| !ofc_colstr_atomic_writef(cs, " "))
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
		case OFC_PARSE_STMT_BLOCK_DATA:
			kwstr = "BLOCK DATA";
			break;
		default:
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

	if (stmt->program.body
		&& !ofc_parse_stmt_list_print(
			cs, (indent + 1), stmt->program.body))
		return false;

	if (!ofc_colstr_newline(cs, indent, NULL))
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
