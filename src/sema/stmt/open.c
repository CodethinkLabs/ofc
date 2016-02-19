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

static void ofc_sema_stmt_io_open__cleanup(
	ofc_sema_stmt_t s)
{
	ofc_sema_expr_delete(s.io_open.unit);
	ofc_sema_expr_delete(s.io_open.access);
	ofc_sema_expr_delete(s.io_open.action);
	ofc_sema_expr_delete(s.io_open.blank);
	ofc_sema_expr_delete(s.io_open.delim);
	ofc_sema_expr_delete(s.io_open.err);
	ofc_sema_expr_delete(s.io_open.file);
	ofc_sema_expr_delete(s.io_open.form);
	ofc_sema_expr_delete(s.io_open.iostat);
	ofc_sema_expr_delete(s.io_open.pad);
	ofc_sema_expr_delete(s.io_open.position);
	ofc_sema_expr_delete(s.io_open.recl);
	ofc_sema_expr_delete(s.io_open.status);
}

ofc_sema_stmt_t* ofc_sema_stmt_io_open(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_IO_OPEN)
		|| !stmt->io.params)
		return NULL;

	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_IO_OPEN;
	s.io_open.unit          = NULL;
	s.io_open.access        = NULL;
	s.io_open.action        = NULL;
	s.io_open.blank         = NULL;
	s.io_open.delim         = NULL;
	s.io_open.err           = NULL;
	s.io_open.file          = NULL;
	s.io_open.form          = NULL;
	s.io_open.iostat        = NULL;
	s.io_open.pad           = NULL;
	s.io_open.position      = NULL;
	s.io_open.recl          = NULL;
	s.io_open.status        = NULL;

	ofc_parse_call_arg_t* ca_unit     = NULL;
	ofc_parse_call_arg_t* ca_access   = NULL;
	ofc_parse_call_arg_t* ca_action   = NULL;
	ofc_parse_call_arg_t* ca_blank    = NULL;
	ofc_parse_call_arg_t* ca_delim    = NULL;
	ofc_parse_call_arg_t* ca_err      = NULL;
	ofc_parse_call_arg_t* ca_file     = NULL;
	ofc_parse_call_arg_t* ca_form     = NULL;
	ofc_parse_call_arg_t* ca_iostat   = NULL;
	ofc_parse_call_arg_t* ca_pad      = NULL;
	ofc_parse_call_arg_t* ca_position = NULL;
	ofc_parse_call_arg_t* ca_recl     = NULL;
	ofc_parse_call_arg_t* ca_status   = NULL;

	unsigned i;
	for (i = 0; i < stmt->io.params->count; i++)
	{
		ofc_parse_call_arg_t* param
			= stmt->io.params->call_arg[i];
		if (!param) continue;

		if (ofc_sparse_ref_empty(param->name))
		{
			if (i >= 1)
			{
				ofc_sparse_ref_error(param->src,
					"Un-named parameter %u has no meaning in OPEN.", i);
				return NULL;
			}

			if (i == 0)
			{
				ca_unit = param;
			}
		}
		else if (ofc_str_ref_equal_strz_ci(param->name.string, "UNIT"))
		{
			if (ca_unit)
			{
				ofc_sparse_ref_error(param->src,
					"Re-definition of UNIT in OPEN.");
				return NULL;
			}

			ca_unit = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name.string, "ACCESS"))
		{
			if (ca_access)
			{
				ofc_sparse_ref_error(param->src,
					"Re-definition of ACCESS in OPEN.");
				return NULL;
			}

			ca_access = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name.string, "ACTION"))
		{
			if (ca_action)
			{
				ofc_sparse_ref_error(param->src,
					"Re-definition of ACTION in OPEN.");
				return NULL;
			}

			ca_action = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name.string, "BLANK"))
		{
			if (ca_blank)
			{
				ofc_sparse_ref_error(param->src,
					"Re-definition of BLANK in OPEN.");
				return NULL;
			}

			ca_blank = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name.string, "DELIM"))
		{
			if (ca_delim)
			{
				ofc_sparse_ref_error(param->src,
					"Re-definition of DELIM in OPEN.");
				return NULL;
			}

			ca_delim = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name.string, "ERR"))
		{
			if (ca_err)
			{
				ofc_sparse_ref_error(param->src,
					"Re-definition of ERR in OPEN.");
				return NULL;
			}

			ca_err = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name.string, "FILE"))
		{
			if (ca_file)
			{
				ofc_sparse_ref_error(param->src,
					"Re-definition of FILE in OPEN.");
				return NULL;
			}

			ca_file = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name.string, "FORM"))
		{
			if (ca_form)
			{
				ofc_sparse_ref_error(param->src,
					"Re-definition of FORM in OPEN.");
				return NULL;
			}

			ca_form = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name.string, "IOSTAT"))
		{
			if (ca_iostat)
			{
				ofc_sparse_ref_error(param->src,
					"Re-definition of IOSTAT in OPEN.");
				return NULL;
			}

			ca_iostat = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name.string, "PAD"))
		{
			if (ca_pad)
			{
				ofc_sparse_ref_error(param->src,
					"Re-definition of PAD in OPEN.");
				return NULL;
			}

			ca_pad = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name.string, "POSITION"))
		{
			if (ca_position)
			{
				ofc_sparse_ref_error(param->src,
					"Re-definition of POSITION in OPEN.");
				return NULL;
			}

			ca_position = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name.string, "RECL"))
		{
			if (ca_recl)
			{
				ofc_sparse_ref_error(param->src,
					"Re-definition of RECL in OPEN.");
				return NULL;
			}

			ca_recl = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name.string, "STATUS"))
		{
			if (ca_status)
			{
				ofc_sparse_ref_error(param->src,
					"Re-definition of STATUS in OPEN.");
				return NULL;
			}

			ca_status = param;
		}
		else
		{
			ofc_sparse_ref_error(param->src,
				"Unrecognized paramater %u name '%.*s' in OPEN.",
				i, param->name.string.size, param->name.string.base);
			return NULL;
		}
	}

	if (!ca_unit)
	{
		ofc_sparse_ref_error(stmt->src,
			"No UNIT defined in OPEN.");
		return NULL;
	}

	if (ca_unit->type == OFC_PARSE_CALL_ARG_EXPR)
	{
		s.io_open.unit = ofc_sema_expr(
			scope, ca_unit->expr);
		if (!s.io_open.unit) return NULL;

		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_open.unit);
		if (!etype)
		{
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_type_is_integer(etype))
		{
			ofc_sparse_ref_error(stmt->src,
				"UNIT must be of type INTEGER in OPEN");
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_expr_validate_uint(s.io_open.unit))
		{
			ofc_sparse_ref_error(stmt->src,
				   "UNIT must be a positive INTEGER in OPEN");
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}
	}
	else
	{
		ofc_sparse_ref_error(stmt->src,
			"UNIT must be an INTEGER expression in OPEN");
		return NULL;
	}

	bool access_type_direct = false;
	bool format_type_unformatted  = false;
	if (ca_access)
	{
		s.io_open.access = ofc_sema_expr(
			scope, ca_access->expr);
		if (!s.io_open.access)
		{
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_open.access);
		if (!etype)
		{
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}

		if (etype->type != OFC_SEMA_TYPE_CHARACTER)
		{
			ofc_sparse_ref_error(stmt->src,
				"ACCESS must be a CHARACTER expression in OPEN");
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}
		else
		{
			const ofc_sema_typeval_t* constant
				= ofc_sema_expr_constant(s.io_open.access);

			access_type_direct = (constant
				&& ofc_typeval_character_equal_strz_ci(constant, "DIRECT"));
			if (constant && !access_type_direct
				&& !ofc_typeval_character_equal_strz_ci(constant, "SEQUENTIAL"))
			{
				ofc_sparse_ref_error(stmt->src,
					"ACCESS must be SEQUENTIAL/DIRECT in OPEN");
				ofc_sema_stmt_io_open__cleanup(s);
				return NULL;
			}
			else if (access_type_direct)
			{
				/* Change default format */
				format_type_unformatted = true;

				if (!ca_recl)
				{
					ofc_sparse_ref_error(stmt->src,
						"Direct ACCESS must have a RECL specifier in OPEN");
					ofc_sema_stmt_io_open__cleanup(s);
					return NULL;
				}
			}
		}
	}

	if (ca_action)
	{
		s.io_open.action = ofc_sema_expr(
			scope, ca_action->expr);
		if (!s.io_open.action)
		{
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_open.action);
		if (!etype)
		{
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}

		if (etype->type != OFC_SEMA_TYPE_CHARACTER)
		{
			ofc_sparse_ref_error(stmt->src,
				"ACTION must be a CHARACTER expression in OPEN");
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}
		else
		{
			const ofc_sema_typeval_t* constant
				= ofc_sema_expr_constant(s.io_open.action);

			if (constant
				&& !ofc_typeval_character_equal_strz_ci(constant, "READWRITE")
				&& !ofc_typeval_character_equal_strz_ci(constant, "WRITE")
				&& !ofc_typeval_character_equal_strz_ci(constant, "READ"))
			{
				ofc_sparse_ref_error(stmt->src,
					"ACTION must be 'READ', 'WRITE' or 'READWRITE' in OPEN");
				ofc_sema_stmt_io_open__cleanup(s);
				return NULL;
			}
		}
	}

	if (ca_form)
	{
		s.io_open.form = ofc_sema_expr(
			scope, ca_form->expr);
		if (!s.io_open.form)
		{
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_open.form);
		if (!etype)
		{
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}

		if (etype->type != OFC_SEMA_TYPE_CHARACTER)
		{
			ofc_sparse_ref_error(stmt->src,
				"FORM must be a CHARACTER expression in OPEN");
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}
		else
		{
			const ofc_sema_typeval_t* constant
				= ofc_sema_expr_constant(s.io_open.form);

			format_type_unformatted = (constant
				&& ofc_typeval_character_equal_strz_ci(constant, "UNFORMATTED"));
			if (constant && !format_type_unformatted
				&& !ofc_typeval_character_equal_strz_ci(constant, "FORMATTED"))
			{
				ofc_sparse_ref_error(stmt->src,
					"FORM must be FORMATTED/UNFORMATTED in OPEN");
				ofc_sema_stmt_io_open__cleanup(s);
				return NULL;
			}
		}
	}

	if (ca_blank && format_type_unformatted)
	{
		ofc_sparse_ref_error(stmt->src,
			"BLANK can only be specified for formatted I/O in OPEN");
		ofc_sema_stmt_io_open__cleanup(s);
		return NULL;
	}
	else if (ca_blank)
	{
		s.io_open.blank = ofc_sema_expr(
			scope, ca_blank->expr);
		if (!s.io_open.blank)
		{
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_open.blank);
		if (!etype)
		{
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}

		if (etype->type != OFC_SEMA_TYPE_CHARACTER)
		{
			ofc_sparse_ref_error(stmt->src,
				"BLANK must be a CHARACTER expression in OPEN");
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}
		else
		{
			const ofc_sema_typeval_t* constant
				= ofc_sema_expr_constant(s.io_open.blank);

			if (constant
				&& !ofc_typeval_character_equal_strz_ci(constant, "ZERO")
				&& !ofc_typeval_character_equal_strz_ci(constant, "NULL"))
			{
				ofc_sparse_ref_error(stmt->src,
					"BLANK must be NULL/ZERO in OPEN");
				ofc_sema_stmt_io_open__cleanup(s);
				return NULL;
			}
		}
	}

	if (ca_delim && format_type_unformatted)
	{
		ofc_sparse_ref_error(stmt->src,
			"DELIM can only be specified for formatted I/O in OPEN");
		ofc_sema_stmt_io_open__cleanup(s);
		return NULL;
	}
	else if (ca_delim)
	{
		s.io_open.delim = ofc_sema_expr(
			scope, ca_delim->expr);
		if (!s.io_open.delim)
		{
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_open.delim);
		if (!etype)
		{
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}

		if (etype->type != OFC_SEMA_TYPE_CHARACTER)
		{
			ofc_sparse_ref_error(stmt->src,
				"DELIM must be a CHARACTER expression in OPEN");
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}
		else
		{
			const ofc_sema_typeval_t* constant
				= ofc_sema_expr_constant(s.io_open.delim);

			if (constant
				&& !ofc_typeval_character_equal_strz_ci(constant, "APOSTROPHE")
				&& !ofc_typeval_character_equal_strz_ci(constant, "QUOTE")
				&& !ofc_typeval_character_equal_strz_ci(constant, "NONE"))
			{
				ofc_sparse_ref_error(stmt->src,
					"DELIM must be APOSTROPHE/QUOTE/NONE in OPEN");
				ofc_sema_stmt_io_open__cleanup(s);
				return NULL;
			}
		}
	}

	if (ca_err)
	{
		s.io_open.err = ofc_sema_expr_label(
			scope, ca_err->expr);
		if (!s.io_open.err)
		{
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}
	}

	bool is_scratch = false;
	if (ca_status)
	{
		s.io_open.status = ofc_sema_expr(
			scope, ca_status->expr);
		if (!s.io_open.status)
		{
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_open.status);
		if (!etype)
		{
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}

		if (etype->type != OFC_SEMA_TYPE_CHARACTER)
		{
			ofc_sparse_ref_error(stmt->src,
				"STATUS must be a CHARACTER expression in OPEN");
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}
		else
		{
			const ofc_sema_typeval_t* constant
				= ofc_sema_expr_constant(s.io_open.status);
			is_scratch = (constant &&
				ofc_typeval_character_equal_strz_ci(constant, "SCRATCH"));
			if (constant && !is_scratch
				&& !ofc_typeval_character_equal_strz_ci(constant, "UNKNOWN")
				&& !ofc_typeval_character_equal_strz_ci(constant, "REPLACE")
				&& !ofc_typeval_character_equal_strz_ci(constant, "OLD")
				&& !ofc_typeval_character_equal_strz_ci(constant, "NEW"))
			{
				ofc_sparse_ref_error(stmt->src,
					"STATUS must be UNKNOWN/REPLACE/OLD/NEW/SCRATCH in OPEN");
				ofc_sema_stmt_io_open__cleanup(s);
				return NULL;
			}
		}
	}

	if (ca_file && is_scratch)
	{
		ofc_sparse_ref_error(stmt->src,
			"FILE can only be specified for non scratch files in OPEN");
		ofc_sema_stmt_io_open__cleanup(s);
		return NULL;
	}
	else if (ca_file)
	{
		s.io_open.file = ofc_sema_expr(
			scope, ca_file->expr);
		if (!s.io_open.file)
		{
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_open.file);
		if (!etype)
		{
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}

		if (etype->type != OFC_SEMA_TYPE_CHARACTER)
		{
			ofc_sparse_ref_error(stmt->src,
				"FILE must be a CHARACTER expression in OPEN");
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}
	}

	if (ca_iostat)
	{
		s.io_open.iostat = ofc_sema_expr(
			scope, ca_iostat->expr);
		if (!s.io_open.iostat)
		{
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}

		if (s.io_open.iostat->type != OFC_SEMA_EXPR_LHS)
		{
			ofc_sparse_ref_error(stmt->src,
				"IOSTAT must be a variable in OPEN");
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_open.iostat);
		if (!etype)
		{
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_type_is_integer(etype))
		{
			ofc_sparse_ref_error(stmt->src,
				"IOSTAT must be of type INTEGER in OPEN");
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}
	}

	if (ca_pad && format_type_unformatted)
	{
		ofc_sparse_ref_error(stmt->src,
			"PAD can only be specified for formatted I/O in OPEN");
		ofc_sema_stmt_io_open__cleanup(s);
		return NULL;
	}
	else if (ca_pad)
	{
		s.io_open.pad = ofc_sema_expr(
			scope, ca_pad->expr);
		if (!s.io_open.pad)
		{
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_open.pad);
		if (!etype)
		{
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}

		if (etype->type != OFC_SEMA_TYPE_CHARACTER)
		{
			ofc_sparse_ref_error(stmt->src,
				"PAD must be a CHARACTER expression in OPEN");
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}
		else
		{
			const ofc_sema_typeval_t* constant
				= ofc_sema_expr_constant(s.io_open.pad);

			if (constant
				&& !ofc_typeval_character_equal_strz_ci(constant, "NO")
				&& !ofc_typeval_character_equal_strz_ci(constant, "YES"))
			{
				ofc_sparse_ref_error(stmt->src,
					"PAD must be YES/NO in OPEN");
				ofc_sema_stmt_io_open__cleanup(s);
				return NULL;
			}
		}
	}

	if (ca_position && access_type_direct)
	{
		ofc_sparse_ref_error(stmt->src,
			"POSITION can only be specified for files with sequential access in OPEN");
		ofc_sema_stmt_io_open__cleanup(s);
		return NULL;
	}
	else if (ca_position)
	{
		s.io_open.position = ofc_sema_expr(
			scope, ca_position->expr);
		if (!s.io_open.position)
		{
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_open.position);
		if (!etype)
		{
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}

		if (etype->type != OFC_SEMA_TYPE_CHARACTER)
		{
			ofc_sparse_ref_error(stmt->src,
				"POSITION must be a CHARACTER expression in OPEN");
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}
		else
		{
			const ofc_sema_typeval_t* constant
				= ofc_sema_expr_constant(s.io_open.position);

			if (constant
				&& !ofc_typeval_character_equal_strz_ci(constant, "REWIND")
				&& !ofc_typeval_character_equal_strz_ci(constant, "APPEND")
				&& !ofc_typeval_character_equal_strz_ci(constant, "ASIS"))
			{
				ofc_sparse_ref_error(stmt->src,
					"POSITION must be REWIND/APPEND/ASIS in OPEN");
				ofc_sema_stmt_io_open__cleanup(s);
				return NULL;
			}
		}
	}

	if (ca_recl)
	{
		s.io_open.recl = ofc_sema_expr(
			scope, ca_recl->expr);
		if (!s.io_open.recl) return NULL;

		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_open.recl);
		if (!etype)
		{
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_type_is_integer(etype))
		{
			ofc_sparse_ref_error(stmt->src,
				"RECL must be of type INTEGER in OPEN");
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}

		const ofc_sema_typeval_t* evalue
			= ofc_sema_expr_constant(s.io_open.recl);
		if (evalue)
		{
			int64_t evalue64;
			bool success = ofc_sema_typeval_get_integer(
				evalue, &evalue64);

			if (success)
			{
				if (evalue64 < 0)
				{
					ofc_sparse_ref_error(stmt->src,
						"RECL must be a positive INTEGER in OPEN");
					ofc_sema_stmt_io_open__cleanup(s);
					return NULL;
				}

				/* TODO - Clamp to range? */
			}
        }
	}

	ofc_sema_stmt_t* as
		= ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_stmt_io_open__cleanup(s);
		return NULL;
	}
	return as;
}

static bool ofc_sema_stmt_open__print_optional(
	ofc_colstr_t* cs, const char* name,
	const ofc_sema_expr_t* expr)
{
	if (!cs || !expr)
		return false;

	if (!ofc_colstr_atomic_writef(cs, ",")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_colstr_atomic_writef(cs, name)
		|| !ofc_colstr_atomic_writef(cs, "= ")
		|| !ofc_sema_expr_print(cs, expr))
		return false;

	return true;
}

bool ofc_sema_stmt_io_open_print(
	ofc_colstr_t* cs,
	ofc_sema_stmt_t* stmt)
{
	if (!cs || !stmt || stmt->type != OFC_SEMA_STMT_IO_OPEN)
		return false;

	if (!ofc_colstr_atomic_writef(cs, "OPEN")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_colstr_atomic_writef(cs, "(")
		|| !ofc_sema_expr_print(cs, stmt->io_open.unit))
		return false;

	if (stmt->io_open.access)
	{
		if (!ofc_sema_stmt_open__print_optional(
			cs, "ACCESS", stmt->io_open.access))
			return false;
	}
	if (stmt->io_open.action)
	{
		if (!ofc_sema_stmt_open__print_optional(
			cs, "ACTION", stmt->io_open.action))
			return false;
	}
	if (stmt->io_open.blank)
	{
		if (!ofc_sema_stmt_open__print_optional(
			cs, "BLANK", stmt->io_open.blank))
			return false;
	}
	if (stmt->io_open.delim)
	{
		if (!ofc_sema_stmt_open__print_optional(
			cs, "DELIM", stmt->io_open.delim))
			return false;
	}
	if (stmt->io_open.err)
	{
		if (!ofc_sema_stmt_open__print_optional(
			cs, "ERR", stmt->io_open.err))
			return false;
	}
	if (stmt->io_open.file)
	{
		if (!ofc_sema_stmt_open__print_optional(
			cs, "FILE", stmt->io_open.file))
			return false;
	}
	if (stmt->io_open.form)
	{
		if (!ofc_sema_stmt_open__print_optional(
			cs, "FORM", stmt->io_open.form))
			return false;
	}
	if (stmt->io_open.iostat)
	{
		if (!ofc_sema_stmt_open__print_optional(
			cs, "IOSTAT", stmt->io_open.iostat))
			return false;
	}
	if (stmt->io_open.pad)
	{
		if (!ofc_sema_stmt_open__print_optional(
			cs, "PAD", stmt->io_open.pad))
			return false;
	}
	if (stmt->io_open.position)
	{
		if (!ofc_sema_stmt_open__print_optional(
			cs, "POSITION", stmt->io_open.position))
			return false;
	}
	if (stmt->io_open.recl)
	{
		if (!ofc_sema_stmt_open__print_optional(
			cs, "RECL", stmt->io_open.recl))
			return false;
	}
	if (stmt->io_open.status)
	{
		if (!ofc_sema_stmt_open__print_optional(
			cs, "STATUS", stmt->io_open.status))
			return false;
	}

	if (!ofc_colstr_atomic_writef(cs, ")"))
		return false;

	return true;

}
