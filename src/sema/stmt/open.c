#include <ofc/sema.h>

void ofc_sema_stmt_io_open__cleanup(
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

	/* Default status */
	s.io_open.access_type   = OFC_SEMA_CALL_ARG_SEQUENTIAL;
	s.io_open.action_type   = OFC_SEMA_CALL_ARG_COUNT;
	s.io_open.blank_type    = OFC_SEMA_CALL_ARG_BLANK_NULL;
	s.io_open.delim_type    = OFC_SEMA_CALL_ARG_DELIM_NONE;
	s.io_open.format_type   = OFC_SEMA_CALL_ARG_FORMATTED;
	s.io_open.padding       = OFC_SEMA_CALL_ARG_PAD_YES;
	s.io_open.position_type = OFC_SEMA_CALL_ARG_ASIS;
	s.io_open.file_type     = OFC_SEMA_CALL_ARG_UNKNOWN;
	/* Default is processor dependant */
	s.io_open.action_type   = OFC_SEMA_CALL_ARG_COUNT;

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

		if (ofc_str_ref_empty(param->name))
		{
			if (i >= 1)
			{
				ofc_sema_scope_error(scope, param->src,
					"Un-named parameter %u has no meaning in OPEN.", i);
				return NULL;
			}

			if (i == 0)
			{
				ca_unit = param;
			}
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "UNIT"))
		{
			if (ca_unit)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of UNIT in OPEN.");
				return NULL;
			}

			ca_unit = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "ACCESS"))
		{
			if (ca_access)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of ACCESS in OPEN.");
				return NULL;
			}

			ca_access = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "ACTION"))
		{
			if (ca_action)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of ACTION in OPEN.");
				return NULL;
			}

			ca_action = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "BLANK"))
		{
			if (ca_blank)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of BLANK in OPEN.");
				return NULL;
			}

			ca_blank = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "DELIM"))
		{
			if (ca_delim)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of DELIM in OPEN.");
				return NULL;
			}

			ca_delim = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "ERR"))
		{
			if (ca_err)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of ERR in OPEN.");
				return NULL;
			}

			ca_err = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "FILE"))
		{
			if (ca_file)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of FILE in OPEN.");
				return NULL;
			}

			ca_file = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "FORM"))
		{
			if (ca_form)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of FORM in OPEN.");
				return NULL;
			}

			ca_form = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "IOSTAT"))
		{
			if (ca_iostat)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of IOSTAT in OPEN.");
				return NULL;
			}

			ca_iostat = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "PAD"))
		{
			if (ca_pad)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of PAD in OPEN.");
				return NULL;
			}

			ca_pad = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "POSITION"))
		{
			if (ca_position)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of POSITION in OPEN.");
				return NULL;
			}

			ca_position = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "RECL"))
		{
			if (ca_recl)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of RECL in OPEN.");
				return NULL;
			}

			ca_recl = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "STATUS"))
		{
			if (ca_status)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of STATUS in OPEN.");
				return NULL;
			}

			ca_status = param;
		}
		else
		{
			ofc_sema_scope_error(scope, param->src,
				"Unrecognized paramater %u name '%.*s' in OPEN.",
				i, param->name.size, param->name.base);
			return NULL;
		}
	}

	if (!ca_unit)
	{
		ofc_sema_scope_error(scope, stmt->src,
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
			ofc_sema_scope_error(scope, stmt->src,
				"UNIT must be of type INTEGER in OPEN");
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_expr_validate_uint(s.io_open.unit))
		{
			ofc_sema_scope_error(scope, stmt->src,
				   "UNIT must be a positive INTEGER in OPEN");
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}
	}
	else
	{
		ofc_sema_scope_error(scope, stmt->src,
			"UNIT must be an INTEGER expression in OPEN");
		return NULL;
	}

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
			ofc_sema_scope_error(scope, stmt->src,
				"ACCESS must be a CHARACTER expression in OPEN");
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}
		else
		{
			const ofc_sema_typeval_t* constant
				= ofc_sema_expr_constant(s.io_open.access);

			if (ofc_typeval_character_equal_strz_ci(constant, "DIRECT"))
			{
				s.io_open.access_type = OFC_SEMA_CALL_ARG_DIRECT;
				/* Change default format */
				s.io_open.format_type = OFC_SEMA_CALL_ARG_UNFORMATTED;

				if (!ca_recl)
				{
					ofc_sema_scope_error(scope, stmt->src,
						"Direct ACCESS must have a RECL specifier in OPEN");
					ofc_sema_stmt_io_open__cleanup(s);
					return NULL;
				}
			}
			else if (!ofc_typeval_character_equal_strz_ci(constant, "SEQUENTIAL"))
			{
				ofc_sema_scope_error(scope, stmt->src,
					"ACCESS must be 'SEQUENTIAL' or 'DIRECT' in OPEN");
				ofc_sema_stmt_io_open__cleanup(s);
				return NULL;
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
			ofc_sema_scope_error(scope, stmt->src,
				"ACTION must be a CHARACTER expression in OPEN");
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}
		else
		{
			const ofc_sema_typeval_t* constant
				= ofc_sema_expr_constant(s.io_open.action);

			if (ofc_typeval_character_equal_strz_ci(constant, "READWRITE"))
			{
				s.io_open.action_type = OFC_SEMA_CALL_ARG_READWRITE;
			}
			else if (ofc_typeval_character_equal_strz_ci(constant, "WRITE"))
			{
				s.io_open.action_type = OFC_SEMA_CALL_ARG_WRITE;
			}
			else if (ofc_typeval_character_equal_strz_ci(constant, "READ"))
			{
				s.io_open.action_type = OFC_SEMA_CALL_ARG_READ;
			}
			else
			{
				ofc_sema_scope_error(scope, stmt->src,
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
			ofc_sema_scope_error(scope, stmt->src,
				"FORM must be a CHARACTER expression in OPEN");
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}
		else
		{
			const ofc_sema_typeval_t* constant
				= ofc_sema_expr_constant(s.io_open.form);

			if (ofc_typeval_character_equal_strz_ci(constant, "FORMATTED"))
			{
				s.io_open.format_type = OFC_SEMA_CALL_ARG_FORMATTED;
			}
			else if (ofc_typeval_character_equal_strz_ci(constant, "UNFORMATTED"))
			{
				s.io_open.format_type = OFC_SEMA_CALL_ARG_UNFORMATTED;
			}
			else
			{
				ofc_sema_scope_error(scope, stmt->src,
					"FORM must be 'FORMATTED' or 'UNFORMATTED' in OPEN");
				ofc_sema_stmt_io_open__cleanup(s);
				return NULL;
			}
		}
	}

	if (ca_blank
		&& (s.io_open.format_type != OFC_SEMA_CALL_ARG_FORMATTED))
	{
		ofc_sema_scope_error(scope, stmt->src,
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
			ofc_sema_scope_error(scope, stmt->src,
				"BLANK must be a CHARACTER expression in OPEN");
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}
		else
		{
			const ofc_sema_typeval_t* constant
				= ofc_sema_expr_constant(s.io_open.blank);

			if (ofc_typeval_character_equal_strz_ci(constant, "ZERO"))
			{
				s.io_open.blank_type = OFC_SEMA_CALL_ARG_BLANK_ZERO;
			}
			else if (!ofc_typeval_character_equal_strz_ci(constant, "NULL"))
			{
				ofc_sema_scope_error(scope, stmt->src,
					"BLANK must be 'NULL' or 'ZERO' in OPEN");
				ofc_sema_stmt_io_open__cleanup(s);
				return NULL;
			}
		}
	}

	if (ca_delim
		&& (s.io_open.format_type != OFC_SEMA_CALL_ARG_FORMATTED))
	{
		ofc_sema_scope_error(scope, stmt->src,
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
			ofc_sema_scope_error(scope, stmt->src,
				"DELIM must be a CHARACTER expression in OPEN");
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}
		else
		{
			const ofc_sema_typeval_t* constant
				= ofc_sema_expr_constant(s.io_open.delim);

			if (ofc_typeval_character_equal_strz_ci(constant, "APOSTROPHE"))
			{
				s.io_open.delim_type = OFC_SEMA_CALL_ARG_DELIM_APOSTROPHE;
			}
			else if (ofc_typeval_character_equal_strz_ci(constant, "QUOTE"))
			{
				s.io_open.delim_type = OFC_SEMA_CALL_ARG_DELIM_QUOTE;
			}
			else if (!ofc_typeval_character_equal_strz_ci(constant, "NONE"))
			{
				ofc_sema_scope_error(scope, stmt->src,
					"DELIM must be 'NULL' or 'ZERO' in OPEN");
				ofc_sema_stmt_io_open__cleanup(s);
				return NULL;
			}
		}
	}

	if (ca_err)
	{
		s.io_open.err = ofc_sema_expr(
			scope, ca_err->expr);
		if (!s.io_open.err)
		{
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_io_check_label(
			scope, stmt, ca_err->name.base,
			ca_err->name.size, false,
			s.io_open.err, NULL))
		{
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}
	}

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
			ofc_sema_scope_error(scope, stmt->src,
				"STATUS must be a CHARACTER expression in OPEN");
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}
		else
		{
			const ofc_sema_typeval_t* constant
				= ofc_sema_expr_constant(s.io_open.status);

			if (ofc_typeval_character_equal_strz_ci(constant, "SCRATCH"))
			{
				s.io_open.file_type = OFC_SEMA_CALL_ARG_SCRATCH;
			}
			else if (ofc_typeval_character_equal_strz_ci(constant, "UNKNOWN"))
			{
				s.io_open.file_type = OFC_SEMA_CALL_ARG_UNKNOWN;
			}
			else if (ofc_typeval_character_equal_strz_ci(constant, "REPLACE"))
			{
				s.io_open.file_type = OFC_SEMA_CALL_ARG_REPLACE;
			}
			else if (ofc_typeval_character_equal_strz_ci(constant, "OLD"))
			{
				s.io_open.file_type = OFC_SEMA_CALL_ARG_OLD;
			}
			else if (ofc_typeval_character_equal_strz_ci(constant, "NEW"))
			{
				s.io_open.file_type = OFC_SEMA_CALL_ARG_NEW;
			}
			else
			{
				ofc_sema_scope_error(scope, stmt->src,
					"STATUS must be 'UNKNOWN', 'REPLACE', 'OLD', 'NEW' or 'SCRATCH' in OPEN");
				ofc_sema_stmt_io_open__cleanup(s);
				return NULL;
			}
		}
	}

	if (ca_file && (s.io_open.file_type == OFC_SEMA_CALL_ARG_SCRATCH))
	{
		ofc_sema_scope_error(scope, stmt->src,
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
			ofc_sema_scope_error(scope, stmt->src,
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
			ofc_sema_scope_error(scope, stmt->src,
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
			ofc_sema_scope_error(scope, stmt->src,
				"IOSTAT must be of type INTEGER in OPEN");
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}
	}

	if (ca_pad && (s.io_open.format_type != OFC_SEMA_CALL_ARG_FORMATTED))
	{
		ofc_sema_scope_error(scope, stmt->src,
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
			ofc_sema_scope_error(scope, stmt->src,
				"PAD must be a CHARACTER expression in OPEN");
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}
		else
		{
			const ofc_sema_typeval_t* constant
				= ofc_sema_expr_constant(s.io_open.pad);

			if (ofc_typeval_character_equal_strz_ci(constant, "NO"))
			{
				s.io_open.padding = OFC_SEMA_CALL_ARG_PAD_NO;
			}
			else if (!ofc_typeval_character_equal_strz_ci(constant, "YES"))
			{
				ofc_sema_scope_error(scope, stmt->src,
					"PAD must be 'YES' or 'NO' in OPEN");
				ofc_sema_stmt_io_open__cleanup(s);
				return NULL;
			}
		}
	}

	if (ca_position && (s.io_open.access_type != OFC_SEMA_CALL_ARG_SEQUENTIAL))
	{
		ofc_sema_scope_error(scope, stmt->src,
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
			ofc_sema_scope_error(scope, stmt->src,
				"POSITION must be a CHARACTER expression in OPEN");
			ofc_sema_stmt_io_open__cleanup(s);
			return NULL;
		}
		else
		{
			const ofc_sema_typeval_t* constant
				= ofc_sema_expr_constant(s.io_open.position);

			if (ofc_typeval_character_equal_strz_ci(constant, "REWIND"))
			{
				s.io_open.position_type = OFC_SEMA_CALL_ARG_REWIND;
			}
			else if (ofc_typeval_character_equal_strz_ci(constant, "APPEND"))
			{
				s.io_open.position_type = OFC_SEMA_CALL_ARG_APPEND;
			}
			else if (!ofc_typeval_character_equal_strz_ci(constant, "ASIS"))
			{
				ofc_sema_scope_error(scope, stmt->src,
					"POSITION must be 'REWIND', 'APPEND' or 'ASIS' in OPEN");
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
			ofc_sema_scope_error(scope, stmt->src,
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
					ofc_sema_scope_error(scope, stmt->src,
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
