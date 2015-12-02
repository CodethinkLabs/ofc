#include <ofc/sema.h>

void ofc_sema_stmt_io_inquire__cleanup(
	ofc_sema_stmt_t s)
{
	ofc_sema_expr_delete(s.io_inquire.unit);
	ofc_sema_expr_delete(s.io_inquire.file);
	ofc_sema_expr_delete(s.io_inquire.err);

	ofc_sema_lhs_delete(s.io_inquire.access);
	ofc_sema_lhs_delete(s.io_inquire.action);
	ofc_sema_lhs_delete(s.io_inquire.blank);
	ofc_sema_lhs_delete(s.io_inquire.delim);
	ofc_sema_lhs_delete(s.io_inquire.direct);
	ofc_sema_lhs_delete(s.io_inquire.exist);
	ofc_sema_lhs_delete(s.io_inquire.form);
	ofc_sema_lhs_delete(s.io_inquire.formatted);
	ofc_sema_lhs_delete(s.io_inquire.iostat);
	ofc_sema_lhs_delete(s.io_inquire.name);
	ofc_sema_lhs_delete(s.io_inquire.named);
	ofc_sema_lhs_delete(s.io_inquire.nextrec);
	ofc_sema_lhs_delete(s.io_inquire.number);
	ofc_sema_lhs_delete(s.io_inquire.opened);
	ofc_sema_lhs_delete(s.io_inquire.pad);
	ofc_sema_lhs_delete(s.io_inquire.position);
	ofc_sema_lhs_delete(s.io_inquire.read);
	ofc_sema_lhs_delete(s.io_inquire.readwrite);
	ofc_sema_lhs_delete(s.io_inquire.recl);
	ofc_sema_lhs_delete(s.io_inquire.sequential);
	ofc_sema_lhs_delete(s.io_inquire.unformatted);
	ofc_sema_lhs_delete(s.io_inquire.write);
}

ofc_sema_stmt_t* ofc_sema_stmt_io_inquire(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_IO_INQUIRE)
		|| !stmt->io.params)
		return NULL;

	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_IO_INQUIRE;
	s.io_inquire.unit          = NULL;
	s.io_inquire.access        = NULL;
	s.io_inquire.action        = NULL;
	s.io_inquire.blank         = NULL;
	s.io_inquire.delim         = NULL;
	s.io_inquire.direct        = NULL;
	s.io_inquire.err           = NULL;
	s.io_inquire.exist         = NULL;
	s.io_inquire.file          = NULL;
	s.io_inquire.form          = NULL;
	s.io_inquire.formatted     = NULL;
	s.io_inquire.iostat        = NULL;
	s.io_inquire.name          = NULL;
	s.io_inquire.named         = NULL;
	s.io_inquire.nextrec       = NULL;
	s.io_inquire.number        = NULL;
	s.io_inquire.opened        = NULL;
	s.io_inquire.pad           = NULL;
	s.io_inquire.position      = NULL;
	s.io_inquire.read          = NULL;
	s.io_inquire.readwrite     = NULL;
	s.io_inquire.recl          = NULL;
	s.io_inquire.sequential    = NULL;
	s.io_inquire.unformatted   = NULL;
	s.io_inquire.write         = NULL;

	ofc_parse_call_arg_t* ca_unit        = NULL;
	ofc_parse_call_arg_t* ca_access      = NULL;
	ofc_parse_call_arg_t* ca_action      = NULL;
	ofc_parse_call_arg_t* ca_blank       = NULL;
	ofc_parse_call_arg_t* ca_delim       = NULL;
	ofc_parse_call_arg_t* ca_direct      = NULL;
	ofc_parse_call_arg_t* ca_err         = NULL;
	ofc_parse_call_arg_t* ca_exist       = NULL;
	ofc_parse_call_arg_t* ca_file        = NULL;
	ofc_parse_call_arg_t* ca_form        = NULL;
	ofc_parse_call_arg_t* ca_formatted   = NULL;
	ofc_parse_call_arg_t* ca_iostat      = NULL;
	ofc_parse_call_arg_t* ca_name        = NULL;
	ofc_parse_call_arg_t* ca_named       = NULL;
	ofc_parse_call_arg_t* ca_nextrec     = NULL;
	ofc_parse_call_arg_t* ca_number      = NULL;
	ofc_parse_call_arg_t* ca_opened      = NULL;
	ofc_parse_call_arg_t* ca_pad         = NULL;
	ofc_parse_call_arg_t* ca_position    = NULL;
	ofc_parse_call_arg_t* ca_read        = NULL;
	ofc_parse_call_arg_t* ca_readwrite   = NULL;
	ofc_parse_call_arg_t* ca_recl        = NULL;
	ofc_parse_call_arg_t* ca_sequential  = NULL;
	ofc_parse_call_arg_t* ca_unformatted = NULL;
	ofc_parse_call_arg_t* ca_write       = NULL;

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
					"Un-named parameter %u has no meaning in INQUIRE.", i);
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
					"Re-definition of UNIT in INQUIRE.");
				return NULL;
			}

			ca_unit = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "ACCESS"))
		{
			if (ca_access)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of ACCESS in INQUIRE.");
				return NULL;
			}

			ca_access = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "ACTION"))
		{
			if (ca_action)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of ACTION in INQUIRE.");
				return NULL;
			}

			ca_action = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "BLANK"))
		{
			if (ca_blank)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of BLANK in INQUIRE.");
				return NULL;
			}

			ca_blank = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "DELIM"))
		{
			if (ca_delim)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of DELIM in INQUIRE.");
				return NULL;
			}

			ca_delim = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "DIRECT"))
		{
			if (ca_direct)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of DIRECT in INQUIRE.");
				return NULL;
			}

			ca_direct = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "ERR"))
		{
			if (ca_err)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of ERR in INQUIRE.");
				return NULL;
			}

			ca_err = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "EXIST"))
		{
			if (ca_exist)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of EXIST in INQUIRE.");
				return NULL;
			}

			ca_exist = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "FILE"))
		{
			if (ca_file)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of FILE in INQUIRE.");
				return NULL;
			}

			ca_file = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "FORM"))
		{
			if (ca_form)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of FORM in INQUIRE.");
				return NULL;
			}

			ca_form = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "FORMATTED"))
		{
			if (ca_formatted)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of FORMATTED in INQUIRE.");
				return NULL;
			}

			ca_formatted = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "IOSTAT"))
		{
			if (ca_iostat)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of IOSTAT in INQUIRE.");
				return NULL;
			}

			ca_iostat = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "NAME"))
		{
			if (ca_name)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of NAME in INQUIRE.");
				return NULL;
			}

			ca_name = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "NAMED"))
		{
			if (ca_named)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of NAMED in INQUIRE.");
				return NULL;
			}

			ca_named = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "NEXTREC"))
		{
			if (ca_nextrec)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of NEXTREC in INQUIRE.");
				return NULL;
			}

			ca_nextrec = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "NUMBER"))
		{
			if (ca_number)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of NUMBER in INQUIRE.");
				return NULL;
			}

			ca_number = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "OPENED"))
		{
			if (ca_opened)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of OPENED in INQUIRE.");
				return NULL;
			}

			ca_opened = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "PAD"))
		{
			if (ca_pad)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of PAD in INQUIRE.");
				return NULL;
			}

			ca_pad = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "POSITION"))
		{
			if (ca_position)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of POSITION in INQUIRE.");
				return NULL;
			}

			ca_position = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "READ"))
		{
			if (ca_read)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of READ in INQUIRE.");
				return NULL;
			}

			ca_read = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "READWRITE"))
		{
			if (ca_readwrite)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of READWRITE in INQUIRE.");
				return NULL;
			}

			ca_readwrite = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "RECL"))
		{
			if (ca_recl)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of RECL in INQUIRE.");
				return NULL;
			}

			ca_recl = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "SEQUENTIAL"))
		{
			if (ca_sequential)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of SEQUENTIAL in INQUIRE.");
				return NULL;
			}

			ca_sequential = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "UNFORMATTED"))
		{
			if (ca_unformatted)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of UNFORMATTED in INQUIRE.");
				return NULL;
			}

			ca_unformatted = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "WRITE"))
		{
			if (ca_write)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of WRITE in INQUIRE.");
				return NULL;
			}

			ca_write = param;
		}
		else
		{
			ofc_sema_scope_error(scope, param->src,
				"Unrecognized paramater %u name '%.*s' in INQUIRE.",
				i, param->name.size, param->name.base);
			return NULL;
		}
	}

	if (!ca_unit && !ca_file)
	{
		ofc_sema_scope_error(scope, stmt->src,
			"No UNIT or FILE defined in INQUIRE.");
		return NULL;
	}
	else if (ca_unit && ca_file)
	{
		ofc_sema_scope_error(scope, stmt->src,
			"UNIT and FILE can't be specified at the same time in INQUIRE.");
		return NULL;
	}

	if (ca_unit && (ca_unit->type == OFC_PARSE_CALL_ARG_EXPR))
	{
		s.io_inquire.unit = ofc_sema_expr(
			scope, ca_unit->expr);
		if (!s.io_inquire.unit) return NULL;

		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_inquire.unit);
		if (!etype)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_type_is_integer(etype))
		{
			ofc_sema_scope_error(scope, stmt->src,
				"UNIT must be of type INTEGER in INQUIRE");
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_expr_validate_uint(s.io_inquire.unit))
		{
			ofc_sema_scope_error(scope, stmt->src,
				   "UNIT must be a positive INTEGER in INQUIRE");
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}
	}
	else if (ca_unit)
	{
		ofc_sema_scope_error(scope, stmt->src,
			"UNIT must be an INTEGER expression in INQUIRE");
		return NULL;
	}

	if (ca_access)
	{
		s.io_inquire.access = ofc_sema_lhs_from_expr(
			scope, ca_access->expr);
		if (!s.io_inquire.access)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_lhs_type(s.io_inquire.access);
		if (!etype)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_type_is_character(etype))
		{
			ofc_sema_scope_error(scope, stmt->src,
				"ACCESS must be a CHARACTER variable in INQUIRE");
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}
	}

	if (ca_action)
	{
		s.io_inquire.action = ofc_sema_lhs_from_expr(
			scope, ca_action->expr);
		if (!s.io_inquire.action)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_lhs_type(s.io_inquire.action);
		if (!etype)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_type_is_character(etype))
		{
			ofc_sema_scope_error(scope, stmt->src,
				"ACTION must be a CHARACTER variable in INQUIRE");
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}
	}

	if (ca_blank)
	{
		s.io_inquire.blank = ofc_sema_lhs_from_expr(
			scope, ca_blank->expr);
		if (!s.io_inquire.blank)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_lhs_type(s.io_inquire.blank);
		if (!etype)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_type_is_character(etype))
		{
			ofc_sema_scope_error(scope, stmt->src,
				"BLANK must be a CHARACTER variable in INQUIRE");
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}
	}

	if (ca_delim)
	{
		s.io_inquire.delim = ofc_sema_lhs_from_expr(
			scope, ca_delim->expr);
		if (!s.io_inquire.delim)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_lhs_type(s.io_inquire.delim);
		if (!etype)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_type_is_character(etype))
		{
			ofc_sema_scope_error(scope, stmt->src,
				"DELIM must be a CHARACTER variable in INQUIRE");
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}
	}

	if (ca_direct)
	{
		s.io_inquire.direct = ofc_sema_lhs_from_expr(
			scope, ca_direct->expr);
		if (!s.io_inquire.direct)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_lhs_type(s.io_inquire.direct);
		if (!etype)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_type_is_character(etype))
		{
			ofc_sema_scope_error(scope, stmt->src,
				"DIRECT must be a CHARACTER variable in INQUIRE");
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}
	}

	if (ca_err)
	{
		s.io_inquire.err = ofc_sema_expr(
			scope, ca_err->expr);
		if (!s.io_inquire.err)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_io_check_label(
			scope, stmt, ca_err->name.base,
			ca_err->name.size, s.io_inquire.err, NULL))
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}
	}

	if (ca_exist)
	{
		s.io_inquire.exist = ofc_sema_lhs_from_expr(
			scope, ca_exist->expr);
		if (!s.io_inquire.exist)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_lhs_type(s.io_inquire.exist);
		if (!etype)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}
		if (!ofc_sema_type_is_logical(etype))
		{
			ofc_sema_scope_error(scope, stmt->src,
				"EXIST must be a LOGICAL variable in INQUIRE");
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}
	}

	else if (ca_file)
	{
		s.io_inquire.file = ofc_sema_expr(
			scope, ca_file->expr);
		if (!s.io_inquire.file)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_inquire.file);
		if (!etype)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_type_is_character(etype))
		{
			ofc_sema_scope_error(scope, stmt->src,
				"FILE must be a CHARACTER expression in INQUIRE");
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}
	}

	if (ca_form)
	{
		s.io_inquire.form = ofc_sema_lhs_from_expr(
			scope, ca_form->expr);
		if (!s.io_inquire.form)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_lhs_type(s.io_inquire.form);
		if (!etype)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_type_is_character(etype))
		{
			ofc_sema_scope_error(scope, stmt->src,
				"FORM must be a CHARACTER variable in INQUIRE");
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}
	}

	if (ca_formatted)
	{
		s.io_inquire.formatted = ofc_sema_lhs_from_expr(
			scope, ca_formatted->expr);
		if (!s.io_inquire.formatted)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_lhs_type(s.io_inquire.formatted);
		if (!etype)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_type_is_character(etype))
		{
			ofc_sema_scope_error(scope, stmt->src,
				"FORMATTED must be a CHARACTER variable in INQUIRE");
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}
	}

	if (ca_iostat)
	{
		s.io_inquire.iostat = ofc_sema_lhs_from_expr(
			scope, ca_iostat->expr);
		if (!s.io_inquire.iostat)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_lhs_type(s.io_inquire.iostat);
		if (!etype)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_type_is_integer(etype))
		{
			ofc_sema_scope_error(scope, stmt->src,
				"IOSTAT must be of type INTEGER in INQUIRE");
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}
	}

	if (ca_name)
	{
		s.io_inquire.name = ofc_sema_lhs_from_expr(
			scope, ca_name->expr);
		if (!s.io_inquire.name)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_lhs_type(s.io_inquire.name);
		if (!etype)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_type_is_character(etype))
		{
			ofc_sema_scope_error(scope, stmt->src,
				"NAME must be a CHARACTER variable in INQUIRE");
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}
	}

	if (ca_named)
	{
		s.io_inquire.named = ofc_sema_lhs_from_expr(
			scope, ca_named->expr);
		if (!s.io_inquire.named)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_lhs_type(s.io_inquire.named);
		if (!etype)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_type_is_logical(etype))
		{
			ofc_sema_scope_error(scope, stmt->src,
				"NAMED must be a LOGICAL variable in INQUIRE");
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}
	}

	if (ca_nextrec)
	{
		s.io_inquire.nextrec = ofc_sema_lhs_from_expr(
			scope, ca_nextrec->expr);
		if (!s.io_inquire.nextrec)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_lhs_type(s.io_inquire.nextrec);
		if (!etype)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_type_is_integer(etype))
		{
			ofc_sema_scope_error(scope, stmt->src,
				"NEXTREC must be of type INTEGER in INQUIRE");
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}
	}

	if (ca_number)
	{
		s.io_inquire.number = ofc_sema_lhs_from_expr(
			scope, ca_number->expr);
		if (!s.io_inquire.number)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_lhs_type(s.io_inquire.number);
		if (!etype)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_type_is_integer(etype))
		{
			ofc_sema_scope_error(scope, stmt->src,
				"NUMBER must be an INTEGER variable in INQUIRE");
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}
	}

	if (ca_opened)
	{
		s.io_inquire.opened = ofc_sema_lhs_from_expr(
			scope, ca_opened->expr);
		if (!s.io_inquire.opened)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_lhs_type(s.io_inquire.opened);
		if (!etype)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_type_is_logical(etype))
		{
			ofc_sema_scope_error(scope, stmt->src,
				"OPENED must be a LOGICAL variable in INQUIRE");
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}
	}

	if (ca_pad)
	{
		s.io_inquire.pad = ofc_sema_lhs_from_expr(
			scope, ca_pad->expr);
		if (!s.io_inquire.pad)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_lhs_type(s.io_inquire.pad);
		if (!etype)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_type_is_character(etype))
		{
			ofc_sema_scope_error(scope, stmt->src,
				"PAD must be a CHARACTER variable in INQUIRE");
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}
	}

	if (ca_position)
	{
		s.io_inquire.position = ofc_sema_lhs_from_expr(
			scope, ca_position->expr);
		if (!s.io_inquire.position)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_lhs_type(s.io_inquire.position);
		if (!etype)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_type_is_character(etype))
		{
			ofc_sema_scope_error(scope, stmt->src,
				"POSITION must be a CHARACTER variable in INQUIRE");
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}
	}

	if (ca_read)
	{
		s.io_inquire.read = ofc_sema_lhs_from_expr(
			scope, ca_read->expr);
		if (!s.io_inquire.read)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_lhs_type(s.io_inquire.read);
		if (!etype)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_type_is_character(etype))
		{
			ofc_sema_scope_error(scope, stmt->src,
				"READ must be a CHARACTER variable in INQUIRE");
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}
	}

	if (ca_readwrite)
	{
		s.io_inquire.readwrite = ofc_sema_lhs_from_expr(
			scope, ca_readwrite->expr);
		if (!s.io_inquire.readwrite)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_lhs_type(s.io_inquire.readwrite);
		if (!etype)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_type_is_character(etype))
		{
			ofc_sema_scope_error(scope, stmt->src,
				"READWRITE must be a CHARACTER variable in INQUIRE");
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}
	}

	if (ca_recl)
	{
		s.io_inquire.recl = ofc_sema_lhs_from_expr(
			scope, ca_recl->expr);
		if (!s.io_inquire.recl)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_lhs_type(s.io_inquire.recl);
		if (!etype)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_type_is_integer(etype))
		{
			ofc_sema_scope_error(scope, stmt->src,
				"RECL must be an INTEGER variable in INQUIRE");
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}
	}

	if (ca_sequential)
	{
		s.io_inquire.sequential = ofc_sema_lhs_from_expr(
			scope, ca_sequential->expr);
		if (!s.io_inquire.sequential)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_lhs_type(s.io_inquire.sequential);
		if (!etype)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_type_is_character(etype))
		{
			ofc_sema_scope_error(scope, stmt->src,
				"SEQUENTIAL must be a CHARACTER variable in INQUIRE");
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}
	}

	if (ca_unformatted)
	{
		s.io_inquire.unformatted = ofc_sema_lhs_from_expr(
			scope, ca_unformatted->expr);
		if (!s.io_inquire.unformatted)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_lhs_type(s.io_inquire.unformatted);
		if (!etype)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_type_is_character(etype))
		{
			ofc_sema_scope_error(scope, stmt->src,
				"UNFORMATTED must be a CHARACTER variable in INQUIRE");
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}
	}

	if (ca_write)
	{
		s.io_inquire.write = ofc_sema_lhs_from_expr(
			scope, ca_write->expr);
		if (!s.io_inquire.write)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_lhs_type(s.io_inquire.write);
		if (!etype)
		{
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_type_is_character(etype))
		{
			ofc_sema_scope_error(scope, stmt->src,
				"WRITE must be a CHARACTER variable in INQUIRE");
			ofc_sema_stmt_io_inquire__cleanup(s);
			return NULL;
		}
	}

	ofc_sema_stmt_t* as
		= ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_stmt_io_inquire__cleanup(s);
		return NULL;
	}
	return as;
}
