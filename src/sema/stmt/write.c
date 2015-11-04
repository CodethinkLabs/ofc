#include <ofc/sema.h>
#include <math.h>


ofc_sema_stmt_t* ofc_sema_stmt_io_write(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_IO_WRITE)
		|| !stmt->io.params)
		return NULL;

	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_WRITE;
	s.io_write.unit        = NULL;
	s.io_write.stdout      = false;
	s.io_write.format_expr = NULL;
	s.io_write.format      = NULL;
	s.io_write.format_ldio = false;
	s.io_write.iostat      = NULL;
	s.io_write.rec         = NULL;
	s.io_write.err         = NULL;
	s.io_write.iolist      = NULL;

	ofc_parse_call_arg_t* ca_unit   = NULL;
	ofc_parse_call_arg_t* ca_format = NULL;
	ofc_parse_call_arg_t* ca_iostat = NULL;
	ofc_parse_call_arg_t* ca_rec    = NULL;
	ofc_parse_call_arg_t* ca_err    = NULL;

	unsigned i;
	for (i = 0; i < stmt->io.params->count; i++)
	{
		ofc_parse_call_arg_t* param
			= stmt->io.params->call_arg[i];
		if (!param) continue;

		if (ofc_str_ref_empty(param->name))
		{
			if (i >= 2)
			{
				ofc_sema_scope_error(scope, param->src,
					"Un-named parameter %u has no meaning in WRITE.", i);
				return NULL;
			}

			if (i == 0)
			{
				ca_unit = param;
			}
			else
			{
				if (!ca_unit)
				{
					ofc_sema_scope_error(scope, param->src,
						"Un-named format parameter only valid after UNIT in WRITE.");
					return NULL;
				}

				ca_format = param;
			}
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "UNIT"))
		{
			if (ca_unit)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of UNIT in WRITE.");
				return NULL;
			}

			ca_unit = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "FMT"))
		{
			if (ca_format)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of FMT in WRITE.");
				return NULL;
			}

			ca_format = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "IOSTAT"))
		{
			if (ca_iostat)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of IOSTAT in WRITE.");
				return NULL;
			}

			ca_iostat = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "REC"))
		{
			if (ca_rec)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of REC in WRITE.");
				return NULL;
			}

			ca_rec = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "ERR"))
		{
			if (ca_err)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of ERR in WRITE.");
				return NULL;
			}

			ca_err = param;
		}
		else
		{
			ofc_sema_scope_error(scope, param->src,
				"Unrecognized paramater %u name '%.*s' in WRITE.",
				i, param->name.size, param->name.base);
			return NULL;
		}
	}

	if (!ca_unit)
	{
		ofc_sema_scope_error(scope, stmt->src,
			"No UNIT defined in WRITE.");
		return NULL;
	}

	if (ca_unit->type == OFC_PARSE_CALL_ARG_ASTERISK)
	{
		s.io_write.stdout = true;
	}
	else if (ca_unit->type == OFC_PARSE_CALL_ARG_EXPR)
	{
		s.io_write.unit = ofc_sema_expr(
			scope, ca_unit->expr);
		if (!s.io_write.unit) return NULL;

		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_write.unit);
		if (!etype) return NULL;

		if (!ofc_sema_type_is_integer(etype))
		{
			ofc_sema_scope_error(scope, stmt->src,
				"UNIT must be of type INTEGER in WRITE");
			ofc_sema_expr_delete(s.io_write.unit);
			return NULL;
		}

		const ofc_sema_typeval_t* evalue
			= ofc_sema_expr_constant(s.io_write.unit);
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
						"UNIT must be a positive INTEGER in WRITE");
					ofc_sema_expr_delete(s.io_write.unit);
					return NULL;
				}

				/* TODO - Clamp to range? */
			}
        }
	}
	else
	{
		ofc_sema_scope_error(scope, stmt->src,
			"UNIT must be an INTEGER expression or asterisk in WRITE");
		return NULL;
	}

	if (!ca_format)
	{
		ofc_sema_scope_error(scope, stmt->src,
			"No format (FMT) defined in WRITE.");
		ofc_sema_expr_delete(s.io_write.unit);
		return NULL;
	}

	if (ca_format->type == OFC_PARSE_CALL_ARG_ASTERISK)
	{
		s.io_write.format_ldio = true;
	}
	else if (ca_format->type == OFC_PARSE_CALL_ARG_EXPR)
	{
		s.io_write.format_expr = ofc_sema_expr(
			scope, ca_format->expr);
		if (!s.io_write.format_expr) return NULL;

		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_write.format_expr);
		if (!etype) return NULL;

		if (ofc_sema_type_is_integer(etype))
		{
			const ofc_sema_typeval_t* format_label
				= ofc_sema_expr_constant(s.io_write.format_expr);
			if (format_label)
			{
				int64_t fl64 = 0;
				if (!ofc_sema_typeval_get_integer(
					format_label, &fl64) || (fl64 < 0))
				{
					ofc_sema_scope_error(scope, stmt->src,
						"Format (FMT) label expression must be a positive INTEGER in WRITE");
					ofc_sema_expr_delete(s.io_write.format_expr);
					ofc_sema_expr_delete(s.io_write.unit);
					return NULL;
				}

				unsigned ulabel = (unsigned) fl64;

				if (((int64_t) ulabel) != fl64)
				{
					ofc_sema_expr_delete(s.io_write.format_expr);
					ofc_sema_expr_delete(s.io_write.unit);
					return NULL;
				}

				const ofc_sema_label_t* label
					= ofc_sema_label_map_find(scope->label, ulabel);
				if (!label)
				{
					ofc_sema_scope_error(scope, stmt->src,
						"Format label expression not defined in WRITE");
					ofc_sema_expr_delete(s.io_write.format_expr);
					ofc_sema_expr_delete(s.io_write.unit);
					return NULL;
				}

				if (label->type != OFC_SEMA_LABEL_FORMAT)
				{
					ofc_sema_scope_error(scope, stmt->src,
						"Label expression must be a FORMAT statement in WRITE");
					ofc_sema_expr_delete(s.io_write.format_expr);
					ofc_sema_expr_delete(s.io_write.unit);
					return NULL;
				}
				s.io_write.format = label->format;
			}


		}
		else if (etype->type == OFC_SEMA_TYPE_CHARACTER)
		{
            /* TODO - Check we can resolve this as a format descriptor. */
		}
		else
		{
			/* TODO - Support INTEGER array formats. */

			ofc_sema_scope_error(scope, stmt->src,
				"Format (FMT) must be a label or character string in WRITE");
			ofc_sema_expr_delete(s.io_write.format_expr);
			ofc_sema_expr_delete(s.io_write.unit);
			return NULL;
		}
	}
	else
	{
		ofc_sema_scope_error(scope, stmt->src,
			"Format (FMT) must be an INTEGER expression or asterisk in WRITE");
		ofc_sema_expr_delete(s.io_write.unit);
		return NULL;
	}

	if (ca_iostat)
	{
		s.io_write.iostat = ofc_sema_expr(
			scope, ca_iostat->expr);
		if (!s.io_write.iostat)
		{
			ofc_sema_expr_delete(s.io_write.unit);
			ofc_sema_expr_delete(s.io_write.format_expr);

			return NULL;
		}

		if (s.io_write.iostat->type != OFC_SEMA_EXPR_LHS)
		{
			ofc_sema_scope_error(scope, stmt->src,
				"IOSTAT must be of a variable in REWIND");
			ofc_sema_expr_delete(s.io_write.unit);
			ofc_sema_expr_delete(s.io_write.format_expr);

			ofc_sema_expr_delete(s.io_write.iostat);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_write.iostat);
		if (!etype)
		{
			ofc_sema_expr_delete(s.io_write.unit);
			ofc_sema_expr_delete(s.io_write.format_expr);

			ofc_sema_expr_delete(s.io_write.iostat);
			return NULL;
		}

		if (!ofc_sema_type_is_integer(etype))
		{
			ofc_sema_scope_error(scope, stmt->src,
				"IOSTAT must be of type INTEGER in REWIND");
			ofc_sema_expr_delete(s.io_write.unit);
			ofc_sema_expr_delete(s.io_write.format_expr);

			ofc_sema_expr_delete(s.io_write.iostat);
			return NULL;
		}
	}

	if (ca_rec)
	{
		/* TODO - Validate REC as positive INTEGER. */
	}

	if (ca_err)
	{
		s.io_write.err = ofc_sema_expr(
			scope, ca_err->expr);
		if (!s.io_write.err)
		{
			ofc_sema_expr_delete(s.io_write.unit);
			ofc_sema_expr_delete(s.io_write.format_expr);

			ofc_sema_expr_delete(s.io_write.iostat);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_write.err);
		if (!etype)
		{
			ofc_sema_expr_delete(s.io_write.unit);
			ofc_sema_expr_delete(s.io_write.format_expr);

			ofc_sema_expr_delete(s.io_write.iostat);
			ofc_sema_expr_delete(s.io_write.err);
			return NULL;
		}

		if (ofc_sema_type_is_integer(etype))
		{
			const ofc_sema_typeval_t* err_label
				= ofc_sema_expr_constant(s.io_write.err);
			if (err_label)
			{
				int64_t fl64 = 0;
				if (!ofc_sema_typeval_get_integer(
					err_label, &fl64) || (fl64 < 0))
				{
					ofc_sema_scope_error(scope, stmt->src,
						"Error (ERR) label expression must be a positive INTEGER in REWIND");
					ofc_sema_expr_delete(s.io_write.unit);
					ofc_sema_expr_delete(s.io_write.format_expr);

					ofc_sema_expr_delete(s.io_write.iostat);
					ofc_sema_expr_delete(s.io_write.err);
					return NULL;
				}

				unsigned ulabel = (unsigned) fl64;

				if (((int64_t) ulabel) != fl64)
				{
					ofc_sema_expr_delete(s.io_write.unit);
					ofc_sema_expr_delete(s.io_write.format_expr);

					ofc_sema_expr_delete(s.io_write.iostat);
					ofc_sema_expr_delete(s.io_write.err);
					return NULL;
				}

				const ofc_sema_label_t* label
					= ofc_sema_label_map_find(scope->label, ulabel);
				if (!label)
				{
					ofc_sema_scope_error(scope, stmt->src,
						"Error label expression not defined in REWIND");
					ofc_sema_expr_delete(s.io_write.unit);
					ofc_sema_expr_delete(s.io_write.format_expr);

					ofc_sema_expr_delete(s.io_write.iostat);
					ofc_sema_expr_delete(s.io_write.err);
					return NULL;
				}
			}
		}
		else
		{
			ofc_sema_scope_error(scope, stmt->src,
				"Error (ERR) must be a label in REWIND");
			ofc_sema_expr_delete(s.io_write.unit);
			ofc_sema_expr_delete(s.io_write.format_expr);

			ofc_sema_expr_delete(s.io_write.iostat);
			return NULL;
		}
	}

	if (s.io_write.format)
	{
		/* Check iolist */
		if (stmt->io.iolist)
		{
			s.io_write.iolist
				= ofc_sema_iolist(
					scope, stmt->io.iolist);
			if (!s.io_write.iolist)
			{
				ofc_sema_expr_delete(s.io_write.unit);
				ofc_sema_expr_delete(s.io_write.format_expr);

				ofc_sema_expr_delete(s.io_write.iostat);
				ofc_sema_expr_delete(s.io_write.err);
				return NULL;
			}
		}

		/* Count elements in iolist */
		unsigned iolist_len
			= ofc_sema_iolist_count(
				s.io_write.iolist);


		unsigned data_desc_count
			= ofc_sema_io_data_format_count(s.io_write.format);

		if ((data_desc_count > 0) && (iolist_len > 0))
		{
			if (iolist_len < data_desc_count)
			{
				ofc_sema_scope_warning(scope, stmt->src,
					"IO list shorter than FORMAT list,"
					" last FORMAT data descriptors will be ignored");
			}
			else if (fmod(iolist_len, data_desc_count) != 0)
			{
				ofc_sema_scope_warning(scope, stmt->src,
					"IO list length is not a multiple of FORMAT list length");
			}

			/* Create a format list of same length as iolist
			 * with only data edit descriptors  */
			ofc_parse_format_desc_list_t* format_list
				= ofc_sema_io_data_format(s.io_write.format, iolist_len);
			if (!format_list)
			{
				ofc_sema_expr_delete(s.io_write.unit);
				ofc_sema_expr_delete(s.io_write.format_expr);
				ofc_sema_expr_delete(s.io_write.iostat);
				ofc_sema_expr_delete(s.io_write.err);
				ofc_sema_expr_list_delete(s.io_write.iolist);
				return NULL;
			}

			/* Compare iolist with format */
			bool fail = !ofc_sema_io_format_iolist_compare(
				scope, stmt, format_list, s.io_write.iolist);
			ofc_parse_format_desc_list_delete(format_list);
			if (fail)
			{
				ofc_sema_expr_delete(s.io_write.unit);
				ofc_sema_expr_delete(s.io_write.format_expr);
				ofc_sema_expr_delete(s.io_write.iostat);
				ofc_sema_expr_delete(s.io_write.err);
				ofc_sema_expr_list_delete(s.io_write.iolist);
				return NULL;
			}
		}
		else if (iolist_len > 0)
		{
			ofc_sema_scope_warning(scope, stmt->src,
				"No data edit descriptors in FORMAT list");
		}
		else if (data_desc_count > 0)
		{
			ofc_sema_scope_warning(scope, stmt->src,
				"No IO list in PRINT statement");
		}

	}

	ofc_sema_stmt_t* as
		= ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_delete(s.io_write.unit);
		ofc_sema_expr_delete(s.io_write.format_expr);
		ofc_sema_expr_delete(s.io_write.iostat);
		ofc_sema_expr_delete(s.io_write.err);
		ofc_sema_expr_list_delete(s.io_write.iolist);
		return NULL;
	}
	return as;
}

static bool ofc_sema_stmt_write__print_optional(
	ofc_colstr_t* cs, const ofc_sema_expr_t* expr)
{
	if (!cs || !expr) return false;

	if (!ofc_colstr_atomic_writef(cs, ","))
		return false;
	if (!ofc_sema_expr_print(cs, expr))
		return false;
	return true;
}

bool ofc_sema_stmt_write_print(ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt)
{
	if (!cs || (stmt->type != OFC_SEMA_STMT_WRITE))
		return false;

	if (!ofc_colstr_atomic_writef(cs, "WRITE"))
		return false;
	if (!ofc_colstr_atomic_writef(cs, "("))
		return false;

	if (stmt->io_write.stdout)
	{
		if (!ofc_colstr_atomic_writef(cs, "*"))
			return false;
	}
	else
	{
		if (!ofc_sema_expr_print(cs,
			stmt->io_write.unit))
				return false;
	}

	if (stmt->io_write.format_ldio)
	{
		if (!ofc_colstr_atomic_writef(cs, ","))
			return false;
		if (!ofc_colstr_atomic_writef(cs, "*"))
			return false;
	}
	else
	{
		if (!ofc_sema_stmt_write__print_optional(
			cs,	stmt->io_write.format_expr))
				return false;
	}

	if (stmt->io_write.iostat)
	{
		if (!ofc_sema_stmt_write__print_optional(
			cs, stmt->io_write.iostat))
				return false;
	}
	if (stmt->io_write.rec)
	{
		if (!ofc_sema_stmt_write__print_optional(
			cs,	stmt->io_write.rec))
				return false;
	}
	if (stmt->io_write.err)
	{
		if (!ofc_sema_stmt_write__print_optional(
			cs,	stmt->io_write.err))
				return false;
	}

	if (!ofc_colstr_atomic_writef(cs, ")"))
		return false;

	return true;
}
