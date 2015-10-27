#include <ofc/sema.h>


ofc_sema_stmt_t* ofc_sema_stmt_io_write(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_IO_WRITE)
		|| !stmt->io.params)
		return NULL;

	ofc_sema_stmt_t s;
	s.io_write.unit        = NULL;
	s.io_write.stdout      = false;
	s.io_write.format_expr = NULL;
	s.io_write.format      = NULL;
	s.io_write.format_ldio = false;
	s.io_write.iostat      = NULL;
	s.io_write.rec         = NULL;
	s.io_write.err         = NULL;

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

		if (etype->type != OFC_SEMA_TYPE_INTEGER)
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
		if (!s.io_write.unit) return NULL;

		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_write.format_expr);
		if (!etype) return NULL;

		if (etype->type == OFC_SEMA_TYPE_INTEGER)
		{
			const ofc_sema_typeval_t* format_label
				= ofc_sema_expr_constant(s.io_write.format_expr);
			if (!format_label)
			{
				ofc_sema_scope_error(scope, stmt->src,
					"Format (FMT) label expression must be resolvable"
					" at compile time in WRITE");
				ofc_sema_expr_delete(s.io_write.format_expr);
				ofc_sema_expr_delete(s.io_write.unit);
				return NULL;
			}

			int64_t fl64 = 0;
			ofc_sema_typeval_get_integer(format_label, &fl64);

			if (fl64 < 0)
			{
				ofc_sema_scope_error(scope, stmt->src,
					"Format (FMT) label expression must be a positive INTEGER in WRITE");
				ofc_sema_expr_delete(s.io_write.format_expr);
				ofc_sema_expr_delete(s.io_write.unit);
				return NULL;
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
		/* TODO - Validate IOSTAT as type INTEGER. */
	}

	if (ca_rec)
	{
		/* TODO - Validate REC as positive INTEGER. */
	}

	if (ca_err)
	{
		/* TODO - Validate ERR as possible label. */
	}

	if (s.io_write.format)
	{
		/* TODO - Validate iolist against FORMAT. */
	}

	ofc_sema_stmt_t* as
		= ofc_sema_stmt_alloc(s);
	if (!as)
	{
		return NULL;
	}
	return as;
}
