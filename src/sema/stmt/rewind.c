#include <ofc/sema.h>

ofc_sema_stmt_t* ofc_sema_stmt_io_rewind(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_IO_REWIND)
		|| !stmt->io.params)
		return NULL;

	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_IO_REWIND;
	s.io_rewind.unit        = NULL;
	s.io_rewind.iostat      = NULL;
	s.io_rewind.err         = NULL;

	ofc_parse_call_arg_t* ca_unit   = NULL;
	ofc_parse_call_arg_t* ca_iostat = NULL;
	ofc_parse_call_arg_t* ca_err    = NULL;

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
					"Un-named parameter %u has no meaning in REWIND.", i);
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
					"Re-definition of UNIT in REWIND.");
				return NULL;
			}

			ca_unit = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "IOSTAT"))
		{
			if (ca_iostat)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of IOSTAT in REWIND.");
				return NULL;
			}

			ca_iostat = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name, "ERR"))
		{
			if (ca_err)
			{
				ofc_sema_scope_error(scope, param->src,
					"Re-definition of ERR in REWIND.");
				return NULL;
			}

			ca_err = param;
		}
		else
		{
			ofc_sema_scope_error(scope, param->src,
				"Unrecognized paramater %u name '%.*s' in REWIND.",
				i, param->name.size, param->name.base);
			return NULL;
		}
	}

	if (!ca_unit)
	{
		ofc_sema_scope_error(scope, stmt->src,
			"No UNIT defined in REWIND.");
		return NULL;
	}

	if (ca_unit->type == OFC_PARSE_CALL_ARG_EXPR)
	{
		s.io_rewind.unit = ofc_sema_expr(
			scope, ca_unit->expr);
		if (!s.io_rewind.unit) return NULL;

		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_rewind.unit);
		if (!etype) return NULL;

		if (!ofc_sema_type_is_integer(etype))
		{
			ofc_sema_scope_error(scope, stmt->src,
				"UNIT must be of type INTEGER in REWIND");
			ofc_sema_expr_delete(s.io_rewind.unit);
			return NULL;
		}

		const ofc_sema_typeval_t* evalue
			= ofc_sema_expr_constant(s.io_rewind.unit);
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
						"UNIT must be a positive INTEGER in REWIND");
					ofc_sema_expr_delete(s.io_rewind.unit);
					return NULL;
				}

				/* TODO - Clamp to range? */
			}
        }
	}
	else
	{
		ofc_sema_scope_error(scope, stmt->src,
			"UNIT must be an INTEGER expression in REWIND");
		return NULL;
	}

	if (ca_iostat)
	{
		s.io_rewind.iostat = ofc_sema_expr(
			scope, ca_iostat->expr);
		if (!s.io_rewind.iostat)
		{
			ofc_sema_expr_delete(s.io_rewind.unit);
			return NULL;
		}

		if (s.io_rewind.iostat->type != OFC_SEMA_EXPR_LHS)
		{
			ofc_sema_scope_error(scope, stmt->src,
				"IOSTAT must be of a variable in REWIND");
			ofc_sema_expr_delete(s.io_rewind.unit);
			ofc_sema_expr_delete(s.io_rewind.iostat);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_rewind.iostat);
		if (!etype)
		{
			ofc_sema_expr_delete(s.io_rewind.unit);
			ofc_sema_expr_delete(s.io_rewind.iostat);
			return NULL;
		}

		if (!ofc_sema_type_is_integer(etype))
		{
			ofc_sema_scope_error(scope, stmt->src,
				"IOSTAT must be of type INTEGER in REWIND");
			ofc_sema_expr_delete(s.io_rewind.unit);
			ofc_sema_expr_delete(s.io_rewind.iostat);
			return NULL;
		}

	}

	if (ca_err)
	{
		s.io_rewind.err = ofc_sema_expr(
			scope, ca_err->expr);
		if (!s.io_rewind.err)
		{
			ofc_sema_expr_delete(s.io_rewind.unit);
			ofc_sema_expr_delete(s.io_rewind.iostat);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_rewind.err);
		if (!etype)
		{
			ofc_sema_expr_delete(s.io_rewind.unit);
			ofc_sema_expr_delete(s.io_rewind.iostat);
			ofc_sema_expr_delete(s.io_rewind.err);
			return NULL;
		}

		if (ofc_sema_type_is_integer(etype))
		{
			const ofc_sema_typeval_t* err_label
				= ofc_sema_expr_constant(s.io_rewind.err);
			if (err_label)
			{
				int64_t fl64 = 0;
				if (!ofc_sema_typeval_get_integer(
					err_label, &fl64) || (fl64 < 0))
				{
					ofc_sema_scope_error(scope, stmt->src,
						"Error (ERR) label expression must be a positive INTEGER in REWIND");
					ofc_sema_expr_delete(s.io_rewind.unit);
					ofc_sema_expr_delete(s.io_rewind.iostat);
					ofc_sema_expr_delete(s.io_rewind.err);
					return NULL;
				}

				unsigned ulabel = (unsigned) fl64;

				if (((int64_t) ulabel) != fl64)
				{
					ofc_sema_expr_delete(s.io_rewind.unit);
					ofc_sema_expr_delete(s.io_rewind.iostat);
					ofc_sema_expr_delete(s.io_rewind.err);
					return NULL;
				}

				const ofc_sema_label_t* label
					= ofc_sema_label_map_find(scope->label, ulabel);
				if (!label)
				{
					ofc_sema_scope_error(scope, stmt->src,
						"Error label expression not defined in REWIND");
					ofc_sema_expr_delete(s.io_rewind.unit);
					ofc_sema_expr_delete(s.io_rewind.iostat);
					ofc_sema_expr_delete(s.io_rewind.err);
					return NULL;
				}
			}
		}
		else
		{
			ofc_sema_scope_error(scope, stmt->src,
				"Error (ERR) must be a label in REWIND");
			ofc_sema_expr_delete(s.io_rewind.unit);
			ofc_sema_expr_delete(s.io_rewind.iostat);
			return NULL;
		}
	}

	ofc_sema_stmt_t* as
		= ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_delete(s.io_rewind.unit);
		return NULL;
	}
	return as;
}
