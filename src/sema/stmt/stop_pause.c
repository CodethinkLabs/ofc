#include <ofc/sema.h>

ofc_sema_stmt_t* ofc_sema_stmt_stop_pause(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt)
		return NULL;

	ofc_sema_stmt_t s;
	switch (stmt->type)
	{
		case OFC_PARSE_STMT_STOP:
			s.type = OFC_SEMA_STMT_STOP;
			break;
		case OFC_PARSE_STMT_PAUSE:
			s.type = OFC_SEMA_STMT_PAUSE;
			break;
		default:
			return NULL;
	}

	ofc_sema_expr_t* expr
		= ofc_sema_expr(
			scope, stmt->stop_pause_return.value);
	if (expr)
	{
		const ofc_sema_type_t* type
			= ofc_sema_expr_type(expr);
		if (!type)
		{
			ofc_sema_expr_delete(expr);
			return NULL;
		}

		if (type->type != OFC_SEMA_TYPE_CHARACTER)
		{
			if (!ofc_sema_type_is_integer(type))
			{
				ofc_sema_scope_error(scope, expr->src,
					"STOP/PAUSE code must be a string or an integer");
				ofc_sema_expr_delete(expr);
				return NULL;
			}

			if (ofc_sema_expr_is_constant(expr))
			{
				unsigned v;
				if (!ofc_sema_expr_resolve_uint(expr, &v)
					|| (v >= 100000))
				{
					ofc_sema_scope_warning(scope, expr->src,
						"STOP/PAUSE code should be a positive integer"
						" less than 5 digits long");
				}
			}
		}
	}
	else if (stmt->stop_pause_return.value)
	{
		/* Failed to resolve expression. */
		return NULL;
	}
	s.stop_pause.str = expr;

	ofc_sema_stmt_t* as
		= ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_delete(
			s.stop_pause.str);
		return NULL;
	}

	return as;
}
