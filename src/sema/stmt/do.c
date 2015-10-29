#include <ofc/sema.h>

ofc_sema_stmt_t* ofc_sema_stmt_do__block(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt
		|| (stmt->type != OFC_PARSE_STMT_DO_BLOCK)
		|| !stmt->do_block.init
		|| !stmt->do_block.last)
		return NULL;

	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_DO_BLOCK;

	/* TODO - Support more advanced LHS. */
	if (stmt->do_block.init->name->type
		!= OFC_PARSE_LHS_VARIABLE)
		return NULL;

	s.do_block.iter = ofc_sema_decl_list_find(
		scope->decl, stmt->do_block.init->name->variable);
	if (!s.do_block.iter) return NULL;

	const ofc_sema_type_t* dtype
		= ofc_sema_decl_type(s.do_block.iter);
	if (!ofc_sema_type_is_scalar(dtype))
	{
		ofc_sema_scope_error(scope,
			stmt->do_block.init->name->src,
				"DO loop iterator must be a scalar type.");

		return NULL;
	}

	if (!ofc_sema_type_is_integer(dtype))
	{
		ofc_sema_scope_warning(scope,
			stmt->do_block.init->name->src,
				"Using REAL in DO loop iterator..");

		return NULL;
	}

	s.do_block.init = ofc_sema_expr(
		scope, stmt->do_block.init->init);
	if (!s.do_block.init) return NULL;

	if (!ofc_sema_type_compare(dtype,
		ofc_sema_expr_type(s.do_block.init)))
	{
		ofc_sema_expr_t* cast
			= ofc_sema_expr_cast(
				scope, s.do_block.init, dtype);
		if (!cast)
		{
			const ofc_sema_type_t* expr_type
				= ofc_sema_expr_type(s.do_block.init);
			ofc_sema_scope_error(scope,
				stmt->do_block.init->init->src,
					"Expression type %s doesn't match lhs type %s",
				ofc_sema_type_str_rep(expr_type->type),
				ofc_sema_type_str_rep(dtype->type));
			ofc_sema_expr_delete(s.do_block.init);
			return NULL;
		}
		s.do_block.init = cast;
	}

	s.do_block.last = ofc_sema_expr(
		scope, stmt->do_block.last);
	if (!s.do_block.last)
	{
		ofc_sema_expr_delete(s.do_block.init);
		return NULL;
	}

	if (!ofc_sema_type_compare(dtype,
		ofc_sema_expr_type(s.do_block.last)))
	{
		ofc_sema_expr_t* cast
			= ofc_sema_expr_cast(
				scope, s.do_block.last, dtype);
		if (!cast)
		{
			const ofc_sema_type_t* expr_type =
				ofc_sema_expr_type(s.do_block.last);
			ofc_sema_scope_error(scope,
				stmt->do_block.last->src,
					"Expression type %s doesn't match lhs type %s",
				ofc_sema_type_str_rep(expr_type->type),
				ofc_sema_type_str_rep(dtype->type));
			ofc_sema_expr_delete(s.do_block.init);
			ofc_sema_expr_delete(s.do_block.last);
			return NULL;
		}
		s.do_block.last = cast;
	}

	s.do_block.step = NULL;
	if (stmt->do_block.step)
	{
		s.do_block.step = ofc_sema_expr(
			scope, stmt->do_block.step);
		if (!s.do_block.block)
		{
			ofc_sema_expr_delete(s.do_block.init);
			ofc_sema_expr_delete(s.do_block.last);
			return NULL;
		}

		if (!ofc_sema_type_compare(dtype,
			ofc_sema_expr_type(s.do_block.step)))
		{
			ofc_sema_expr_t* cast
				= ofc_sema_expr_cast(
					scope, s.do_block.step, dtype);
			if (!cast)
			{
				const ofc_sema_type_t* expr_type =
					ofc_sema_expr_type(s.do_block.step);
				ofc_sema_scope_error(scope,
					stmt->do_block.step->src,
						"Expression type %s doesn't match lhs type %s",
					ofc_sema_type_str_rep(expr_type->type),
					ofc_sema_type_str_rep(dtype->type));
				ofc_sema_expr_delete(s.do_block.init);
				ofc_sema_expr_delete(s.do_block.last);
				ofc_sema_expr_delete(s.do_block.step);
				return NULL;
			}
			s.do_block.step = cast;
		}
	}

	s.do_block.block = NULL;
	if (stmt->do_block.block)
	{
		s.do_block.block = ofc_sema_stmt_list_create();
		if (!s.do_block.block)
		{
			ofc_sema_expr_delete(s.do_block.init);
			ofc_sema_expr_delete(s.do_block.last);
			ofc_sema_expr_delete(s.do_block.step);
			return NULL;
		}

		unsigned i;
		for (i = 0; i < stmt->do_block.block->count; i++)
		{
			ofc_sema_stmt_t* stat = ofc_sema_stmt(
				scope, stmt->do_block.block->stmt[i]);

			if (!ofc_sema_stmt_list_add(
				s.do_block.block, stat))
			{
				ofc_sema_stmt_delete(stat);
				ofc_sema_stmt_list_delete(s.do_block.block);
				ofc_sema_expr_delete(s.do_block.init);
				ofc_sema_expr_delete(s.do_block.last);
				ofc_sema_expr_delete(s.do_block.step);
				return NULL;
			}
		}
	}

	ofc_sema_stmt_t* as = ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_stmt_list_delete(s.do_block.block);
		ofc_sema_expr_delete(s.do_block.init);
		ofc_sema_expr_delete(s.do_block.last);
		ofc_sema_expr_delete(s.do_block.step);
		return NULL;
	}

	return as;
}

ofc_sema_stmt_t* ofc_sema_stmt_do(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt)
		return NULL;

	switch(stmt->type)
	{
		case OFC_PARSE_STMT_DO_BLOCK:
			return ofc_sema_stmt_do__block(scope, stmt);
		default:
			break;
	}

	return NULL;
}
