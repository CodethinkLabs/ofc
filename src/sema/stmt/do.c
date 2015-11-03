#include <ofc/sema.h>


bool ofc_sema_stmt_do_decl(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt)
		return false;


	ofc_parse_assign_t* assign;
	switch (stmt->type)
	{
		case OFC_PARSE_STMT_DO_LABEL:
			assign = stmt->do_label.init;
			break;

		case OFC_PARSE_STMT_DO_BLOCK:
			assign = stmt->do_block.init;
			break;

		case OFC_PARSE_STMT_DO_WHILE:
		case OFC_PARSE_STMT_DO_WHILE_BLOCK:
			return true;

		default:
			return false;
	}

	if (!assign || !assign->name)
		return false;

	ofc_parse_lhs_t* lhs
		= assign->name;

	ofc_str_ref_t base_name;
	if (!ofc_parse_lhs_base_name(
		*lhs, &base_name))
		return false;

	const ofc_sema_decl_t* iter
		= ofc_sema_scope_decl_find(
			scope, base_name);
	if (iter) return true;

	if (lhs->type != OFC_PARSE_LHS_VARIABLE)
	{
		ofc_sema_scope_error(scope, lhs->src,
			"Can't implicitly declare array as DO loop iterator.");
		return false;
	}

	ofc_sema_decl_t* idecl
		= ofc_sema_decl_implicit_lhs(scope, lhs);
	if (!idecl) return false;

	if (!ofc_sema_decl_list_add(
		scope->decl, idecl))
	{
		ofc_sema_decl_delete(idecl);
		return false;
	}

	return true;
}


static bool ofc_sema_stmt__loop_control(
	ofc_sema_scope_t* scope,
	const ofc_parse_assign_t* parse_init,
	const ofc_parse_expr_t* parse_last,
	const ofc_parse_expr_t* parse_step,
	const ofc_sema_decl_t** sema_iter,
	ofc_sema_expr_t** sema_init,
	ofc_sema_expr_t** sema_last,
	ofc_sema_expr_t** sema_step)
{
	*sema_iter = ofc_sema_scope_decl_find(
		scope, parse_init->name->variable);
	if (!*sema_iter) return false;

	const ofc_sema_type_t* dtype
		= ofc_sema_decl_type(*sema_iter);
	if (!ofc_sema_type_is_scalar(dtype))
	{
		ofc_sema_scope_error(scope,
			parse_init->name->src,
				"DO loop iterator must be a scalar type.");

		return false;
	}

	if (!ofc_sema_type_is_integer(dtype))
	{
		ofc_sema_scope_warning(scope,
			parse_init->name->src,
				"Using REAL in DO loop iterator..");

		return false;
	}

	*sema_init = ofc_sema_expr(
		scope, parse_init->init);
	if (!*sema_init) return false;

	if (!ofc_sema_type_compare(dtype,
		ofc_sema_expr_type(*sema_init)))
	{
		ofc_sema_expr_t* cast
			= ofc_sema_expr_cast(
				scope, *sema_init, dtype);
		if (!cast)
		{
			const ofc_sema_type_t* expr_type
				= ofc_sema_expr_type(*sema_init);
			ofc_sema_scope_error(scope,
				parse_init->init->src,
					"Expression type %s doesn't match lhs type %s",
				ofc_sema_type_str_rep(expr_type->type),
				ofc_sema_type_str_rep(dtype->type));
			ofc_sema_expr_delete(*sema_init);
			return false;
		}
		*sema_init = cast;
	}

	*sema_last = ofc_sema_expr(
		scope, parse_last);
	if (!*sema_last)
	{
		ofc_sema_expr_delete(*sema_init);
		return false;
	}

	if (!ofc_sema_type_compare(dtype,
		ofc_sema_expr_type(*sema_last)))
	{
		ofc_sema_expr_t* cast
			= ofc_sema_expr_cast(
				scope, *sema_last, dtype);
		if (!cast)
		{
			const ofc_sema_type_t* expr_type =
				ofc_sema_expr_type(*sema_last);
			ofc_sema_scope_error(scope,
				parse_last->src,
					"Expression type %s doesn't match lhs type %s",
				ofc_sema_type_str_rep(expr_type->type),
				ofc_sema_type_str_rep(dtype->type));
			ofc_sema_expr_delete(*sema_init);
			ofc_sema_expr_delete(*sema_last);
			return false;
		}
		*sema_last = cast;
	}

	*sema_step = NULL;
	if (parse_step)
	{
		*sema_step = ofc_sema_expr(
			scope, parse_step);
		if (!*sema_step)
		{
			ofc_sema_expr_delete(*sema_init);
			ofc_sema_expr_delete(*sema_last);
			return false;
		}

		if (!ofc_sema_type_compare(dtype,
			ofc_sema_expr_type(*sema_step)))
		{
			ofc_sema_expr_t* cast
				= ofc_sema_expr_cast(
					scope, *sema_step, dtype);
			if (!cast)
			{
				const ofc_sema_type_t* expr_type =
					ofc_sema_expr_type(*sema_step);
				ofc_sema_scope_error(scope,
					parse_step->src,
						"Expression type %s doesn't match lhs type %s",
					ofc_sema_type_str_rep(expr_type->type),
					ofc_sema_type_str_rep(dtype->type));
				ofc_sema_expr_delete(*sema_init);
				ofc_sema_expr_delete(*sema_last);
				ofc_sema_expr_delete(*sema_step);
				return false;
			}
			*sema_step = cast;
		}
	}

	return true;
}

ofc_sema_stmt_t* ofc_sema_stmt_do__label(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt
		|| (stmt->type != OFC_PARSE_STMT_DO_LABEL)
		|| !stmt->do_label.init
		|| !stmt->do_label.last
		|| !stmt->do_label.end_label)
		return NULL;

	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_DO_LABEL;
	s.do_label.iter = NULL;
	s.do_label.init = NULL;
	s.do_label.last = NULL;
	s.do_label.step = NULL;

	s.do_label.end_label = ofc_sema_expr(
		scope, stmt->do_label.end_label);
	if (!s.do_label.end_label)
		return NULL;

	if (!ofc_sema_stmt__loop_control(
		scope, stmt->do_label.init,
		stmt->do_label.last, stmt->do_label.step,
		&s.do_label.iter, &s.do_label.init,
		&s.do_label.last, &s.do_label.step))
	{
		ofc_sema_expr_delete(s.do_label.end_label);
		return NULL;
	}

	ofc_sema_stmt_t* as = ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_delete(s.do_label.end_label);
		ofc_sema_expr_delete(s.do_label.init);
		ofc_sema_expr_delete(s.do_label.last);
		ofc_sema_expr_delete(s.do_label.step);
		return NULL;
	}

	return as;
}

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
	s.do_block.iter = NULL;
	s.do_block.init = NULL;
	s.do_block.last = NULL;
	s.do_block.step = NULL;

	if (!ofc_sema_stmt__loop_control(
		scope, stmt->do_block.init,
		stmt->do_block.last, stmt->do_block.step,
		&s.do_block.iter, &s.do_block.init,
		&s.do_block.last, &s.do_block.step))
		return NULL;


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

ofc_sema_stmt_t* ofc_sema_stmt_do_while__label(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{

	if (!stmt
		|| (stmt->type != OFC_PARSE_STMT_DO_WHILE)
		|| !stmt->do_while_block.cond)
		return NULL;

	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_DO_WHILE;

	s.do_while.end_label = ofc_sema_expr(
	scope, stmt->do_while.end_label);
	if (!s.do_while.end_label)
		return NULL;

	s.do_while.cond = ofc_sema_expr(
		scope, stmt->do_while.cond);
	if (!s.do_while.cond)
		return NULL;

	const ofc_sema_type_t* type
		= ofc_sema_expr_type(s.do_while.cond);
	if (!ofc_sema_type_is_logical(type))
	{
		ofc_sema_scope_error(scope, stmt->do_while.cond->src,
			"IF condition type must be LOGICAL.");

		ofc_sema_expr_delete(s.do_while.cond);
		return NULL;
	}

	ofc_sema_stmt_t* as = ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_delete(s.do_while.cond);
		return NULL;
	}

	return as;
}

ofc_sema_stmt_t* ofc_sema_stmt_do_while__block(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{

	if (!stmt
		|| (stmt->type != OFC_PARSE_STMT_DO_WHILE_BLOCK)
		|| !stmt->do_while_block.cond)
		return NULL;

	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_DO_WHILE_BLOCK;

	s.do_while_block.cond = ofc_sema_expr(
		scope, stmt->do_while_block.cond);
	if (!s.do_while_block.cond)
		return NULL;

	const ofc_sema_type_t* type
		= ofc_sema_expr_type(s.do_while_block.cond);
	if (!ofc_sema_type_is_logical(type))
	{
		ofc_sema_scope_error(scope, stmt->do_while_block.cond->src,
			"IF condition type must be LOGICAL.");

		ofc_sema_expr_delete(s.do_while_block.cond);
		return NULL;
	}

	s.do_while_block.block = NULL;
	if (stmt->do_while_block.block)
	{

		s.do_while_block.block = ofc_sema_stmt_list_create();

		if (!s.do_while_block.block)
		{
			ofc_sema_expr_delete(s.do_while_block.cond);
			return NULL;
		}

		unsigned i;
		for (i = 0; i < stmt->do_while_block.block->count; i++)
		{
			ofc_sema_stmt_t* stat = ofc_sema_stmt(
				scope, stmt->do_while_block.block->stmt[i]);

			if (!ofc_sema_stmt_list_add(
				s.do_while_block.block, stat))
			{
				ofc_sema_stmt_delete(stat);
				ofc_sema_expr_delete(s.do_while_block.cond);
				ofc_sema_stmt_list_delete(s.do_while_block.block);
				return NULL;
			}
		}
	}

	ofc_sema_stmt_t* as = ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_stmt_list_delete(s.do_while_block.block);
		ofc_sema_expr_delete(s.do_while_block.cond);
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
		case OFC_PARSE_STMT_DO_LABEL:
			return ofc_sema_stmt_do__label(scope, stmt);
		case OFC_PARSE_STMT_DO_BLOCK:
			return ofc_sema_stmt_do__block(scope, stmt);
		case OFC_PARSE_STMT_DO_WHILE:
			return ofc_sema_stmt_do_while__label(scope, stmt);
		case OFC_PARSE_STMT_DO_WHILE_BLOCK:
			return ofc_sema_stmt_do_while__block(scope, stmt);

		default:
			break;
	}

	return NULL;
}
