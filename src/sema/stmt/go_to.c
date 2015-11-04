#include <ofc/sema.h>


static ofc_sema_stmt_t* ofc_sema_stmt_go_to__assigned(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_GO_TO;
	s.go_to.label = ofc_sema_expr(
		scope, stmt->go_to_list.cond);
	if (!s.go_to.label) return NULL;

	if (!ofc_sema_expr_validate_uint(s.go_to.label))
	{
		ofc_sema_scope_error(scope, s.go_to.label->src,
			"GO TO target must be a positive integer.");
		ofc_sema_expr_delete(s.go_to.label);
		return NULL;
	}

	s.go_to.allow = ofc_sema_expr_list(
		scope, stmt->go_to_list.label);
	if (!s.go_to.allow)
	{
		ofc_sema_expr_delete(s.go_to.label);
		return NULL;
	}

	unsigned i;
	for (i = 0; i < s.go_to.allow->count; i++)
	{
		ofc_sema_expr_t* expr
			= s.go_to.allow->expr[i];

		if (!ofc_sema_expr_is_constant(expr))
		{
			ofc_sema_scope_error(scope, expr->src,
				"Assigned GO TO allow list entry must be constant.");
			ofc_sema_expr_list_delete(s.go_to.allow);
			ofc_sema_expr_delete(s.go_to.label);
			return NULL;
		}

		if (!ofc_sema_expr_validate_uint(expr))
		{
			ofc_sema_scope_error(scope, expr->src,
				"Assigned GO TO allow list entry must be a positive INTEGER.");
			ofc_sema_expr_list_delete(s.go_to.allow);
			ofc_sema_expr_delete(s.go_to.label);
			return NULL;
		}
	}

	if (ofc_sema_expr_is_constant(s.go_to.label))
	{
		const ofc_sema_typeval_t* label_ctv
			= ofc_sema_expr_constant(s.go_to.label);

		bool match = false;
		for (i = 0; i < s.go_to.allow->count; i++)
		{
			ofc_sema_expr_t* expr
				= s.go_to.allow->expr[i];

			const ofc_sema_typeval_t* ctv
				= ofc_sema_expr_constant(expr);

			if (ofc_sema_typeval_compare(label_ctv, ctv))
			{
				match = true;
				break;
			}
		}

		if (!match)
		{
			ofc_sema_scope_error(scope, s.go_to.label->src,
				"Assigned GO TO target not in allow list.");
			ofc_sema_expr_list_delete(s.go_to.allow);
			ofc_sema_expr_delete(s.go_to.label);
			return NULL;
		}

		ofc_sema_scope_warning(scope, s.go_to.label->src,
			"Using assigned GO TO for a constant label makes little sense.");
	}

	ofc_sema_stmt_t* as
		= ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_list_delete(s.go_to.allow);
		ofc_sema_expr_delete(s.go_to.label);
		return NULL;
	}

	return as;
}

static ofc_sema_stmt_t* ofc_sema_stmt_go_to__computed(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_GO_TO_COMPUTED;
	s.go_to_comp.cond = ofc_sema_expr(
		scope, stmt->go_to_list.cond);
	if (!s.go_to_comp.cond) return NULL;

	const ofc_sema_type_t* type
		= ofc_sema_expr_type(s.go_to_comp.cond);
	if (!ofc_sema_type_is_scalar(type))
	{
		ofc_sema_scope_error(scope, s.go_to_comp.cond->src,
			"Computed GO TO value must be scalar.");
		ofc_sema_expr_delete(s.go_to_comp.cond);
		return NULL;
	}

	if (!ofc_sema_type_is_integer(type))
	{
		ofc_sema_expr_t* cast
			= ofc_sema_expr_cast(
				scope, s.go_to_comp.cond,
				ofc_sema_type_integer_default());
		if (!cast)
		{
			ofc_sema_expr_delete(s.go_to_comp.cond);
			return NULL;
		}
		s.go_to_comp.cond = cast;
	}

	s.go_to_comp.label = ofc_sema_expr_list(
		scope, stmt->go_to_list.label);
	if (!s.go_to_comp.label)
	{
		ofc_sema_expr_delete(s.go_to_comp.cond);
		return NULL;
	}

	unsigned i;
	for (i = 0; i < s.go_to_comp.label->count; i++)
	{
		ofc_sema_expr_t* expr
			= s.go_to_comp.label->expr[i];

		if (!ofc_sema_expr_is_constant(expr))
		{
			ofc_sema_scope_warning(scope, expr->src,
				"Computed GO TO label list entry should be constant.");
		}

		if (!ofc_sema_expr_validate_uint(expr))
		{
			ofc_sema_scope_error(scope, expr->src,
				"Computed GO TO label list entry must be a positive INTEGER.");
			ofc_sema_expr_list_delete(s.go_to_comp.label);
			ofc_sema_expr_delete(s.go_to_comp.cond);
			return NULL;
		}
	}

	if (ofc_sema_expr_is_constant(s.go_to_comp.cond))
	{
		ofc_sema_scope_warning(scope, s.go_to_comp.cond->src,
			"Using computed GO TO for a constant value makes little sense.");
	}

	ofc_sema_stmt_t* as
		= ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_list_delete(s.go_to_comp.label);
		ofc_sema_expr_delete(s.go_to_comp.cond);
		return NULL;
	}

	return as;
}


ofc_sema_stmt_t* ofc_sema_stmt_go_to(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt)
		return NULL;

	switch (stmt->type)
	{
		case OFC_PARSE_STMT_GO_TO:
			break;
		case OFC_PARSE_STMT_GO_TO_ASSIGNED:
			return ofc_sema_stmt_go_to__assigned(scope, stmt);
		case OFC_PARSE_STMT_GO_TO_COMPUTED:
			return ofc_sema_stmt_go_to__computed(scope, stmt);
		default:
			return NULL;
	}

	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_GO_TO;
	s.go_to.label = ofc_sema_expr(
		scope, stmt->go_to.label);
	if (!s.go_to.label) return NULL;

	if (!ofc_sema_expr_validate_uint(s.go_to.label))
	{
		ofc_sema_scope_error(scope, s.go_to.label->src,
			"GO TO target must be a positive integer");
		ofc_sema_expr_delete(s.go_to.label);
		return NULL;
	}

	if (!ofc_sema_expr_is_constant(s.go_to.label))
	{
		ofc_sema_scope_warning(scope, s.go_to.label->src,
			"Should use assigned GO TO when target isn't a constant label");
	}

	s.go_to.allow = NULL;

	ofc_sema_stmt_t* as
		= ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_delete(s.go_to.label);
		return NULL;
	}

	return as;
}
