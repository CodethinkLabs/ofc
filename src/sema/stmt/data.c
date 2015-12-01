#include <ofc/sema.h>

static bool ofc_sema_stmt__data(
	ofc_sema_scope_t* scope,
	const ofc_parse_lhs_list_t* nlist,
	const ofc_parse_clist_t*   clist)
{
	if (!scope || !clist || !nlist)
		return false;

	/* Resolve destination list. */
	ofc_sema_lhs_t* lhs[nlist->count];
	unsigned        lhsc[nlist->count];

	unsigned lhs_count = 0;

	unsigned i;
	for (i = 0; i < nlist->count; i++)
	{
		/* TODO - Handle implicit DO. */

		lhs[i] = ofc_sema_lhs(scope, nlist->lhs[i]);
		if (!lhs[i])
		{
			unsigned j;
			for (j = 0; j < i; j++)
				ofc_sema_lhs_delete(lhs[j]);
			return false;
		}

		lhsc[i] = ofc_sema_lhs_elem_count(lhs[i]);

		if (lhsc[i] == 0)
		{
			unsigned j;
			for (j = 0; j <= i; j++)
				ofc_sema_lhs_delete(lhs[j]);
			return false;
		}

		lhs_count += lhsc[i];
	}

	/* Expand LHS list to match size. */
	ofc_sema_lhs_t* elhs[lhs_count];
	unsigned        elhsc[lhs_count];

	unsigned j;
	for (i = 0, j = 0; i < nlist->count; i++)
	{
		elhs[j]  = lhs[i];
		elhsc[j] = lhsc[i];
		j++;

		unsigned k;
		for (k = 1; k < lhsc[i]; k++, j++)
		{
			elhs[j]  = NULL;
			elhsc[j] = (lhsc[i] - k);
		}
	}

	/* Resolve expression list. */
	ofc_sema_expr_t* expr[clist->count];
	unsigned         exprc[clist->count];

	unsigned expr_count = 0;
	for (i = 0; i < clist->count; i++)
	{
		expr[i] = ofc_sema_expr(
			scope, clist->entry[i]->expr);
		if (!expr[i])
		{
			for (j = 0; j < i; j++)
				ofc_sema_expr_delete(expr[j]);
			for (j = 0; j < nlist->count; j++)
				ofc_sema_lhs_delete(lhs[j]);
			return false;
		}

		exprc[i] = clist->entry[i]->repeat;
		if (exprc[i] == 0) exprc[i] = 1;

		expr_count += exprc[i];
	}

	/* Expand expression list. */
	const ofc_sema_expr_t* eexpr[expr_count];

	for (i = 0, j = 0; i < clist->count; i++)
	{
		unsigned k;
		for (k = 0; k < exprc[i]; k++)
			eexpr[j++] = expr[i];
	}

	unsigned count = lhs_count;
	if (expr_count < lhs_count)
	{
		ofc_sema_scope_warning(scope, lhs[0]->src,
			"Not enough initializer elements in DATA statement");
		count = expr_count;
	}
	else if (expr_count > lhs_count)
	{
		ofc_sema_scope_warning(scope, expr[0]->src,
			"Too many initializer elements in DATA statement, ignoring");
	}

	bool success = true;
	for (i = 0; i < count; i++)
	{
		const ofc_sema_type_t* ltype
			= ofc_sema_lhs_type(elhs[i]);
		if (ofc_sema_type_is_composite(ltype))
		{
			unsigned elem_count = elhsc[i];
			if (elem_count > (count - i))
				elem_count = (count - i);

			if (elhs[i]->type == OFC_SEMA_LHS_ARRAY_SLICE)
			{
				ofc_sema_scope_error(scope, elhs[i]->src,
					"Array slice initializers not yet supported");
				success = false;
			}
			else
			{
				if (!ofc_sema_lhs_init_array(
					scope, elhs[i], NULL,
					elem_count, &eexpr[i]))
				{
					ofc_sema_scope_error(scope, elhs[i]->src,
						"Invalid array LHS in DATA statement");
					success = false;
				}
			}

			i += (elem_count - 1);
		}
		else
		{
			if (!ofc_sema_lhs_init(
				scope, elhs[i], eexpr[i]))
			{
				ofc_sema_scope_error(scope, elhs[i]->src,
					"Invalid LHS in DATA statement");
				success = false;
			}
		}
	}

	for (i = 0; i < nlist->count; i++)
		ofc_sema_lhs_delete(lhs[i]);
	for (i = 0; i < clist->count; i++)
		ofc_sema_expr_delete(expr[i]);

	return success;
}


bool ofc_sema_stmt_data(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_DATA)
		|| !stmt->data)
		return false;

	unsigned j;
	for (j = 0; j < stmt->data->count; j++)
	{
		const ofc_parse_data_entry_t* entry
			= stmt->data->entry[j];
		if (!entry) continue;

		if (!ofc_sema_stmt__data(scope,
			entry->nlist, entry->clist))
			return false;
	}

	return true;
}
