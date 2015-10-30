#include <ofc/sema.h>


bool ofc_sema_stmt_equivalence(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_EQUIVALENCE))
		return false;

	unsigned g;
	for (g = 0; g < stmt->equivalence.count; g++)
	{
		const ofc_parse_lhs_list_t* list
			= stmt->equivalence.group[g];
		if (!list || (list->count <= 1))
			continue;

		ofc_sema_lhs_t* base
			= ofc_sema_lhs(scope, list->lhs[0]);
		if (!base) return false;

		unsigned i;
		for (i = 1; i < list->count; i++)
		{
			ofc_sema_lhs_t* elhs
				= ofc_sema_lhs(scope, list->lhs[i]);
			if (!elhs)
			{
				ofc_sema_lhs_delete(base);
				return false;
			}

			if (!ofc_sema_type_compare(
				ofc_sema_lhs_type(base),
				ofc_sema_lhs_type(elhs)))
			{
				ofc_sema_scope_warning(scope, list->lhs[0]->src,
					"EQUIVALENCE types don't match.");
			}

			if (!ofc_sema_equiv(
				base, elhs))
			{
				/* TODO - Better error messages for this in 'sema/equiv.c'. */
				ofc_sema_scope_warning(scope, list->lhs[0]->src,
					"EQUIVALENCE statement causes collision.");

				ofc_sema_lhs_delete(elhs);
				ofc_sema_lhs_delete(base);
				return false;
			}
		}

		ofc_sema_lhs_delete(base);
	}

	return true;
}
