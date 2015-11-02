#include <ofc/sema.h>


bool ofc_sema_stmt_dimension(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_DIMENSION)
		|| !stmt->dimension)
		return false;

	unsigned i;
	for (i = 0; i < stmt->dimension->count; i++)
	{
		ofc_parse_lhs_t* lhs
			= stmt->dimension->lhs[i];
		if (!lhs) continue;

		if (lhs->type != OFC_PARSE_LHS_ARRAY)
		{
			ofc_sema_scope_error(scope, lhs->src,
				"DIMENSION entry must contain array dimensions.");
			return false;
		}

		if (!lhs->parent
			|| (lhs->parent->type != OFC_PARSE_LHS_VARIABLE))
		{
			/* TODO - Support nested arrays in DIMENSION. */
			ofc_sema_scope_error(scope, lhs->src,
				"Nested arrays not yet supported in DIMENSION.");
			return false;
		}

        ofc_str_ref_t base_name;
		if (!ofc_parse_lhs_base_name(
			*lhs, &base_name))
			return false;

		ofc_sema_decl_t* decl
			= ofc_sema_scope_decl_find_modify(
				scope, base_name);
		if (!decl)
		{
			decl = ofc_sema_decl_implicit_name(
				scope, base_name);
			if (!decl)
			{
				ofc_str_ref_t n = base_name;
				ofc_sema_scope_error(scope, lhs->src,
					"No declaration for '%.*s' and no valid IMPLICIT rule.",
					n.size, n.base);
				return false;
			}

			if (!ofc_sema_decl_list_add(
				scope->decl, decl))
			{
				ofc_sema_decl_delete(decl);
				return false;
			}
		}

		if (ofc_sema_decl_is_initialized(decl))
		{
			ofc_sema_scope_error(scope, lhs->src,
				"Can't modify dimensions of initialized declaration.");
			return false;
		}

		ofc_sema_array_t* array
			= ofc_sema_array(
				scope, NULL, lhs->array.index);
		if (!array) return false;

		const ofc_sema_type_t* type
			= ofc_sema_type_create_array(
				decl->type, array,
				false, false, false);
		if (!type)
		{
			ofc_sema_array_delete(array);
			return false;
		}

		decl->type = type;
	}

	return true;
}
