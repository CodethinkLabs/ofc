#include <ofc/sema.h>


bool ofc_sema_stmt_common(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_COMMON)
		|| !stmt->common_namelist)
		return false;

	unsigned g;
	for (g = 0; g < stmt->common_namelist->count; g++)
	{
		ofc_parse_common_group_t* group
			= stmt->common_namelist->group[g];
		if (!group || !group->names)
			continue;

		ofc_sema_common_t* common
			= ofc_sema_scope_common_find_create(
				scope, group->group);
		if (!common) return false;

		unsigned i;
		for (i = 0; i < group->names->count; i++)
		{
			ofc_parse_lhs_t* lhs
				= group->names->lhs[i];
			if (!lhs) continue;

			ofc_str_ref_t base_name;
			if (!ofc_parse_lhs_base_name(
				*lhs, &base_name))
				return false;

			if ((lhs->type != OFC_PARSE_LHS_VARIABLE)
				&& (lhs->type != OFC_PARSE_LHS_ARRAY))
			{
				ofc_sema_scope_error(scope, lhs->src,
					"Invalid LHS '%.*s' in COMMON list.",
					base_name.size, base_name.base);
				return false;
			}

			ofc_sema_decl_t* decl
				= ofc_sema_scope_decl_find_modify(
					scope, base_name, false);
			if (!decl)
			{
				decl = ofc_sema_decl_implicit_lhs(
					scope, lhs);
				if (!decl)
				{
					ofc_sema_scope_error(scope, lhs->src,
						"No declaration for '%.*s' and no valid IMPLICIT rule.",
						base_name.size, base_name.base);
					return false;
				}

				if (!ofc_sema_decl_list_add(
					scope->decl, decl))
				{
					ofc_sema_decl_delete(decl);
					return false;
				}
			}
			else if (lhs->type != OFC_PARSE_LHS_VARIABLE)
			{
				ofc_sema_scope_error(scope, lhs->src,
					"Can't mark part of array '%.*s' as COMMON.",
					base_name.size, base_name.base);
				return false;
			}

			if (!ofc_sema_common_add(
				common, decl))
			{
				if (ofc_str_ref_empty(base_name))
				{
					ofc_sema_scope_error(scope, lhs->src,
						"Failed to add '%.*s' to global COMMON block.",
						base_name.size, base_name.base);
				}
				else
				{
					ofc_sema_scope_error(scope, lhs->src,
						"Failed to add '%.*s' to COMMON block '%.*s'.",
						base_name.size, base_name.base,
						group->group.size, group->group.base);
				}
				return false;
			}
		}
	}

	return true;
}
