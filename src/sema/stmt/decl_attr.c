#include <ofc/sema.h>


static bool ofc_sema_stmt_decl_attr__decl(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt)
		return false;

	unsigned i;
	for (i = 0; i < stmt->decl_attr.count; i++)
	{
		ofc_str_ref_t decl_name = *stmt->decl_attr.name[i];

		ofc_sema_decl_t* decl =
			ofc_sema_scope_decl_find_modify(
				scope, decl_name);

		if (!decl)
		{
			decl = ofc_sema_decl_implicit_name(
				scope, decl_name);

			if (!decl)
			{
				ofc_sema_scope_error(scope, stmt->src,
					"No declaration for '%.*s' and no valid IMPLICIT rule.",
					decl_name.size, decl_name.base);
				return false;
			}

			if (!ofc_sema_decl_list_add(
				scope->decl, decl))
			{
				ofc_sema_decl_delete(decl);
				return false;
			}
		}

		switch (stmt->type)
		{
			case OFC_PARSE_STMT_DECL_ATTR_AUTOMATIC:
				decl->is_automatic = true;
				break;
			case OFC_PARSE_STMT_DECL_ATTR_STATIC:
				decl->is_static = true;
				break;
			case OFC_PARSE_STMT_DECL_ATTR_VOLATILE:
				decl->is_volatile = true;
				break;

			default:
				return false;
		}
	}

	return true;
}

static bool ofc_sema_stmt_decl_attr__scope(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt)
		return false;

	/* TODO - Build subroutine and function definitions list */

	return false;
}

bool ofc_sema_stmt_decl_attr(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt)
		return false;

	switch (stmt->type)
	{
		case OFC_PARSE_STMT_DECL_ATTR_EXTERNAL:
		case OFC_PARSE_STMT_DECL_ATTR_INTRINSIC:
			return ofc_sema_stmt_decl_attr__scope(
				scope, stmt);

		case OFC_PARSE_STMT_DECL_ATTR_AUTOMATIC:
		case OFC_PARSE_STMT_DECL_ATTR_STATIC:
		case OFC_PARSE_STMT_DECL_ATTR_VOLATILE:
			return ofc_sema_stmt_decl_attr__decl(
				scope, stmt);

		default:
			break;
	}

	return false;
}
