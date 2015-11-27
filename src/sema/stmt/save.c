#include <ofc/sema.h>


bool ofc_sema_stmt_save(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_SAVE))
		return false;

	if (!stmt->save.list)
	{
		/* TODO - Mark all decl's in scope as static. */
		return false;
	}

	unsigned i;
	for (i = 0; i < stmt->save.list->count; i++)
	{
		ofc_parse_save_t* save
			= stmt->save.list->save[i];
		if (!save) continue;

		if (save->is_common)
		{
			ofc_sema_common_t* common
				= ofc_sema_scope_common_find_create(
					scope, save->common);
			/* TODO - Warn when new COMMON block is created in SAVE */
			if (!common) return NULL;

			if (!ofc_sema_common_save(common))
			{
				/* TODO - Allow this. */
				ofc_sema_scope_error(scope, save->common,
					"Can't mark a COMMON block as STATIC after use");
				return false;
			}
		}
		else
		{
			if (!save->lhs)
				return false;

			ofc_str_ref_t base_name;
			if (!ofc_parse_lhs_base_name(
				*(save->lhs), &base_name))
				return false;

			const ofc_sema_decl_t* decl
				= ofc_sema_scope_decl_find(
					scope, base_name, true);
			if (decl)
			{
				/* TODO - Allow application of STATIC attribute to decl. */
				ofc_sema_scope_error(scope, save->lhs->src,
					"Using SAVE on existing declaration not yet supported");
				return false;
			}

			ofc_sema_spec_t* spec
				= ofc_sema_scope_spec_modify(
					scope, base_name);
			if (!spec) return false;

			if (spec->is_automatic)
			{
				ofc_sema_scope_error(scope, save->lhs->src,
					"Can't SAVE an AUTOMATIC variable");
				return false;
			}

			if (spec->is_static)
			{
				ofc_sema_scope_warning(scope, save->lhs->src,
					"SAVE on STATIC variable is redundant");
			}

			spec->is_static = true;
		}
	}

	return true;
}
