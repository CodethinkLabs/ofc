#include <ofc/sema.h>

void ofc_sema_scope_delete(
	ofc_sema_scope_t* scope)
{
	if (!scope)
		return;

	ofc_hashmap_delete(scope->child);

	ofc_sema_decl_list_delete(
		scope->args);

	ofc_sema_implicit_delete(
		scope->implicit);

	ofc_sema_decl_list_delete(
		scope->decl);
	ofc_hashmap_delete(
		scope->parameter);
	ofc_hashmap_delete(
		scope->label);
	ofc_sema_stmt_list_delete(
		scope->stmt);

	free(scope);
}

static bool ofc_sema_scope__add_child(
	ofc_sema_scope_t* scope,
	ofc_sema_scope_t* child)
{
	if (!scope || !child)
		return false;

	if ((scope == child)
		|| (child->parent != scope))
		return false;

	if (!scope->child)
	{
		bool cs = (scope->lang_opts
			&& scope->lang_opts->case_sensitive);

		scope->child = ofc_hashmap_create(
			(void*)(cs ? ofc_str_ref_ptr_hash
				: ofc_str_ref_ptr_hash_ci),
			(void*)(cs ? ofc_str_ref_ptr_equal
				: ofc_str_ref_ptr_equal_ci),
			(void*)ofc_sema_scope_get_name,
			(void*)ofc_sema_scope_delete);
		if (!scope->child) return false;
	}

	return ofc_hashmap_add(
		scope->child, child);
}

static ofc_sema_scope_t* ofc_sema_scope__create(
	ofc_sema_scope_t*      parent,
	const ofc_lang_opts_t* lang_opts,
	const ofc_sparse_t*    src,
	ofc_sema_scope_e       type)
{
	ofc_sema_scope_t* scope
		= (ofc_sema_scope_t*)malloc(
			sizeof(ofc_sema_scope_t));
	if (!scope) return NULL;

	scope->parent = parent;
	scope->child  = NULL;

	scope->lang_opts = lang_opts;
	if (!scope->lang_opts && parent)
		scope->lang_opts = parent->lang_opts;

	scope->src = src;
	if (!scope->src && parent)
		scope->src = parent->src;

	ofc_lang_opts_t opts = ofc_sema_scope_get_lang_opts(scope);

	scope->type        = type;
	scope->name        = OFC_STR_REF_EMPTY;
	scope->return_type = NULL;
	scope->args        = NULL;

	/* TODO - Inherit implicit rules from parent. */
	scope->implicit = ofc_sema_implicit_create();

	scope->common    = NULL;
	scope->decl      = ofc_sema_decl_list_create(opts.case_sensitive);
	scope->parameter = ofc_sema_parameter_map_create(opts.case_sensitive);
	scope->label     = ofc_sema_label_map_create();
	scope->stmt      = ofc_sema_stmt_list_create();

	scope->external = false;
	scope->intrinsic = false;

	if (!scope->implicit
		|| !scope->decl
		|| !scope->parameter
		|| !scope->stmt)
	{
		ofc_sema_scope_delete(scope);
		return NULL;
	}

	return scope;
}

static bool ofc_sema_scope__body(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_list_t* body)
{
	if (!scope)
		return false;

	if (!body)
		return true;

	bool has_exec_stmt = false;

	unsigned i;
	for (i = 0; i < body->count; i++)
	{
        ofc_parse_stmt_t* stmt = body->stmt[i];
		if (!stmt) continue;

		if (stmt->type == OFC_PARSE_STMT_EMPTY)
			continue;

		switch (stmt->type)
		{
			case OFC_PARSE_STMT_PROGRAM:
				if (!ofc_sema_scope_program(scope, stmt))
					return false;
				break;
			case OFC_PARSE_STMT_SUBROUTINE:
				if (!ofc_sema_scope_subroutine(scope, stmt))
					return false;
				break;
			case OFC_PARSE_STMT_FUNCTION:
				if (!ofc_sema_scope_function(scope, stmt))
					return false;
				break;
			case OFC_PARSE_STMT_BLOCK_DATA:
				if (!ofc_sema_scope_block_data(scope, stmt))
					return false;
				break;

			case OFC_PARSE_STMT_PARAMETER:
				if (!ofc_sema_parameter(scope, stmt))
					return false;
				break;

			case OFC_PARSE_STMT_IMPLICIT_NONE:
			case OFC_PARSE_STMT_IMPLICIT:
				if (!ofc_sema_implicit(scope, stmt))
					return false;
				break;

			case OFC_PARSE_STMT_DECL:
				if (!ofc_sema_decl(scope, stmt))
					return false;
				break;

			case OFC_PARSE_STMT_FORMAT:
				if (!ofc_sema_format(scope, stmt))
					return false;
				break;

			case OFC_PARSE_STMT_DATA:
				if (!ofc_sema_stmt_data(scope, stmt))
					return false;
				break;

			case OFC_PARSE_STMT_DIMENSION:
				if (!ofc_sema_stmt_dimension(scope, stmt))
					return false;
				break;

			case OFC_PARSE_STMT_EQUIVALENCE:
				if (!ofc_sema_stmt_equivalence(scope, stmt))
					return false;
				break;

			case OFC_PARSE_STMT_COMMON:
				if (!ofc_sema_stmt_common(scope, stmt))
					return false;
				break;

			case OFC_PARSE_STMT_ENTRY:
			case OFC_PARSE_STMT_NAMELIST:
			case OFC_PARSE_STMT_DECL_ATTR_EXTERNAL:
			case OFC_PARSE_STMT_DECL_ATTR_INTRINSIC:
				ofc_sema_scope_error(scope, stmt->src,
					"Unsuported statement");
				/* TODO - Support these statements. */
				return false;
			case OFC_PARSE_STMT_DECL_ATTR_AUTOMATIC:
			case OFC_PARSE_STMT_DECL_ATTR_STATIC:
			case OFC_PARSE_STMT_DECL_ATTR_VOLATILE:
				if (!ofc_sema_stmt_decl_attr(scope, stmt))
					return false;
				break;
			case OFC_PARSE_STMT_POINTER:
			case OFC_PARSE_STMT_TYPE:
			case OFC_PARSE_STMT_STRUCTURE:
			case OFC_PARSE_STMT_UNION:
			case OFC_PARSE_STMT_MAP:
			case OFC_PARSE_STMT_RECORD:
			case OFC_PARSE_STMT_SAVE:
				ofc_sema_scope_error(scope, stmt->src,
					"Unsuported statement");
				/* TODO - Support these statements. */
				return false;

			default:
				has_exec_stmt = true;
				if (!ofc_sema_stmt_scoped_decl(
					scope, stmt))
					return false;
				break;
		}
	}

	if (has_exec_stmt)
	{
		switch (scope->type)
		{
			case OFC_SEMA_SCOPE_GLOBAL:
			case OFC_SEMA_SCOPE_BLOCK_DATA:
				/* TODO - Error: Unexpected executable statement in scope. */
				return false;
			default:
				break;
		}

		for (i = 0; i < body->count; i++)
		{
			ofc_parse_stmt_t* stmt = body->stmt[i];
			if (!stmt) continue;

			if (stmt->type == OFC_PARSE_STMT_EMPTY)
				continue;

			switch (stmt->type)
			{
				case OFC_PARSE_STMT_PROGRAM:
				case OFC_PARSE_STMT_SUBROUTINE:
				case OFC_PARSE_STMT_FUNCTION:
				case OFC_PARSE_STMT_BLOCK_DATA:
				case OFC_PARSE_STMT_PARAMETER:
				case OFC_PARSE_STMT_IMPLICIT_NONE:
				case OFC_PARSE_STMT_IMPLICIT:
				case OFC_PARSE_STMT_DECL:
				case OFC_PARSE_STMT_FORMAT:
				case OFC_PARSE_STMT_DATA:
				case OFC_PARSE_STMT_ENTRY:
				case OFC_PARSE_STMT_DIMENSION:
				case OFC_PARSE_STMT_EQUIVALENCE:
				case OFC_PARSE_STMT_COMMON:
				case OFC_PARSE_STMT_NAMELIST:
				case OFC_PARSE_STMT_DECL_ATTR_EXTERNAL:
				case OFC_PARSE_STMT_DECL_ATTR_INTRINSIC:
				case OFC_PARSE_STMT_DECL_ATTR_AUTOMATIC:
				case OFC_PARSE_STMT_DECL_ATTR_STATIC:
				case OFC_PARSE_STMT_DECL_ATTR_VOLATILE:
				case OFC_PARSE_STMT_POINTER:
				case OFC_PARSE_STMT_TYPE:
				case OFC_PARSE_STMT_STRUCTURE:
				case OFC_PARSE_STMT_UNION:
				case OFC_PARSE_STMT_MAP:
				case OFC_PARSE_STMT_RECORD:
				case OFC_PARSE_STMT_SAVE:
					/* These are already handled. */
					break;
				default:
					if (!ofc_sema_stmt_scoped(
						scope, stmt))
						return false;
					break;
			}
		}
	}

	return true;
}

ofc_sema_scope_t* ofc_sema_scope_global(
	const ofc_lang_opts_t* lang_opts,
	const ofc_sparse_t* src,
	const ofc_parse_stmt_list_t* list)
{
	if (!list)
		return NULL;

	ofc_sema_scope_t* scope
		= ofc_sema_scope__create(
			NULL, lang_opts, src, OFC_SEMA_SCOPE_GLOBAL);
	if (!scope) return NULL;

	scope->lang_opts = lang_opts;

	if (!ofc_sema_scope__body(scope, list))
	{
		ofc_sema_scope_delete(scope);
		return NULL;
	}

	return scope;
}

ofc_sema_scope_t* ofc_sema_scope_program(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt || !scope
		|| (stmt->type != OFC_PARSE_STMT_PROGRAM))
		return NULL;

	ofc_sema_scope_t* program
		= ofc_sema_scope__create(
			scope, NULL, NULL, OFC_SEMA_SCOPE_PROGRAM);
	if (!program) return NULL;

	program->name = stmt->program.name;

	if (!ofc_sema_scope__body(
		program, stmt->program.body))
	{
		ofc_sema_scope_delete(program);
		return NULL;
	}

	if (!ofc_sema_scope__add_child(scope, program))
	{
		ofc_sema_scope_delete(program);
		return NULL;
	}

	return program;
}

ofc_sema_scope_t* ofc_sema_scope_subroutine(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt)
		return NULL;

	/* TODO - Implement. */
	return NULL;
}

ofc_sema_scope_t* ofc_sema_scope_function(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt)
		return NULL;

	/* TODO - Implement. */
	return NULL;
}

ofc_sema_scope_t* ofc_sema_scope_if(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_list_t* block)
{
	if (!scope || !block)
		return NULL;

	ofc_sema_scope_t* if_scope
		= ofc_sema_scope__create(
			scope, NULL, NULL, OFC_SEMA_SCOPE_IF);
	if (!if_scope) return NULL;

	if (!ofc_sema_scope__body(scope, block))
	{
		ofc_sema_scope_delete(if_scope);
		return NULL;
	}

	return if_scope;
}

ofc_sema_scope_t* ofc_sema_scope_block_data(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt)
		return NULL;

	/* TODO - Implement. */
	return NULL;
}


const ofc_str_ref_t* ofc_sema_scope_get_name(
	const ofc_sema_scope_t* scope)
{
	return (!scope ? NULL : &scope->name);
}

ofc_lang_opts_t ofc_sema_scope_get_lang_opts(
	const ofc_sema_scope_t* scope)
{
	if (!scope)
		return OFC_LANG_OPTS_F77;

	if (scope->lang_opts)
		return *scope->lang_opts;

	return ofc_sema_scope_get_lang_opts(scope->parent);
}



const ofc_sema_decl_t* ofc_sema_scope_decl_find(
	const ofc_sema_scope_t* scope, ofc_str_ref_t name)
{
	if (!scope)
		return NULL;

	const ofc_sema_decl_t* decl
		= ofc_sema_decl_list_find(
			scope->decl, name);
	if (decl) return decl;

	return ofc_sema_scope_decl_find(
		scope->parent, name);
}

ofc_sema_decl_t* ofc_sema_scope_decl_find_modify(
	ofc_sema_scope_t* scope, ofc_str_ref_t name)
{
	if (!scope)
		return NULL;

	ofc_sema_decl_t* decl
		= ofc_sema_decl_list_find_modify(
			scope->decl, name);
	if (decl) return decl;

	return ofc_sema_scope_decl_find_modify(
		scope->parent, name);
}

const ofc_sema_scope_t* ofc_sema_scope_child_find(
	const ofc_sema_scope_t* scope, ofc_str_ref_t name)
{
	if (!scope)
		return NULL;

	const ofc_sema_scope_t* child
		= ofc_hashmap_find(scope->child, &name);
	if (child) return child;

	return ofc_sema_scope_child_find(
		scope->parent, name);
}

ofc_sema_scope_t* ofc_sema_scope_child_find_modify(
	ofc_sema_scope_t* scope, ofc_str_ref_t name)
{
	if (!scope)
		return NULL;

	ofc_sema_scope_t* child
		= ofc_hashmap_find_modify(
			scope->child, &name);
	if (child) return child;

	return ofc_sema_scope_child_find_modify(
		scope->parent, name);
}


ofc_sema_common_t* ofc_sema_scope_common_find_create(
	ofc_sema_scope_t* scope, ofc_str_ref_t name)
{
	if (!scope)
		return NULL;

	ofc_lang_opts_t opts
		= ofc_sema_scope_get_lang_opts(scope);

	ofc_sema_common_t* common = NULL;
	if (!scope->common)
	{
		switch (scope->type)
		{
			case OFC_SEMA_SCOPE_GLOBAL:
			case OFC_SEMA_SCOPE_PROGRAM:
			case OFC_SEMA_SCOPE_FUNCTION:
			case OFC_SEMA_SCOPE_SUBROUTINE:
				break;
			default:
				/* TODO - Error: Can't declare common block here. */
				return NULL;
		}

		scope->common = ofc_sema_common_map_create(
			opts.case_sensitive);
		if (!scope->common) return NULL;
	}
	else
	{
		common = ofc_hashmap_find_modify(
			scope->common, &name);
	}

	if (!common)
	{
		common = ofc_sema_common_create(
			name, opts.case_sensitive);
		if (!common) return NULL;

		if (!ofc_hashmap_add(
			scope->common, common))
		{
			ofc_sema_common_delete(common);
			return NULL;
		}
	}

	return common;
}

bool ofc_sema_scope_common_add(
	ofc_sema_scope_t* scope,
	ofc_str_ref_t group, const ofc_sema_decl_t* decl)
{
	ofc_sema_common_t* common
		= ofc_sema_scope_common_find_create(
			scope, group);
    return ofc_sema_common_add(
		common, decl);
}


void ofc_sema_scope_error(
	const ofc_sema_scope_t* scope, ofc_str_ref_t pos,
	const char* format, ...)
{
	va_list args;
	va_start(args, format);
	ofc_sparse_error_va(scope->src, pos.base, format, args);
	va_end(args);
}

void ofc_sema_scope_warning(
	const ofc_sema_scope_t* scope, ofc_str_ref_t pos,
	const char* format, ...)
{
	va_list args;
	va_start(args, format);
	ofc_sparse_warning_va(scope->src, pos.base, format, args);
	va_end(args);
}
