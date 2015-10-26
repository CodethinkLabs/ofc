#include <ofc/sema.h>

void ofc_sema_scope_delete(
	ofc_sema_scope_t* scope)
{
	if (!scope)
		return;

	if (scope->child)
	{
		unsigned i;
		for (i = 0; i < scope->child->count; i++)
			ofc_sema_scope_delete(scope->child->scope[i]);
		free(scope->child->scope);
		free(scope->child);
	}

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
		scope->child = (ofc_sema_scope_list_t*)malloc(
			sizeof(ofc_sema_scope_list_t));
		if (!scope->child) return false;

		scope->child->count = 0;
		scope->child->scope = NULL;
	}

	ofc_sema_scope_t** nlist
		= (ofc_sema_scope_t**)realloc(scope->child->scope,
			(sizeof(ofc_sema_scope_t*) * (scope->child->count + 1)));
	if (!nlist) return false;
	scope->child->scope = nlist;

	scope->child->scope[scope->child->count++] = child;
	return true;
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

	scope->implicit = ofc_sema_implicit_create();

	scope->decl = ofc_sema_decl_list_create(opts.case_sensitive);
	scope->parameter = ofc_sema_parameter_map_create(opts.case_sensitive);
	scope->label = ofc_sema_label_map_create();
	scope->stmt = ofc_sema_stmt_list_create();

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

			default:
				if (!ofc_sema_stmt(scope, stmt))
					return false;
				break;
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

	if (!ofc_sema_scope__body(scope, stmt->program.body))
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
	/* TODO - Implement. */
	return NULL;
}

ofc_sema_scope_t* ofc_sema_scope_function(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	/* TODO - Implement. */
	return NULL;
}

ofc_sema_scope_t* ofc_sema_scope_block_data(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	/* TODO - Implement. */
	return NULL;
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

void ofc_sema_scope_error(
	const ofc_sema_scope_t* scope, ofc_str_ref_t pos,
	const char* format, ...)
{
	va_list args;
	va_start(args, format);
	ofc_sparse_error(scope->src, pos.base, format, args);
	va_end(args);
}

void ofc_sema_scope_warning(
	const ofc_sema_scope_t* scope, ofc_str_ref_t pos,
	const char* format, ...)
{
	va_list args;
	va_start(args, format);
	ofc_sparse_warning(scope->src, pos.base, format, args);
	va_end(args);
}
