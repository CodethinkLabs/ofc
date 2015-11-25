#include <ofc/sema.h>


void ofc_sema_scope_delete(
	ofc_sema_scope_t* scope)
{
	if (!scope)
		return;

	ofc_hashmap_delete(scope->child);

	ofc_str_ref_list_delete(
		scope->args);

	ofc_sema_implicit_delete(
		scope->implicit);

	ofc_sema_common_map_delete(scope->common);
	ofc_hashmap_delete(scope->spec);
	ofc_sema_decl_list_delete(
		scope->decl);
	ofc_hashmap_delete(
		scope->parameter);
	ofc_hashmap_delete(
		scope->label);

	switch (scope->type)
	{
		case OFC_SEMA_SCOPE_STMT_FUNC:
			ofc_sema_expr_delete(scope->expr);
			break;

		default:
			ofc_sema_stmt_list_delete(
				scope->stmt);
			break;
	}

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
	scope->args        = NULL;

	scope->implicit = (parent
		? ofc_sema_implicit_copy(parent->implicit) : NULL);
	if (!scope->implicit)
		scope->implicit = ofc_sema_implicit_create();

	scope->common    = NULL;
	scope->spec      = ofc_sema_spec_map_create(opts.case_sensitive);
	scope->decl      = ofc_sema_decl_list_create(opts.case_sensitive);
	scope->parameter = ofc_sema_parameter_map_create(opts.case_sensitive);
	scope->label     = ofc_sema_label_map_create();

	scope->external = false;
	scope->intrinsic = false;

	switch (scope->type)
	{
		case OFC_SEMA_SCOPE_STMT_FUNC:
			scope->expr = NULL;
			break;

		default:
			scope->stmt = ofc_sema_stmt_list_create();
			if (!scope->stmt)
			{
				ofc_sema_scope_delete(scope);
				return NULL;
			}
			break;
	}

	if (!scope->implicit
		|| !scope->decl
		|| !scope->parameter)
	{
		ofc_sema_scope_delete(scope);
		return NULL;
	}

	return scope;
}


static bool ofc_sema_scope__body(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_list_t* body);

static bool ofc_sema_scope__subroutine(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_SUBROUTINE))
		return false;

	ofc_str_ref_t name = stmt->program.name;
	if (ofc_str_ref_empty(name))
		return false;

	ofc_sema_decl_t* decl
		= ofc_sema_scope_decl_find_modify(
			scope, name, true);
	if (decl)
	{
		if (!ofc_sema_decl_is_subroutine(decl))
		{
			ofc_sema_scope_error(scope, stmt->src,
				"Can't redefine variable as SUBROUTINE");
			return false;
		}
	}
	else
	{
		const ofc_sema_type_t* stype
			= ofc_sema_type_subroutine();
		if (!stype) return false;

		decl = ofc_sema_decl_create(stype, name);
		if (!decl) return false;

		if (!ofc_sema_decl_list_add(
			scope->decl, decl))
		{
			ofc_sema_decl_delete(decl);
			return false;
		}
	}


	ofc_sema_scope_t* sub_scope
		= ofc_sema_scope__create(scope, NULL, NULL,
			OFC_SEMA_SCOPE_SUBROUTINE);
	if (!sub_scope) return false;
	sub_scope->name = name;

	if (stmt->program.args)
	{
		/* TODO - Create exclusive list to avoid duplicate arguments. */
		sub_scope->args = ofc_str_ref_list_create();
		if (!sub_scope->args)
		{
			ofc_sema_scope_delete(sub_scope);
			return false;
		}

		unsigned i;
		for (i = 0; i < stmt->program.args->count; i++)
		{
			/* TODO - Handle ASTERISK and AMPERSAND in SUBROUTINE arguments. */
			ofc_parse_call_arg_t* arg
				= stmt->program.args->call_arg[i];
			if (!arg
				|| (arg->type != OFC_PARSE_CALL_ARG_EXPR)
				|| !arg->expr
				|| (arg->expr->type != OFC_PARSE_EXPR_VARIABLE)
				|| !arg->expr->variable
				|| (arg->expr->variable->type != OFC_PARSE_LHS_VARIABLE))
			{
				ofc_sema_scope_error(scope, stmt->src,
					"SUBROUTINE arguments can only be names");
				ofc_sema_scope_delete(sub_scope);
				return false;
			}

			if (!ofc_str_ref_empty(arg->name))
			{
				ofc_sema_scope_error(scope, stmt->src,
					"Named parameters not valid as SUBROUTINE arguments");
				ofc_sema_scope_delete(sub_scope);
				return false;
			}

			ofc_str_ref_t arg_name
				= arg->expr->variable->variable;

			if (!ofc_str_ref_list_add(
				sub_scope->args, arg_name))
			{
				ofc_sema_scope_delete(sub_scope);
				return false;
			}
		}
	}

	if (!ofc_sema_scope__body(
		sub_scope, stmt->program.body))
	{
		ofc_sema_scope_delete(sub_scope);
		return false;
	}

	if (!ofc_sema_decl_init_func(
		decl, sub_scope))
	{
		ofc_sema_scope_delete(sub_scope);
		return false;
	}

	return true;
}

static bool ofc_sema_scope__function(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_FUNCTION))
		return false;

	ofc_str_ref_t name = stmt->program.name;
	if (ofc_str_ref_empty(name))
		return false;

	ofc_sema_spec_t* spec;
	const ofc_sema_type_t* rtype;
	if (stmt->program.type)
	{
		spec = ofc_sema_spec(scope,
			stmt->program.type);
	}
	else
	{
		spec = ofc_sema_scope_spec_find_final(
			scope, stmt->program.name);
		if (!spec)
		{
			ofc_sema_scope_error(scope, stmt->src,
				"No IMPLICIT type matches FUNCTION name");
			return false;
		}
	}
	rtype = ofc_sema_type_spec(spec);
	ofc_sema_spec_delete(spec);
	if (!rtype) return false;

	const ofc_sema_type_t* ftype
		= ofc_sema_type_create_function(rtype);
	if (!ftype) return false;

	ofc_sema_decl_t* decl
		= ofc_sema_scope_decl_find_modify(
			scope, name, true);
	if (decl)
	{
		if (!ofc_sema_decl_is_function(decl))
		{
			ofc_sema_scope_error(scope, stmt->src,
				"Can't redeclare used variable as FUNCTION");
			return false;
		}
		else if (!ofc_sema_type_compare(rtype,
			ofc_sema_decl_base_type(decl)))
		{
			ofc_sema_scope_error(scope, stmt->src,
				"Conflicting definitions of FUNCTION return type");
			return false;
		}
	}
	else
	{
		decl = ofc_sema_decl_create(ftype, name);
		if (!decl) return false;

		if (!ofc_sema_decl_list_add(
			scope->decl, decl))
		{
			ofc_sema_decl_delete(decl);
			return false;
		}
	}

	ofc_sema_scope_t* func_scope
		= ofc_sema_scope__create(scope, NULL, NULL,
			OFC_SEMA_SCOPE_FUNCTION);
	if (!func_scope) return false;
	func_scope->name = name;

	if (stmt->program.args)
	{
		/* TODO - Create exclusive list to avoid duplicate arguments. */
		func_scope->args = ofc_str_ref_list_create();
		if (!func_scope->args)
		{
			ofc_sema_scope_delete(func_scope);
			return false;
		}

		unsigned i;
		for (i = 0; i < stmt->program.args->count; i++)
		{
			ofc_parse_call_arg_t* arg
				= stmt->program.args->call_arg[i];
			if (!arg
				|| (arg->type != OFC_PARSE_CALL_ARG_EXPR)
				|| !arg->expr
				|| (arg->expr->type != OFC_PARSE_EXPR_VARIABLE)
				|| !arg->expr->variable
				|| (arg->expr->variable->type != OFC_PARSE_LHS_VARIABLE))
			{
				ofc_sema_scope_error(scope, stmt->src,
					"FUNCTION arguments can only be names");
				ofc_sema_scope_delete(func_scope);
				return false;
			}

			if (!ofc_str_ref_empty(arg->name))
			{
				ofc_sema_scope_error(scope, stmt->src,
					"Named parameters not valid as FUNCTION arguments");
				ofc_sema_scope_delete(func_scope);
				return false;
			}

			ofc_str_ref_t arg_name
				= arg->expr->variable->variable;

			if (!ofc_str_ref_list_add(
				func_scope->args, arg_name))
			{
				ofc_sema_scope_delete(func_scope);
				return false;
			}
		}
	}

	if (!ofc_sema_scope__body(
		func_scope, stmt->program.body))
	{
		ofc_sema_scope_delete(func_scope);
		return false;
	}

	if (!ofc_sema_decl_init_func(
		decl, func_scope))
	{
		ofc_sema_scope_delete(func_scope);
		return false;
	}

	return true;
}


static bool ofc_sema_scope__body(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_list_t* body)
{
	if (scope->type == OFC_SEMA_SCOPE_STMT_FUNC)
		return false;

	if (!body)
		return true;

	/* Initial scan for FORMAT statements */
	unsigned i;
	for (i = 0; i < body->count; i++)
	{
		ofc_parse_stmt_t* stmt = body->stmt[i];
		if (!stmt) continue;

		switch (stmt->type)
		{
			case OFC_PARSE_STMT_FORMAT:
				if (!ofc_sema_format(scope, stmt))
					return false;
				break;

			default:
				break;
		}
	}

	for (i = 0; i < body->count; i++)
	{
		ofc_parse_stmt_t* stmt = body->stmt[i];
		if (!stmt) continue;

		if (stmt->type == OFC_PARSE_STMT_EMPTY)
			continue;

		if (ofc_sema_stmt_is_stmt_func(scope, stmt))
		{
			if (!ofc_sema_scope_stmt_func(scope, stmt))
				return false;
			continue;
		}

		switch (stmt->type)
		{
			case OFC_PARSE_STMT_FORMAT:
				/* These are already handled. */
				break;

			case OFC_PARSE_STMT_SUBROUTINE:
				if (!ofc_sema_scope__subroutine(scope, stmt))
					return false;
				break;

			case OFC_PARSE_STMT_FUNCTION:
				if (!ofc_sema_scope__function(scope, stmt))
					return false;
				break;

			case OFC_PARSE_STMT_PROGRAM:
				if (!ofc_sema_scope_program(scope, stmt))
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

			case OFC_PARSE_STMT_DATA:
				if (!ofc_sema_stmt_data(scope, stmt))
					return false;
				break;

			case OFC_PARSE_STMT_DIMENSION:
				if (!ofc_sema_stmt_dimension(scope, stmt))
					return false;
				break;

			case OFC_PARSE_STMT_EQUIVALENCE:
				/* We handle this next pass. */
				break;

			case OFC_PARSE_STMT_COMMON:
				if (!ofc_sema_stmt_common(scope, stmt))
					return false;
				break;

			case OFC_PARSE_STMT_DECL_ATTR_AUTOMATIC:
			case OFC_PARSE_STMT_DECL_ATTR_STATIC:
			case OFC_PARSE_STMT_DECL_ATTR_VOLATILE:
			case OFC_PARSE_STMT_DECL_ATTR_EXTERNAL:
			case OFC_PARSE_STMT_DECL_ATTR_INTRINSIC:
				if (!ofc_sema_stmt_decl_attr(scope, stmt))
					return false;
				break;

			case OFC_PARSE_STMT_ENTRY:
			case OFC_PARSE_STMT_NAMELIST:
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
				switch (scope->type)
				{
					case OFC_SEMA_SCOPE_GLOBAL:
					case OFC_SEMA_SCOPE_BLOCK_DATA:
						ofc_sema_scope_error(scope, stmt->src,
							"Unexpected executable statement in scope.");
						return false;
					default:
						break;
				}

				if (!ofc_sema_stmt_scoped(scope, stmt))
					return false;
				break;
		}
	}

	/* Declare unused specifiers which exist in COMMON blocks. */
	if (scope->common)
	{
		for (i = 0; i < scope->common->count; i++)
		{
			ofc_sema_common_t* common
				= scope->common->common[i];
			if (!common) continue;

			unsigned j;
			for (j = 0; j < common->count; j++)
			{
				if (common->decl[j])
					continue;

				const ofc_sema_spec_t* spec
					= common->spec[j];
				if (!spec) return false;

				ofc_sema_spec_t* fspec
					= ofc_sema_scope_spec_find_final(
						scope, spec->name);
				if (fspec) spec = fspec;

				ofc_sema_decl_t* decl = ofc_sema_decl_spec(
					scope, spec->name, spec, NULL);
				ofc_sema_spec_delete(fspec);
				if (!decl) return false;

				if (!ofc_sema_decl_list_add(
					scope->decl, decl))
				{
					ofc_sema_decl_delete(decl);
					return false;
				}
			}
		}
	}

	/* Handle EQUIVALENCE statements */
	for (i = 0; i < body->count; i++)
	{
		ofc_parse_stmt_t* stmt = body->stmt[i];
		if (!stmt) continue;

		switch (stmt->type)
		{
			case OFC_PARSE_STMT_EQUIVALENCE:
				if (!ofc_sema_stmt_equivalence(scope, stmt))
					return false;
				break;

			default:
				break;
		}
	}

	/* TODO - Check for unused specifiers. */

	/* TODO - Check declarations exist and are used for FUNCTION arguments. */

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

ofc_sema_scope_t* ofc_sema_scope_stmt_func(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_ASSIGNMENT)
		|| !stmt->assignment
		|| !stmt->assignment->name
		|| !stmt->assignment->init)
		return NULL;

	if ((stmt->assignment->name->type != OFC_PARSE_LHS_ARRAY)
		|| !stmt->assignment->name->parent
		|| (stmt->assignment->name->parent->type != OFC_PARSE_LHS_VARIABLE))
		return NULL;

	ofc_str_ref_t base_name;
	if (!ofc_parse_lhs_base_name(
		*(stmt->assignment->name), &base_name))
		return NULL;

    ofc_sema_decl_t* decl
		= ofc_sema_scope_decl_find_modify(scope, base_name, true);
	if (decl) return NULL;

	ofc_sema_spec_t* spec
		= ofc_sema_scope_spec_find_final(
			scope, base_name);
	if (spec->array)
	{
		ofc_sema_spec_delete(spec);
		return NULL;
	}

	decl = ofc_sema_decl_function(
		scope, base_name, spec);
	ofc_sema_spec_delete(spec);
	if (!decl)
	{
		ofc_sema_scope_error(scope, stmt->src,
			"No IMPLICIT rule matches statement function name.");
		return NULL;
	}

	if (!ofc_sema_decl_list_add(
		scope->decl, decl))
	{
		ofc_sema_decl_delete(decl);
		return NULL;
	}

	ofc_sema_scope_t* func
		= ofc_sema_scope__create(
			scope, NULL, NULL, OFC_SEMA_SCOPE_STMT_FUNC);
	if (!func) return NULL;

	const ofc_parse_array_index_t* index
		= stmt->assignment->name->array.index;
	if (index && (index->count > 0))
	{
		func->args = ofc_str_ref_list_create();
		if (!func->args)
		{
			ofc_sema_scope_delete(func);
			return NULL;
		}

		unsigned i;
		for (i = 0; i < index->count; i++)
		{
			const ofc_parse_array_range_t* range
				= index->range[i];
			if (!range || range->is_slice
				|| range->last || range->stride)
			{
				ofc_sema_scope_error(scope, stmt->src,
					"Invalid argument list in statement function.");
				ofc_sema_scope_delete(func);
				return NULL;
			}

			const ofc_parse_expr_t* expr
				= range->first;
			if ((expr->type != OFC_PARSE_EXPR_VARIABLE)
				|| !expr->variable
				|| (expr->variable->type != OFC_PARSE_LHS_VARIABLE))
			{
				ofc_sema_scope_error(scope, stmt->src,
					"Statement function's argument list"
					" must only contain argument names.");
				ofc_sema_scope_delete(func);
				return NULL;
			}

			ofc_str_ref_t arg_name
				= expr->variable->variable;

			if (!ofc_str_ref_list_add(
				func->args, arg_name))
			{
				ofc_sema_scope_delete(func);
				return NULL;
			}
		}
	}

	func->expr = ofc_sema_expr(
		func, stmt->assignment->init);
	if (!func->expr)
	{
		ofc_sema_scope_delete(func);
		return NULL;
	}

	if (!ofc_sema_decl_init_func(
		decl, func))
	{
		ofc_sema_scope_delete(func);
		return NULL;
	}

	return func;
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

	if (!ofc_sema_scope__body(if_scope, block))
	{
		ofc_sema_scope_delete(if_scope);
		return NULL;
	}

	return if_scope;
}

ofc_sema_scope_t* ofc_sema_scope_implicit_do(
	ofc_sema_scope_t* scope)
{
	if (!scope)
		return NULL;

	ofc_sema_scope_t* id_scope
		= ofc_sema_scope__create(
			scope, NULL, NULL, OFC_SEMA_SCOPE_IMPLICIT_DO);
	if (!id_scope) return NULL;

	return id_scope;
}

ofc_sema_scope_t* ofc_sema_scope_block_data(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt || !scope
		|| (stmt->type != OFC_PARSE_STMT_BLOCK_DATA))
		return NULL;

	ofc_sema_scope_t* block_data
		= ofc_sema_scope__create(
			scope, NULL, NULL, OFC_SEMA_SCOPE_BLOCK_DATA);
	if (!block_data) return NULL;

	block_data->name = stmt->program.name;

	if (!ofc_sema_scope__body(
		block_data, stmt->program.body))
	{
		ofc_sema_scope_delete(block_data);
		return NULL;
	}

	if (!ofc_sema_scope__add_child(scope, block_data))
	{
		ofc_sema_scope_delete(block_data);
		return NULL;
	}

	return block_data;
}


bool ofc_sema_scope_is_root(
	const ofc_sema_scope_t* scope)
{
	switch (scope->type)
	{
		case OFC_SEMA_SCOPE_STMT_FUNC:
		case OFC_SEMA_SCOPE_IF:
		case OFC_SEMA_SCOPE_IMPLICIT_DO:
			return false;
		default:
			break;
	}

	return true;
}

ofc_sema_scope_t* ofc_sema_scope_root(
	ofc_sema_scope_t* scope)
{
	return (ofc_sema_scope_is_root(scope) ? scope
		: ofc_sema_scope_root(scope->parent));
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



static ofc_sema_spec_t* ofc_sema_scope_spec__find(
	const ofc_sema_scope_t* scope, ofc_str_ref_t name)
{
	if (!scope)
		return NULL;

	ofc_sema_spec_t* n;
	const ofc_sema_scope_t* s;
	for (s = scope, n = NULL; s && !n;
		n = ofc_hashmap_find_modify(s->spec, &name), s = s->parent);

	return n;
}

ofc_sema_spec_t* ofc_sema_scope_spec_modify(
	ofc_sema_scope_t* scope, ofc_str_ref_t name)
{
	if (!scope || ofc_str_ref_empty(name))
		return NULL;

	ofc_sema_spec_t* spec
		= ofc_hashmap_find_modify(
			scope->spec, &name);
	if (spec) return spec;

	ofc_sema_spec_t* parent
		= ofc_sema_scope_spec__find(
			scope, name);

	spec = (parent
		? ofc_sema_spec_copy(parent)
		: ofc_sema_spec_create(name));
	if (!spec) return NULL;
	spec->name = name;

	/* Don't inherit COMMON block reference. */
	spec->common = NULL;

	if (!ofc_hashmap_add(
		scope->spec, spec))
	{
		ofc_sema_spec_delete(spec);
		return NULL;
	}

	return spec;
}

ofc_sema_spec_t* ofc_sema_scope_spec_find_final(
	const ofc_sema_scope_t* scope, ofc_str_ref_t name)
{
	ofc_sema_spec_t* spec
		= ofc_sema_scope_spec__find(
			scope, name);

	return ofc_sema_implicit_apply(
		scope->implicit, name, spec);
}



const ofc_sema_decl_t* ofc_sema_scope_decl_find(
	const ofc_sema_scope_t* scope, ofc_str_ref_t name, bool local)
{
	if (!scope)
		return NULL;

	const ofc_sema_decl_t* decl
		= ofc_sema_decl_list_find(
			scope->decl, name);
	if (decl) return decl;

	if (local)
		return NULL;

	return ofc_sema_scope_decl_find(
		scope->parent, name, false);
}

ofc_sema_decl_t* ofc_sema_scope_decl_find_modify(
	ofc_sema_scope_t* scope, ofc_str_ref_t name, bool local)
{
	if (!scope)
		return NULL;

	ofc_sema_decl_t* decl
		= ofc_sema_decl_list_find_modify(
			scope->decl, name);
	if (decl) return decl;

	if (local)
		return NULL;

	return ofc_sema_scope_decl_find_modify(
		scope->parent, name, false);
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
			case OFC_SEMA_SCOPE_SUBROUTINE:
			case OFC_SEMA_SCOPE_FUNCTION:
			case OFC_SEMA_SCOPE_BLOCK_DATA:
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
		common = ofc_sema_common_map_find_modify(
			scope->common, name);
	}

	if (!common)
	{
		common = ofc_sema_common_create(name);
		if (!common) return NULL;

		if (!ofc_sema_common_map_add(
			scope->common, common))
		{
			ofc_sema_common_delete(common);
			return NULL;
		}
	}

	return common;
}

bool ofc_sema_scope_parameter_add(
	ofc_sema_scope_t* scope,
	ofc_sema_parameter_t* param)
{
	return ofc_hashmap_add(scope->parameter, param);
}



void ofc_sema_scope_error(
	const ofc_sema_scope_t* scope, ofc_str_ref_t pos,
	const char* format, ...)
{
	if (!scope)
		return;

	va_list args;
	va_start(args, format);
	ofc_sparse_error_va(scope->src, pos.base, format, args);
	va_end(args);
}

void ofc_sema_scope_warning(
	const ofc_sema_scope_t* scope, ofc_str_ref_t pos,
	const char* format, ...)
{
	if (!scope)
		return;

	va_list args;
	va_start(args, format);
	ofc_sparse_warning_va(scope->src, pos.base, format, args);
	va_end(args);
}
