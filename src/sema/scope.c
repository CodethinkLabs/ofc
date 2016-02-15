/* Copyright 2015 Codethink Ltd.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "ofc/sema.h"


void ofc_sema_scope_delete(
	ofc_sema_scope_t* scope)
{
	if (!scope)
		return;

	ofc_sema_scope_list_delete(
		scope->child);

	ofc_sema_arg_list_delete(
		scope->args);

	ofc_sema_implicit_delete(
		scope->implicit);

	ofc_sema_common_map_delete(scope->common);
	ofc_sema_spec_map_delete(scope->spec);
	ofc_sema_decl_list_delete(scope->decl);
	ofc_sema_equiv_list_delete(scope->equiv);
	ofc_sema_label_map_delete(scope->label);

	ofc_sema_structure_list_delete(scope->structure);
	ofc_sema_structure_list_delete(scope->derived_type);

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
		scope->child = ofc_sema_scope_list_create();
		if (!scope->child) return false;
	}

	return ofc_sema_scope_list_add(
		scope->child, child);
}

static ofc_sema_scope_t* ofc_sema_scope__create(
	ofc_sema_scope_t*      parent,
	const ofc_lang_opts_t* lang_opts,
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

	ofc_lang_opts_t opts = ofc_sema_scope_get_lang_opts(scope);

	scope->type        = type;
	scope->name        = OFC_STR_REF_EMPTY;
	scope->args        = NULL;

	scope->implicit = (parent
		? ofc_sema_implicit_copy(parent->implicit) : NULL);
	if (!scope->implicit)
		scope->implicit = ofc_sema_implicit_create();

	bool is_root = ofc_sema_scope_is_root(scope);

	scope->common = NULL;
	scope->spec   = NULL;
	scope->decl   = NULL;
	scope->equiv  = NULL;

	scope->structure    = NULL;
	scope->derived_type = NULL;

	bool alloc_fail = false;
	if (is_root)
	{
		scope->spec  = ofc_sema_spec_map_create(opts.case_sensitive);
		scope->decl  = ofc_sema_decl_list_create(opts.case_sensitive);
		scope->equiv = ofc_sema_equiv_list_create();

		scope->structure
			= ofc_sema_structure_list_create(opts.case_sensitive);
		scope->derived_type
			= ofc_sema_structure_list_create(opts.case_sensitive);

		alloc_fail = (!scope->spec
			|| !scope->decl
			|| !scope->equiv
			|| !scope->structure
			|| !scope->derived_type);
	}

	scope->label = ofc_sema_label_map_create();

	scope->external = false;
	scope->intrinsic = false;

	switch (scope->type)
	{
		case OFC_SEMA_SCOPE_STMT_FUNC:
			scope->expr = NULL;
			break;

		default:
			scope->stmt = NULL;
			break;
	}

	if (!scope->implicit
		|| alloc_fail
		|| !scope->label)
	{
		ofc_sema_scope_delete(scope);
		return NULL;
	}

	return scope;
}



static bool ofc_sema_scope__body_scan_format(
	const ofc_parse_stmt_t* stmt,
	ofc_sema_scope_t* scope)
{
	if (!stmt) return false;
	return ((stmt->type != OFC_PARSE_STMT_FORMAT)
		|| ofc_sema_format(scope, stmt));
}

static bool ofc_sema_scope__body_scan_equivalence(
	const ofc_parse_stmt_t* stmt,
	ofc_sema_scope_t* scope)
{
	if (!stmt) return false;
	return ((stmt->type != OFC_PARSE_STMT_EQUIVALENCE)
		|| ofc_sema_stmt_equivalence(scope, stmt));
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
	if (!ofc_parse_stmt_list_foreach(body, scope,
		(void*)ofc_sema_scope__body_scan_format))
		return false;

	if (scope->stmt)
		return false;

	scope->stmt = ofc_sema_stmt_list(scope, body);
	if (!scope->stmt) return false;

	/* Declare arguments */
	if (scope->args)
	{
		unsigned i;
		for (i = 0; i < scope->args->count; i++)
		{
			if (scope->args->arg[i].alt_return)
				continue;

			ofc_sparse_ref_t name
				= scope->args->arg[i].name;
			const ofc_sema_decl_t* exist
				= ofc_sema_scope_decl_find(
					scope, name.string, true);
			if (exist) continue;

			ofc_sema_spec_t* spec
				= ofc_sema_scope_spec_find_final(
					scope, name);
			ofc_sema_decl_t* decl = ofc_sema_decl_spec(
				scope, name, spec, NULL);
			ofc_sema_spec_delete(spec);
			if (!decl) return false;
		}
	}

	/* Declare unused specifiers which exist in COMMON blocks. */
	if (scope->common)
	{
		unsigned i;
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

				ofc_sema_spec_t* spec
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
			}
		}
	}

	/* Handle EQUIVALENCE statements */
	if (!ofc_parse_stmt_list_foreach(body, scope,
		(void*)ofc_sema_scope__body_scan_equivalence))
		return false;

	/* TODO - Check for unused specifiers. */

	/* TODO - Check declarations exist and are used for FUNCTION arguments. */

	return true;
}


bool ofc_sema_scope_subroutine(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_SUBROUTINE))
		return false;

	ofc_sparse_ref_t name = stmt->program.name;
	if (ofc_sparse_ref_empty(name))
		return false;

	ofc_sema_decl_t* decl
		= ofc_sema_scope_decl_find_modify(
			scope, name.string, true);
	if (decl)
	{
		if (!ofc_sema_decl_is_subroutine(decl))
		{
			ofc_sparse_ref_error(stmt->src,
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

		if (!ofc_sema_scope_decl_add(
			scope, decl))
		{
			ofc_sema_decl_delete(decl);
			return false;
		}
	}


	ofc_sema_scope_t* sub_scope
		= ofc_sema_scope__create(scope, NULL,
			OFC_SEMA_SCOPE_SUBROUTINE);
	if (!sub_scope) return false;
	sub_scope->name = name.string;

	if (stmt->program.args)
	{
		sub_scope->args = ofc_sema_arg_list(
			stmt->program.args);
		if (!sub_scope->args)
		{
			ofc_sema_scope_delete(sub_scope);
			return false;
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

bool ofc_sema_scope_function(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_FUNCTION))
		return false;

	ofc_sparse_ref_t name = stmt->program.name;
	if (ofc_sparse_ref_empty(name))
		return false;

	ofc_sema_scope_t* func_scope
		= ofc_sema_scope__create(scope, NULL,
			OFC_SEMA_SCOPE_FUNCTION);
	if (!func_scope) return false;
	func_scope->name = name.string;

	if (stmt->program.args)
	{
		func_scope->args = ofc_sema_arg_list(
			stmt->program.args);
		if (!func_scope->args)
		{
			ofc_sema_scope_delete(func_scope);
			return false;
		}
	}


	if (stmt->program.type)
	{
		ofc_sema_spec_t* spec
			= ofc_sema_spec(func_scope,
				stmt->program.type);
		if (!spec)
		{
			ofc_sema_scope_delete(func_scope);
			return false;
		}

		if (ofc_sema_spec_is_dynamic_array(spec))
		{
			ofc_sparse_ref_error(stmt->src,
				"FUNCTION return type cannot be a dynamically sized array");
			ofc_sema_spec_delete(spec);
			ofc_sema_scope_delete(func_scope);
			return NULL;
		}

		spec->name = stmt->program.name;

		if (!ofc_sema_spec_map_add(
			func_scope->spec, spec))
		{
			ofc_sema_spec_delete(spec);
			ofc_sema_scope_delete(func_scope);
			return false;
		}
	}

	if (!ofc_sema_scope__body(
		func_scope, stmt->program.body))
	{
		ofc_sema_scope_delete(func_scope);
		return false;
	}

	ofc_sema_spec_t* spec
		= ofc_sema_scope_spec_find_final(
			func_scope, stmt->program.name);
	if (!spec)
	{
		ofc_sparse_ref_error(stmt->src,
			"No IMPLICIT type matches FUNCTION name");
		ofc_sema_scope_delete(func_scope);
		return false;
	}
	const ofc_sema_type_t* rtype
		= ofc_sema_type_spec(spec);
	ofc_sema_spec_delete(spec);
	if (!rtype)
	{
		ofc_sema_scope_delete(func_scope);
		return false;
	}

	const ofc_sema_type_t* ftype
		= ofc_sema_type_create_function(rtype);
	if (!ftype)
	{
		ofc_sema_scope_delete(func_scope);
		return false;
	}

	ofc_sema_decl_t* decl
		= ofc_sema_scope_decl_find_modify(
			scope, name.string, true);
	if (decl)
	{
		if (!ofc_sema_decl_is_function(decl))
		{
			ofc_sparse_ref_error(stmt->src,
				"Can't redeclare used variable as FUNCTION");
			ofc_sema_scope_delete(func_scope);
			return false;
		}
		else if (!ofc_sema_type_compare(rtype,
			ofc_sema_decl_base_type(decl)))
		{
			ofc_sparse_ref_error(stmt->src,
				"Conflicting definitions of FUNCTION return type");
			ofc_sema_scope_delete(func_scope);
			return false;
		}
	}
	else
	{
		decl = ofc_sema_decl_create(ftype, name);
		if (!decl)
		{
			ofc_sema_scope_delete(func_scope);
			return false;
		}

		if (!ofc_sema_scope_decl_add(
			scope, decl))
		{
			ofc_sema_decl_delete(decl);
			ofc_sema_scope_delete(func_scope);
			return false;
		}
	}

	if (!ofc_sema_decl_init_func(
		decl, func_scope))
	{
		ofc_sema_scope_delete(func_scope);
		return false;
	}

	return true;
}

ofc_sema_scope_t* ofc_sema_scope_global(
	const ofc_lang_opts_t* lang_opts,
	const ofc_parse_stmt_list_t* list)
{
	if (!list)
		return NULL;

	ofc_sema_scope_t* scope
		= ofc_sema_scope__create(
			NULL, lang_opts, OFC_SEMA_SCOPE_GLOBAL);
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
			scope, NULL, OFC_SEMA_SCOPE_PROGRAM);
	if (!program) return NULL;

	program->name = stmt->program.name.string;

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

	ofc_sparse_ref_t base_name;
	if (!ofc_parse_lhs_base_name(
		*(stmt->assignment->name), &base_name))
		return NULL;

	ofc_sema_decl_t* decl
		= ofc_sema_scope_decl_find_modify(
			scope, base_name.string, true);
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
		ofc_sparse_ref_error(stmt->src,
			"No IMPLICIT rule matches statement function name.");
		return NULL;
	}

	ofc_sema_scope_t* func
		= ofc_sema_scope__create(
			scope, NULL, OFC_SEMA_SCOPE_STMT_FUNC);
	if (!func) return NULL;

	const ofc_parse_array_index_t* index
		= stmt->assignment->name->array.index;
	if (index && (index->count > 0))
	{
		func->args = ofc_sema_arg_list_stmt_func(index);
		if (!func->args)
		{
			ofc_sema_scope_delete(func);
			return false;
		}

		/* Mark specifiers for stmt func arguments as used. */
		unsigned i;
		for (i = 0; i < func->args->count; i++)
		{
			ofc_sema_spec_t* spec
				= ofc_sema_scope_spec_modify(
					scope, func->args->arg[i].name);
			ofc_sema_spec_mark_used(scope, spec);
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

ofc_sema_scope_t* ofc_sema_scope_block_data(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt || !scope
		|| (stmt->type != OFC_PARSE_STMT_BLOCK_DATA))
		return NULL;

	ofc_sema_scope_t* block_data
		= ofc_sema_scope__create(
			scope, NULL, OFC_SEMA_SCOPE_BLOCK_DATA);
	if (!block_data) return NULL;

	block_data->name = stmt->program.name.string;

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


bool ofc_sema_scope_is_procedure(
	const ofc_sema_scope_t* scope)
{
	if (!scope)
		return false;

	switch (scope->type)
	{
		case OFC_SEMA_SCOPE_STMT_FUNC:
		case OFC_SEMA_SCOPE_SUBROUTINE:
		case OFC_SEMA_SCOPE_FUNCTION:
			return true;
		default:
			break;
	}
	return false;
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



const ofc_sema_label_t* ofc_sema_scope_label_find(
	const ofc_sema_scope_t* scope, unsigned label)
{
	if (!scope)
		return NULL;

	const ofc_sema_label_t* l
		=ofc_sema_label_map_find(
			scope->label, label);
	if (l) return l;

	if (!ofc_sema_scope_is_root(scope))
		return ofc_sema_scope_label_find(
			scope->parent, label);

	return NULL;
}


static ofc_sema_spec_t* ofc_sema_scope_spec__find(
	const ofc_sema_scope_t* scope, ofc_str_ref_t name)
{
	if (!scope)
		return NULL;

	if (!scope->spec)
		return ofc_sema_scope_spec__find(
			scope->parent, name);

	ofc_sema_spec_t* n;
	const ofc_sema_scope_t* s;
	for (s = scope, n = NULL; s && !n;
		n = ofc_hashmap_find_modify(s->spec->map, &name), s = s->parent);

	return n;
}

ofc_sema_spec_t* ofc_sema_scope_spec_modify(
	ofc_sema_scope_t* scope, ofc_sparse_ref_t name)
{
	if (!scope || ofc_sparse_ref_empty(name))
		return NULL;

	if (!scope->spec)
		return ofc_sema_scope_spec_modify(
			scope->parent, name);

	ofc_sema_spec_t* spec
		= ofc_hashmap_find_modify(
			scope->spec->map, &name.string);
	if (spec) return spec;

	ofc_sema_spec_t* parent
		= ofc_sema_scope_spec__find(
			scope, name.string);

	spec = (parent
		? ofc_sema_spec_copy(parent)
		: ofc_sema_spec_create(name));
	if (!spec) return NULL;
	spec->name = name;

	/* Don't inherit COMMON block reference. */
	spec->common = NULL;

	if (!ofc_sema_spec_map_add(
		scope->spec, spec))
	{
		ofc_sema_spec_delete(spec);
		return NULL;
	}

	return spec;
}

ofc_sema_spec_t* ofc_sema_scope_spec_find_final(
	const ofc_sema_scope_t* scope, ofc_sparse_ref_t name)
{
	ofc_sema_spec_t* spec
		= ofc_sema_scope_spec__find(
			scope, name.string);

	return ofc_sema_implicit_apply(
		scope->implicit, name, spec);
}



bool ofc_sema_scope_equiv_add(
	ofc_sema_scope_t* scope, ofc_sema_equiv_t* equiv)
{
	if (!scope)
		return false;

	if (!scope->equiv)
		return ofc_sema_scope_equiv_add(
			scope->parent, equiv);

	return ofc_sema_equiv_list_add(
		scope->equiv, equiv);
}



bool ofc_sema_scope_decl_add(
	ofc_sema_scope_t* scope, ofc_sema_decl_t* decl)
{
	if (!scope || !decl)
		return false;

	if (!scope->decl)
		return ofc_sema_scope_decl_add(
			scope->parent, decl);

	return ofc_sema_decl_list_add(
		scope->decl, decl);
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


bool ofc_sema_scope_derived_type_add(
	ofc_sema_scope_t* scope,
	ofc_sema_structure_t* structure)
{
	if (!scope)
		return NULL;

	if (!scope->derived_type)
		return ofc_sema_scope_derived_type_add(
			scope->parent, structure);

	return ofc_sema_structure_list_add(
		scope->derived_type, structure);
}

ofc_sema_structure_t* ofc_sema_scope_derived_type_find(
	ofc_sema_scope_t* scope, ofc_str_ref_t name)
{
	if (!scope)
		return NULL;

	ofc_sema_structure_t* structure
		= ofc_sema_structure_list_find_modify(
			scope->derived_type, name);
	if (structure) return structure;

	return ofc_sema_scope_derived_type_find(
		scope->parent, name);
}

bool ofc_sema_scope_structure_add(
	ofc_sema_scope_t* scope,
	ofc_sema_structure_t* structure)
{
	if (!scope)
		return NULL;

	if (!scope->structure)
		return ofc_sema_scope_structure_add(
			scope->parent, structure);

	return ofc_sema_structure_list_add(
		scope->structure, structure);
}

ofc_sema_structure_t* ofc_sema_scope_structure_find(
	ofc_sema_scope_t* scope, ofc_str_ref_t name)
{
	if (!scope)
		return NULL;

	ofc_sema_structure_t* structure
		= ofc_sema_structure_list_find_modify(
			scope->structure, name);
	if (structure) return structure;

	return ofc_sema_scope_derived_type_find(
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
		if (!ofc_sema_scope_is_root(scope))
			return ofc_sema_scope_common_find_create(
				scope->parent, name);

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


static bool ofc_sema_scope_child__print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_sema_scope_list_t* list)
{
	if (!cs || !list)
		return false;

    unsigned i;
	for (i = 0; i < list->count; i++)
	{
		if (!ofc_sema_scope_print(cs, indent, list->scope[i]))
			return false;
	}
	return true;
}

static bool ofc_sema_scope_body__print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_sema_scope_t* scope)
{
	if (!cs || !scope)
		return false;

	switch (scope->type)
	{
		case OFC_SEMA_SCOPE_GLOBAL:
		case OFC_SEMA_SCOPE_STMT_FUNC:
			break;
		default:
			if (!ofc_colstr_newline(cs, indent, NULL)
				|| !ofc_colstr_atomic_writef(cs, "IMPLICIT")
				|| !ofc_colstr_atomic_writef(cs, " ")
				|| !ofc_colstr_atomic_writef(cs, "NONE"))
				return false;
			break;
	}

	/* TODO - Use scope error printing preferably with source location. */

	if (scope->structure && !ofc_sema_structure_list_print(
		cs, indent, scope->structure))
	{
		ofc_file_error(NULL, NULL,
			"Failed to print structure list");
		return false;
	}
	if (scope->derived_type && !ofc_sema_structure_list_print(
		cs, indent, scope->derived_type))
	{
		ofc_file_error(NULL, NULL,
			"Failed to print derived type list");
		return false;
	}

	if (scope->spec
		&& !ofc_sema_spec_list_print(cs, indent, scope, scope->spec->list))
	{
		ofc_file_error(NULL, NULL,
			"Failed to print spec list");
		return false;
	}
	if (scope->decl
		&& !ofc_sema_decl_list_print(cs, indent, scope->decl))
	{
		ofc_file_error(NULL, NULL,
			"Failed to print decl list");
		return false;
	}

	if (scope->decl && (scope->type != OFC_SEMA_SCOPE_GLOBAL)
		&& !ofc_sema_decl_list_procedure_spec_print(cs, indent, scope->decl))
	{
		ofc_file_error(NULL, NULL,
			"Failed to print procedure specifier list");
		return false;
	}

	if (scope->common
		&& !ofc_sema_common_map_print(cs, indent, scope->common))
	{
		ofc_file_error(NULL, NULL,
			"Failed to print common map");
		return false;
	}

	if (scope->equiv
		&& !ofc_sema_equiv_list_print(cs, indent, scope->equiv))
	{
		ofc_file_error(NULL, NULL,
			"Failed to print equiv list");
		return false;
	}

	if (scope->decl
		&& !ofc_sema_decl_list_stmt_func_print(cs, indent, scope->decl))
	{
		ofc_file_error(NULL, NULL,
			"Failed to print stmt func list");
		return false;
	}

	if (scope->stmt
		&& !ofc_sema_stmt_list_print(cs, indent, scope->label, scope->stmt))
	{
		ofc_file_error(NULL, NULL,
			"Failed to print stmt list");
		return false;
	}
	if (scope->label
		&& !ofc_sema_format_label_list_print(cs, indent, scope->label->format))
	{
		ofc_file_error(NULL, NULL,
			"Failed to print format label list");
		return false;
	}

	return true;
}

bool ofc_sema_scope_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_sema_scope_t* scope)
{
	if (!scope)
		return false;

	const char* kwstr = NULL;
	switch (scope->type)
	{
		case OFC_SEMA_SCOPE_GLOBAL:
			break;
		case OFC_SEMA_SCOPE_PROGRAM:
			kwstr = "PROGRAM";
			break;
		case OFC_SEMA_SCOPE_SUBROUTINE:
			kwstr = "SUBROUTINE";
			break;
		case OFC_SEMA_SCOPE_FUNCTION:
			kwstr = "FUNCTION";
			break;
		case OFC_SEMA_SCOPE_BLOCK_DATA:
			kwstr = "BLOCK DATA";
			break;
		case OFC_SEMA_SCOPE_STMT_FUNC:
			return ofc_sema_expr_print(cs, scope->expr);

		default:
			return false;
	}

	if (kwstr)
	{
		if (scope->type != OFC_SEMA_SCOPE_FUNCTION)
		{
			/* Decl function printing will handle the new lines
			   as we need to specify the return type. */
			if (!ofc_colstr_newline(cs, indent, NULL))
				return false;
		}

		if (!ofc_colstr_atomic_writef(cs, "%s ", kwstr))
			return false;

		if (scope->name.base)
		{
			if (!ofc_colstr_atomic_writef(cs, "%.*s",
				scope->name.size, scope->name.base))
					return false;
		}

		switch (scope->type)
		{
			case OFC_SEMA_SCOPE_FUNCTION:
			case OFC_SEMA_SCOPE_SUBROUTINE:
			case OFC_SEMA_SCOPE_STMT_FUNC:
				if (!ofc_colstr_atomic_writef(cs, "("))
					return false;
				if (scope->args && !ofc_sema_arg_list_print(cs, scope->args))
					return false;
				if (!ofc_colstr_atomic_writef(cs, ")"))
					return false;
				break;
			default:
				break;
		}
	}

	if (!ofc_sema_scope_body__print(
		cs, (indent + 1), scope))
		return false;

	if (kwstr)
	{
		if (!ofc_colstr_newline(cs, indent, NULL)
			|| !ofc_colstr_atomic_writef(cs, "END %s ", kwstr))
			return false;

		if (scope->name.base)
		{
			if (!ofc_colstr_atomic_writef(cs, "%.*s",
				scope->name.size, scope->name.base))
					return false;
		}
	}

	if (scope->child
		&& !ofc_sema_scope_child__print(cs, indent, scope->child))
		return false;

	if (!ofc_sema_decl_list_procedure_print(cs, indent, scope->decl))
	{
		ofc_file_error(NULL, NULL,
			"Failed to print procedure list");
		return false;
	}

	return true;
}

ofc_sema_scope_list_t* ofc_sema_scope_list_create(void)
{
	ofc_sema_scope_list_t* list
		= (ofc_sema_scope_list_t*)malloc(
			sizeof(ofc_sema_scope_list_t));
	if (!list) return NULL;

	list->scope = NULL;
	list->count = 0;

	return list;
}

bool ofc_sema_scope_list_add(
	ofc_sema_scope_list_t* list,
	ofc_sema_scope_t* scope)
{
	if (!list || !scope) return false;

	ofc_sema_scope_t** nscope
		= (ofc_sema_scope_t**)realloc(list->scope,
			(sizeof(ofc_sema_scope_t*) * (list->count + 1)));
	if (!nscope) return false;
	list->scope = nscope;

	list->scope[list->count++] = scope;
	return true;
}

bool ofc_sema_scope_list_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_sema_scope_list_t* list)
{
	if (!list)
		return false;

	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		if (!ofc_sema_scope_print(cs,
			indent, list->scope[i]))
			return false;
	}
	return true;
}

void ofc_sema_scope_list_delete(
	ofc_sema_scope_list_t* list)
{
	if (!list)
		return;

	unsigned i;
	for (i = 0; i < list->count; i++)
		ofc_sema_scope_delete(list->scope[i]);

	free(list->scope);
	free(list);
}
