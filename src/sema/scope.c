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
#include "ofc/global_opts.h"

extern ofc_global_opts_t global_opts;

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
	ofc_sema_decl_list_delete(scope->decl);
	ofc_sema_equiv_list_delete(scope->equiv);
	ofc_sema_label_map_delete(scope->label);
	ofc_sema_external_list_delete(scope->external);

	ofc_sema_structure_list_delete(scope->structure);
	ofc_sema_structure_list_delete(scope->derived_type);

	ofc_sema_module_list_delete(scope->module);

	ofc_sema_expr_delete(scope->expr);
	ofc_sema_stmt_list_delete(scope->stmt);

	ofc_parse_file_delete(scope->file);

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
	ofc_sema_scope_t* parent,
	ofc_sema_scope_e  type)
{
	ofc_sema_scope_t* scope
		= (ofc_sema_scope_t*)malloc(
			sizeof(ofc_sema_scope_t));
	if (!scope) return NULL;

	scope->file = NULL;
	scope->src  = OFC_SPARSE_REF_EMPTY;

	scope->parent = parent;
	scope->child  = NULL;

	scope->type = type;
	scope->name = OFC_STR_REF_EMPTY;
	scope->args = NULL;

	scope->module = NULL;

	scope->attr_external = false;
	scope->attr_intrinsic = false;
	scope->attr_save = false;
	scope->attr_recursive = false;

	scope->contains_automatic = false;

	scope->access = OFC_SEMA_ACCESSIBILITY_DEFAULT;

	scope->decl = NULL;

	scope->implicit = NULL;

	scope->common = NULL;
	scope->equiv  = NULL;

	scope->structure    = NULL;
	scope->derived_type = NULL;

	scope->label = NULL;

	scope->expr = NULL;
	scope->stmt = NULL;

	scope->external = NULL;

	if (scope->type == OFC_SEMA_SCOPE_SUPER)
		return scope;

	scope->decl = ofc_sema_decl_list_create(
		global_opts.case_sensitive);

	scope->external = ofc_sema_external_list_create(
		global_opts.case_sensitive);

	bool alloc_fail = !scope->decl;
	if (scope->type != OFC_SEMA_SCOPE_STMT_FUNC)
	{
		scope->implicit = (parent
			? ofc_sema_implicit_copy(parent->implicit) : NULL);
		if (!scope->implicit)
			scope->implicit = ofc_sema_implicit_create();

		scope->common = ofc_sema_common_map_create(
			global_opts.case_sensitive);
		scope->equiv  = ofc_sema_equiv_list_create();

		scope->structure
			= ofc_sema_structure_list_create(
				global_opts.case_sensitive);
		scope->derived_type
			= ofc_sema_structure_list_create(
				global_opts.case_sensitive);

		scope->label = ofc_sema_label_map_create();

		if (!scope->implicit
			|| !scope->common
			|| !scope->equiv
			|| !scope->structure
			|| !scope->derived_type
			|| !scope->label)
			alloc_fail = true;
	}

	if (alloc_fail)
	{
		ofc_sema_scope_delete(scope);
		return NULL;
	}

	return scope;
}



static bool ofc_sema_scope__body_sema_equivalence(
	const ofc_parse_stmt_t* stmt,
	ofc_sema_scope_t* scope)
{
	if (!stmt) return false;
	return ((stmt->type != OFC_PARSE_STMT_EQUIVALENCE)
		|| ofc_sema_stmt_equivalence(scope, stmt));
}

static bool ofc_sema_scope__body_label_resolve(
	ofc_sema_expr_t* expr,
	ofc_sema_scope_t* scope)
{
	if (!expr) return false;

	if (!expr->is_label || expr->label)
		return true;

	if (ofc_sema_expr_is_constant(expr))
	{
		unsigned ulabel;
		if (!ofc_sema_expr_resolve_uint(
			expr, &ulabel))
		{
			ofc_sparse_ref_error(expr->src,
				"Invalid label constant");
			return false;
		}

		ofc_sema_label_t* label
			= ofc_sema_scope_label_modify(
				scope, ulabel);
		if (!label)
		{
			ofc_sparse_ref_error(expr->src,
				"Label does not exist in current scope");
			return false;
		}

		if ((label->type == OFC_SEMA_LABEL_STMT)
			&& label->stmt && !expr->is_format
			&& (label->stmt->type == OFC_SEMA_STMT_IO_FORMAT))
		{
			ofc_sparse_ref_warning(expr->src,
				"Jumping to a FORMAT statement");
		}

		expr->label = label;
		label->used = true;
	}

	return true;
}

static bool ofc_sema_scope__body_format_validate(
	ofc_sema_stmt_t* stmt,
	ofc_sema_scope_t* scope)
{
	(void)scope;

	if (!stmt) return false;

	switch (stmt->type)
	{
		case OFC_SEMA_STMT_IO_WRITE:
		case OFC_SEMA_STMT_IO_READ:
		case OFC_SEMA_STMT_IO_PRINT:
			return ofc_sema_stmt_io_format_validate(stmt);
		default:
			break;
	}

	return true;
}

static bool ofc_sema_scope__body_format_validate_defaults_against(
	ofc_sema_stmt_t* stmt,
	ofc_sema_stmt_t* format_stmt)
{
	(void)format_stmt;

	if (!stmt) return false;

	switch (stmt->type)
	{
		case OFC_SEMA_STMT_IO_WRITE:
		case OFC_SEMA_STMT_IO_READ:
		case OFC_SEMA_STMT_IO_PRINT:
			return ofc_sema_stmt_io_format_validate_defaults(stmt, format_stmt);
		default:
			break;
	}

	return true;
}

static bool ofc_sema_scope__body_format_validate_defaults(
	ofc_sema_stmt_t* stmt,
	ofc_sema_scope_t* scope)
{
	(void)scope;

	if (!stmt) return false;

	switch (stmt->type)
	{
		case OFC_SEMA_STMT_IO_FORMAT:
			/* Validate FORMAT descriptors defaults. */
			return ofc_sema_stmt_list_foreach(scope->stmt, stmt,
				(void*)ofc_sema_scope__body_format_validate_defaults_against);
		default:
			break;
	}

	return true;
}

static bool ofc_sema_scope__body_validate(
	ofc_sema_scope_t* scope)
{
	if (!scope)
		return false;

	/* Warn about unused declarations. */
	if (scope->decl)
	{
		unsigned i;
		for (i = 0; i < scope->decl->count; i++)
		{
			const ofc_sema_decl_t* decl
				= scope->decl->decl_ref[i];
			if (!decl || decl->common
				|| ofc_sparse_ref_empty(decl->name)
				|| ofc_sema_decl_is_parameter(decl)
				|| ofc_sema_decl_is_common(decl))
				continue;

			/* TODO - Warn properly about unreferenced procedures. */
			if(ofc_sema_decl_is_procedure(decl))
				continue;

			/* Statement function arguments are always used. */
			if (decl->is_stmt_func_arg)
				continue;

			if (decl->is_argument)
			{
				if (!decl->was_read)
				{
					ofc_sparse_ref_warning(decl->name,
						"Argument '%.*s' not used",
						decl->name.string.size,
						decl->name.string.base);
				}
			}
			else if (decl->is_return)
			{
				if (!decl->was_written)
				{
					ofc_sparse_ref_warning(decl->name,
						"FUNCTION '%.*s' provides no return value",
						decl->name.string.size,
						decl->name.string.base);
				}
			}
			else if (!decl->was_written && !decl->was_read)
			{
				ofc_sparse_ref_warning(decl->name,
					"Variable '%.*s' declared but not used",
					decl->name.string.size,
					decl->name.string.base);
			}
			else if (!decl->was_written)
			{
				if (ofc_sema_decl_is_initialized(decl, NULL))
				{
					/* TODO - Advise that decl could be PARAMETER. */
				}
				else
				{
					ofc_sparse_ref_warning(decl->name,
						"Variable '%.*s' read but never written",
						decl->name.string.size,
						decl->name.string.base);
				}
			}
			else if (!decl->was_read)
			{
				ofc_sparse_ref_warning(decl->name,
					"Variable '%.*s' written but never read",
					decl->name.string.size,
					decl->name.string.base);
			}
		}
	}

	return true;
}

bool ofc_sema_scope__body_decl_finalize(
	ofc_sema_decl_t* decl, void* param)
{
	(void)param;

	if (!decl)
		return true;

	if (ofc_sema_decl_is_unknown_external(decl))
	{
		ofc_sparse_ref_warning(decl->name,
			"External symbol is not explicitly typed after IMPLICIT NONE.");
		return true;
	}

	return ofc_sema_decl_type_finalize(decl);
}

static bool ofc_sema_scope__body(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_list_t* body)
{
	if (scope->type == OFC_SEMA_SCOPE_STMT_FUNC)
		return false;

	if (!body)
		return true;

	if (scope->stmt)
		return false;

	scope->stmt = ofc_sema_stmt_list(scope, NULL, body);
	if (!scope->stmt)
		return false;

	/* Finalize declarations. */
	if (!ofc_sema_scope_foreach_decl(scope, NULL,
		(void*)ofc_sema_scope__body_decl_finalize))
		return false;

	/* Resolve labels */
	if (!ofc_sema_stmt_list_foreach_expr(scope->stmt, scope,
		(void*)ofc_sema_scope__body_label_resolve))
		return false;

	/* Warn about unused labels. */
	if (scope->label)
	{
		unsigned i;
		for (i = 0; i < scope->label->count; i++)
		{
			ofc_sema_label_t* label
				= scope->label->label[i];
			if (!label) continue;

			if (!label->used)
			{
				/* TODO - Get position of actual label. */
				ofc_sparse_ref_warning(ofc_sema_label_src(label),
					"Label %u is defined but not used", label->number);
			}
		}
	}

	/* Validate FORMAT descriptors. */
	if (!ofc_sema_stmt_list_foreach(scope->stmt, scope,
		(void*)ofc_sema_scope__body_format_validate))
		return false;

	/* Validate FORMAT descriptors defaults. */
	if (!ofc_sema_stmt_list_foreach(scope->stmt, scope,
		(void*)ofc_sema_scope__body_format_validate_defaults))
		return false;

	/* Handle EQUIVALENCE statements */
	if (!ofc_parse_stmt_list_foreach(body, scope,
		(void*)ofc_sema_scope__body_sema_equivalence))
		return false;

	return ofc_sema_scope__body_validate(scope);
}

bool ofc_sema_scope__check_namespace_collision(
	ofc_sema_scope_t* scope,
	const char* name_space, ofc_sparse_ref_t ref)
{
	if (!scope)
		return false;

	bool collision = false;

	if (!global_opts.no_warn_name_keyword)
	{
		bool space, is;
		if (ofc_sparse_ref_begins_with_keyword(ref, &space, &is))
		{
			if (is)
			{
				ofc_sparse_ref_warning(ref,
					"Symbol name is a language keyword");
			}
			else if (space || global_opts.warn_name_keyword_all)
			{
				ofc_sparse_ref_warning(ref,
					"Symbol name begins with language keyword");
			}
		}
	}

	if (ofc_sema_scope_common_name_exists(scope, ref.string))
	{
		if (!global_opts.no_warn_namespace_col)
		{
			ofc_sparse_ref_warning(ref,
				"%s name '%.*s' conflicts with a common block",
				name_space, ref.string.size, ref.string.base);
		}

		collision = true;
	}

	if (ofc_sema_scope_block_data_name_exists(scope, ref.string))
	{
		if (!global_opts.no_warn_namespace_col)
		{
			ofc_sparse_ref_warning(ref,
				"T%s name '%.*s' conflicts with a block data",
				name_space, ref.string.size, ref.string.base);
		}

		collision = true;
	}

	if (ofc_sema_scope_structure_name_exists(scope, ref.string))
	{
		if (!global_opts.no_warn_namespace_col)
		{
			ofc_sparse_ref_warning(ref,
				"%s name '%.*s' conflicts with a structure",
				name_space, ref.string.size, ref.string.base);
		}

		collision = true;
	}

	if (ofc_sema_scope_decl_find(scope, ref.string, true))
	{
		if (!global_opts.no_warn_namespace_col)
		{
			ofc_sparse_ref_warning(ref,
				"%s name '%.*s' conflicts with a declaration",
				name_space, ref.string.size, ref.string.base);
		}

		collision = true;
	}

	char* name_str = strndup(ref.string.base, ref.string.size);

	if (ofc_sema_intrinsic_name_reserved(name_str))
	{
		if (!global_opts.no_warn_namespace_col)
		{
			ofc_sparse_ref_warning(ref,
				"%s name '%.*s' conflicts with a reserved intrinsic keyword",
				name_space, ref.string.size, ref.string.base);
		}

		collision = true;
	}

	free (name_str);
	return collision;
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
		= ofc_sema_scope_decl_find_create_ns(
			scope, name, true, "Subroutine");
	if (!decl) return false;

	if (!ofc_sema_decl_subroutine(decl))
	{
		ofc_sparse_ref_error(stmt->src,
			"Can't redefine declaration as SUBROUTINE");
		return false;
	}

	ofc_sema_scope_t* sub_scope
		= ofc_sema_scope__create(scope,
			OFC_SEMA_SCOPE_SUBROUTINE);
	if (!sub_scope) return false;
	sub_scope->src  = stmt->src;
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

	if (stmt->program.end_has_label
		&& !ofc_sema_label_map_add_end_scope(
			sub_scope->label, stmt->program.end_label, sub_scope))
	{
		ofc_sema_scope_delete(sub_scope);
		return false;
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

	const ofc_sema_type_t* type = NULL;
	if (stmt->program.type)
	{
		type = ofc_sema_type(
			scope, stmt->program.type, NULL);
		if (!type) return false;
	}

	ofc_sema_scope_t* func_scope
		= ofc_sema_scope__create(scope,
			OFC_SEMA_SCOPE_FUNCTION);
	if (!func_scope) return false;
	func_scope->src  = stmt->src;
	func_scope->name = name.string;

	ofc_sema_decl_t* rdecl
		= ofc_sema_scope_decl_find_create_ns(
			func_scope, name, true, NULL);
	if (!rdecl)
	{
		ofc_sema_scope_delete(func_scope);
		return false;
	}

	if (type) rdecl->type = type;

	rdecl->is_return = true;

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

	if (stmt->program.end_has_label
		&& !ofc_sema_label_map_add_end_scope(
			func_scope->label, stmt->program.end_label, func_scope))
	{
		ofc_sema_scope_delete(func_scope);
		return false;
	}

	if (!ofc_sema_scope__body(
		func_scope, stmt->program.body))
	{
		ofc_sema_scope_delete(func_scope);
		return false;
	}

	if (!ofc_sema_decl_type_finalize(rdecl))
	{
		ofc_sparse_ref_error(stmt->src,
			"No IMPLICIT type matches FUNCTION name");
		ofc_sema_scope_delete(func_scope);
		return false;
	}

	ofc_sema_decl_t* fdecl
		= ofc_sema_scope_decl_find_create_ns(
			scope, name, true, "Function");
	if (!fdecl)
	{
		ofc_sema_scope_delete(func_scope);
		return false;
	}

	if (!ofc_sema_decl_type_set(
		fdecl, rdecl->type, name))
	{
		ofc_sparse_ref_error(stmt->src,
			"Conflicting definitions of FUNCTION return type");
		ofc_sema_scope_delete(func_scope);
		return false;
	}

	if (!ofc_sema_decl_function(fdecl))
	{
		ofc_sparse_ref_error(stmt->src,
			"Can't redeclare used variable as FUNCTION");
		ofc_sema_scope_delete(func_scope);
		return false;
	}

	if (!ofc_sema_decl_init_func(
		fdecl, func_scope))
	{
		ofc_sema_scope_delete(func_scope);
		return false;
	}

	return true;
}

ofc_sema_scope_t* ofc_sema_scope_super(void)
{
	return ofc_sema_scope__create(
		NULL, OFC_SEMA_SCOPE_SUPER);
}

ofc_sema_scope_t* ofc_sema_scope_global(
	ofc_sema_scope_t* super,
	ofc_parse_file_t* file)
{
	if (!file)
		return NULL;

	ofc_sema_scope_t* scope
		= ofc_sema_scope__create(
			super, OFC_SEMA_SCOPE_GLOBAL);
	if (!scope) return NULL;

	const ofc_parse_stmt_list_t* list = file->stmt;
	if (!ofc_sema_scope__body(scope, list))
	{
		ofc_sema_scope_delete(scope);
		return NULL;
	}

	if (super && !ofc_sema_scope__add_child(super, scope))
	{
		ofc_sema_scope_delete(scope);
		return NULL;
	}

	if (list && (list->count > 0)
		&& list->stmt[0])
	{
		if (list->count == 1)
		{
			scope->src = list->stmt[0]->src;
		}
		else if (list->stmt[list->count - 1])
		{
			ofc_sparse_ref_bridge(
				list->stmt[0]->src,
				list->stmt[list->count - 1]->src,
				&scope->src);
		}
	}

	scope->file = file;
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
			scope, OFC_SEMA_SCOPE_PROGRAM);
	if (!program) return NULL;

	program->src  = stmt->src;
	program->name = stmt->program.name.string;

	if (stmt->program.end_has_label
		&& !ofc_sema_label_map_add_end_scope(
			program->label, stmt->program.end_label, program))
	{
		ofc_sema_scope_delete(program);
		return false;
	}

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

	ofc_sema_decl_t* edecl
		= ofc_sema_scope_decl_find_modify(
			scope, base_name.string, true);
	if (ofc_sema_decl_is_array(edecl)
		|| ofc_sema_type_is_character(
			ofc_sema_decl_type(edecl)))
		return NULL;

	ofc_sema_scope_t* func
		= ofc_sema_scope__create(
			scope, OFC_SEMA_SCOPE_STMT_FUNC);
	if (!func) return NULL;
	func->src = stmt->src;

	ofc_sema_decl_t* rdecl;
	if (edecl)
	{
		rdecl = ofc_sema_decl_copy(edecl);
		if (!ofc_sema_decl_list_add(
			func->decl, rdecl))
		{
			ofc_sema_decl_delete(rdecl);
			ofc_sema_scope_delete(func);
			return NULL;
		}
	}
	else
	{
		rdecl = ofc_sema_scope_decl_find_create_ns(
			func, base_name, true, NULL);
	}
	if (!rdecl || !rdecl->type)
	{
		ofc_sparse_ref_error(stmt->src,
			"No IMPLICIT rule matches statement function name");
		ofc_sema_scope_delete(func);
		return NULL;
	}
	rdecl->is_argument = false;
	rdecl->is_return   = true;
	rdecl->was_written = true;

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

		unsigned i;
		for (i = 0; i < func->args->count; i++)
		{
			ofc_sparse_ref_t arg_name
				= func->args->arg[i].name;

			ofc_sema_decl_t* pdecl
				= ofc_sema_scope_decl_find_create(
					scope, arg_name, true);
			if (!pdecl)
			{
				ofc_sema_scope_delete(func);
				return false;
			}

			ofc_sema_decl_t* adecl
				= ofc_sema_decl_copy(pdecl);
			if (!ofc_sema_decl_list_add(
				func->decl, adecl))
			{
				ofc_sema_decl_delete(adecl);
				ofc_sema_scope_delete(func);
				return false;
			}

			pdecl->is_stmt_func_arg = true;

			adecl->is_argument = true;
			adecl->is_return   = false;
		}
	}

	func->expr = ofc_sema_expr(
		func, stmt->assignment->init);
	if (!func->expr)
	{
		ofc_sema_scope_delete(func);
		return NULL;
	}

	ofc_sema_decl_t* decl
		= ofc_sema_scope_decl_find_create(
			scope, base_name, true);
	if (!decl || !decl->type)
	{
		ofc_sema_scope_delete(func);
		return NULL;
	}

	if (ofc_sema_decl_is_array(decl)
		|| ofc_sema_decl_is_procedure(decl))
	{
		ofc_sema_scope_delete(func);
		return NULL;
	}

	if (!ofc_sema_decl_function(decl))
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

	if (!ofc_sema_scope__body_validate(func))
	{
		ofc_sema_scope_delete(func);
		return NULL;
	}

	return func;
}

ofc_sema_scope_t* ofc_sema_scope_module(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_MODULE))
		return NULL;

	ofc_sparse_ref_t name = stmt->program.name;
	if (ofc_sparse_ref_empty(name))
		return NULL;

	ofc_sema_scope_t* module
		= ofc_sema_scope__create(
			scope, OFC_SEMA_SCOPE_MODULE);
	if (!module)
	{
		ofc_sema_scope_delete(module);
		return NULL;
	}
	module->src  = stmt->src;
	module->name = stmt->program.name.string;

	if (stmt->program.end_has_label
		&& !ofc_sema_label_map_add_end_scope(
			module->label, stmt->program.end_label, module))
	{
		ofc_sema_scope_delete(module);
		return NULL;
	}

	if (!ofc_sema_scope__body(
		module, stmt->program.body))
	{
		ofc_sema_scope_delete(module);
		return NULL;
	}

	if (!ofc_sema_scope__add_child(scope, module))
	{
		ofc_sema_scope_delete(module);
		return NULL;
	}

	return module;
}

bool ofc_sema_scope_block_data_name_exists(
	ofc_sema_scope_t* scope,
	ofc_str_ref_t name)
{
	if (!scope || !scope->child)
		return false;

	ofc_sema_scope_t* block_data
		= ofc_sema_scope_list_find_name(
			scope->child, name);

	return (block_data != NULL);
}

ofc_sema_scope_t* ofc_sema_scope_block_data(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt || !scope
		|| (stmt->type != OFC_PARSE_STMT_BLOCK_DATA))
		return NULL;

	if (stmt->program.end_has_label)
	{
		ofc_sparse_ref_error(stmt->src,
			"END BLOCK DATA can't have label");
		return NULL;
	}

	ofc_sema_scope_t* block_data
		= ofc_sema_scope__create(
			scope, OFC_SEMA_SCOPE_BLOCK_DATA);
	if (!block_data) return NULL;

	block_data->src  = stmt->src;
	block_data->name = stmt->program.name.string;

	if (!ofc_sema_scope__body(
		block_data, stmt->program.body))
	{
		ofc_sema_scope_delete(block_data);
		return NULL;
	}

	ofc_sema_scope__check_namespace_collision(
		scope, "Block Data", stmt->src);

	if (!ofc_sema_scope__add_child(scope, block_data))
	{
		ofc_sema_scope_delete(block_data);
		return NULL;
	}

	return block_data;
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



const ofc_sema_implicit_t* ofc_sema_scope_implicit(
	const ofc_sema_scope_t* scope)
{
	if (!scope)
		return NULL;
	if (scope->implicit)
		return scope->implicit;
	return ofc_sema_scope_implicit(
		scope->parent);
}

ofc_sema_implicit_t* ofc_sema_scope_implicit_modify(
	ofc_sema_scope_t* scope)
{
	if (!scope)
		return NULL;
	if (scope->implicit)
		return scope->implicit;
	return ofc_sema_scope_implicit_modify(
		scope->parent);
}



const ofc_sema_label_t* ofc_sema_scope_label_find(
	const ofc_sema_scope_t* scope, unsigned label)
{
	if (!scope)
		return NULL;

	const ofc_sema_label_t* l
		= ofc_sema_label_map_find(
			scope->label, label);
	if (l) return l;

	if (!scope->label)
		return ofc_sema_scope_label_find(
			scope->parent, label);

	return NULL;
}

ofc_sema_label_t* ofc_sema_scope_label_modify(
	ofc_sema_scope_t* scope, unsigned label)
{
	if (!scope)
		return NULL;

	ofc_sema_label_t* l
		= ofc_sema_label_map_find_modify(
			scope->label, label);
	if (l) return l;

	if (!scope->label)
		return ofc_sema_scope_label_modify(
			scope->parent, label);

	return NULL;
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


const ofc_sema_decl_t* ofc_sema_scope_decl_find(
	const ofc_sema_scope_t* scope, ofc_str_ref_t name, bool local)
{
	if (!scope)
		return NULL;

	switch (scope->type)
	{
		case OFC_SEMA_SCOPE_STMT_FUNC:
			break;
		default:
			local = true;
			break;
	}

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

	switch (scope->type)
	{
		case OFC_SEMA_SCOPE_STMT_FUNC:
			break;
		default:
			local = true;
			break;
	}

	ofc_sema_decl_t* decl
		= ofc_sema_decl_list_find_modify(
			scope->decl, name);
	if (decl) return decl;

	if (local)
		return NULL;

	return ofc_sema_scope_decl_find_modify(
		scope->parent, name, false);
}

ofc_sema_decl_t* ofc_sema_scope_decl_find__create(
	ofc_sema_scope_t* scope, ofc_sparse_ref_t name,
	bool local, bool create, const char* name_space)
{
	if (!scope)
		return NULL;

	ofc_sema_decl_t* decl
		= ofc_sema_decl_list_find_modify(
			scope->decl, name.string);
	if (decl) return decl;

	switch (scope->type)
	{
		case OFC_SEMA_SCOPE_STMT_FUNC:
			break;
		default:
			local = true;
			break;
	}

	if (!local)
	{
		decl = ofc_sema_scope_decl_find__create(
			scope->parent, name, false, false, name_space);
		if (decl) return decl;
	}

	if (create)
	{
		decl = ofc_sema_decl_create(
			ofc_sema_scope_implicit(scope), name);
		if (!decl) return NULL;
		decl->is_static = scope->attr_save;

		if (name_space != NULL)
		{
			ofc_sema_scope__check_namespace_collision(
				scope, name_space, name);
		}

		ofc_sema_external_t* external
			= ofc_sema_external_list_find_modify(
				scope->external, name.string);
		if (external)
		{
			decl->is_external = true;
			if (!ofc_sema_decl_reference(decl))
			{
				ofc_sema_decl_delete(decl);
				return NULL;
			}
			external->decl = decl;
		}

		if (scope->args)
		{
			bool case_sensitive = false;
			if (scope && scope->decl)
				case_sensitive = scope->decl->case_sensitive;

			unsigned i;
			for (i = 0; i < scope->args->count; i++)
			{
				if (scope->args->arg[i].alt_return
					|| ofc_sparse_ref_empty(scope->args->arg[i].name))
					continue;

				const ofc_str_ref_t arg_name
					= scope->args->arg[i].name.string;

				if (case_sensitive
					? ofc_str_ref_equal(arg_name, name.string)
					: ofc_str_ref_equal_ci(arg_name, name.string))
				{
					decl->is_argument = true;
					break;
				}
			}
		}

		if (!ofc_sema_decl_list_add(
			scope->decl, decl))
		{
			ofc_sema_decl_delete(decl);
			return NULL;
		}
	}

	return decl;
}



ofc_sema_decl_t* ofc_sema_scope_decl_find_create_ns(
	ofc_sema_scope_t* scope, ofc_sparse_ref_t name,
	bool local, const char* name_space)
{
	return ofc_sema_scope_decl_find__create(
		scope, name, local, true, name_space);
}

ofc_sema_decl_t* ofc_sema_scope_decl_find_create(
	ofc_sema_scope_t* scope, ofc_sparse_ref_t name, bool local)
{
	return ofc_sema_scope_decl_find_create_ns(
		scope, name, local, "Declaration");
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

bool ofc_sema_scope_structure_name_exists(
	ofc_sema_scope_t* scope, ofc_str_ref_t name)
{
	if (!scope)
		return NULL;

	ofc_sema_structure_t* structure =
		ofc_sema_scope_structure_find(
			scope, name);

	return structure ? true : false;
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

bool ofc_sema_scope_common_name_exists(
	ofc_sema_scope_t* scope, ofc_str_ref_t name)
{
    if (!scope || !scope->common)
		return false;

	const ofc_sema_common_t* common
		= ofc_sema_common_map_find(
			scope->common, name);

	return common ? true : false;
}

ofc_sema_common_t* ofc_sema_scope_common_find_create(
	ofc_sema_scope_t* scope, ofc_sparse_ref_t name)
{
	if (!scope)
		return NULL;

	if (!scope->common)
		return ofc_sema_scope_common_find_create(
			scope->parent, name);

	ofc_sema_common_t* common
		= ofc_sema_common_map_find_modify(
			scope->common, name.string);
	if (!common)
	{
		common = ofc_sema_common_create(name.string);
		if (!common) return NULL;

		ofc_sema_scope__check_namespace_collision(
			scope, "Common Block", name);

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

	if (scope->module &&
		!ofc_sema_module_list_print(
			cs, indent, scope->module))
	{
		ofc_file_error(NULL, NULL,
			"Failed to print module list");
		return false;
	}

	bool implicit_none = true;
	switch (scope->type)
	{
		case OFC_SEMA_SCOPE_GLOBAL:
		case OFC_SEMA_SCOPE_STMT_FUNC:
			implicit_none = false;
			break;

		default:
			implicit_none = true;
			break;
	}

	if (scope->args)
	{
		unsigned i;
		for (i = 0; i < scope->args->count; i++)
		{
			if (scope->args->arg[i].alt_return
				|| ofc_sparse_ref_empty(scope->args->arg[i].name))
				continue;

			if (!ofc_sema_decl_list_find(
				scope->decl, scope->args->arg[i].name.string))
			{
				implicit_none = false;
				break;
			}
		}
	}

	if (implicit_none)
	{
		if (!ofc_colstr_newline(cs, indent, NULL)
			|| !ofc_colstr_keyword_atomic_writez(cs, "IMPLICIT")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_keyword_atomic_writez(cs, "NONE"))
			return false;
	}

	if (scope->type == OFC_SEMA_SCOPE_MODULE)
	{
		if ((scope->access == OFC_SEMA_ACCESSIBILITY_PUBLIC)
			&& (!ofc_colstr_newline(cs, indent, NULL)
				|| !ofc_colstr_keyword_atomic_writez(cs, "PUBLIC")))
			return false;
		if ((scope->access == OFC_SEMA_ACCESSIBILITY_PRIVATE)
			&& (!ofc_colstr_newline(cs, indent, NULL)
				|| !ofc_colstr_keyword_atomic_writez(cs, "PRIVATE")))
			return false;
	}

	if (scope->external
		&& !ofc_sema_external_list_print(cs, indent, scope->external))
	{
		ofc_file_error(NULL, NULL,
			"Failed to print externals list");
		return false;
	}

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

	return true;
}

bool ofc_sema_scope_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_sema_scope_t* scope)
{
	if (!scope)
		return false;

	bool is_procedure = false;
	bool implicit = false;
	const char* kwstr = NULL;
	switch (scope->type)
	{
		case OFC_SEMA_SCOPE_GLOBAL:
			break;
		case OFC_SEMA_SCOPE_PROGRAM:
			kwstr = "PROGRAM";
			implicit = ofc_str_ref_empty(scope->name);
			break;
		case OFC_SEMA_SCOPE_SUBROUTINE:
			kwstr = "SUBROUTINE";
			is_procedure = true;
			break;
		case OFC_SEMA_SCOPE_FUNCTION:
			kwstr = "FUNCTION";
			is_procedure = true;
			break;
		case OFC_SEMA_SCOPE_MODULE:
			kwstr = "MODULE";
			break;
		case OFC_SEMA_SCOPE_BLOCK_DATA:
			kwstr = "BLOCK DATA";
			break;
		case OFC_SEMA_SCOPE_STMT_FUNC:
			return ofc_sema_expr_print(cs, scope->expr);

		default:
			return false;
	}

	if (kwstr && !implicit)
	{
		if (scope->type != OFC_SEMA_SCOPE_FUNCTION)
		{
			/* Decl function printing will handle the new lines
			   as we need to specify the return type. */
			if (!ofc_colstr_newline(cs, indent, NULL))
				return false;
		}

		const ofc_print_opts_t* opts
			= ofc_colstr_print_opts_get(cs);

		bool is_recursive = scope->attr_recursive;

		if (scope->contains_automatic
			&& is_procedure
			&& (!opts || !opts->automatic))
			is_recursive = true;

		if (is_recursive)
		{
			if (!ofc_colstr_keyword_atomic_writez(cs, "RECURSIVE")
				|| !ofc_colstr_atomic_writef(cs, " "))
				return false;
		}

		if (!ofc_colstr_keyword_atomic_writez(cs, kwstr)
			|| !ofc_colstr_atomic_writef(cs, " "))
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
		const ofc_sema_label_t* label
			= ofc_sema_label_map_find_end_scope(
				scope->label, scope);
		const unsigned* ulabel = (label ? &label->number : NULL);

		if (!ofc_colstr_newline(cs, indent, ulabel)
			|| !ofc_colstr_keyword_atomic_writef(cs, "END %s ", kwstr))
			return false;

		if (scope->name.base)
		{
			if (!ofc_colstr_atomic_writef(cs, "%.*s",
				scope->name.size, scope->name.base))
					return false;
		}
	}

	/* TODO - Resolve potential issue with global declarations,
	          being printed inside implicit PROGRAM scope. */

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

ofc_sema_scope_t* ofc_sema_scope_list_find_name(
	ofc_sema_scope_list_t* list,
	ofc_str_ref_t name)
{
    if (!list)
		return NULL;

	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		/* TODO - Handle case sensitivity flag */

        if (ofc_str_ref_equal_ci(
			list->scope[i]->name, name))
				return list->scope[i];
	}

	return NULL;
}

static ofc_sema_scope_t* ofc_sema_scope_list__find_type_name(
	ofc_sema_scope_list_t* list,
	ofc_sema_scope_e type,
	ofc_str_ref_t name)
{
	if (!list)
		return NULL;

	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		if (ofc_str_ref_equal_ci(list->scope[i]->name, name)
			&& (list->scope[i]->type == type))
				return list->scope[i];
	}

	return NULL;
}


ofc_sema_scope_t* ofc_sema_scope_find_type_name(
	ofc_sema_scope_t* scope,
	ofc_sema_scope_e type,
	ofc_str_ref_t name)
{
	if (!scope)
		return NULL;

	ofc_sema_scope_t* match
		= ofc_sema_scope_list__find_type_name(
			scope->child, type, name);

	if (match)
		return match;

	if (scope->parent)
	{
		match = ofc_sema_scope_find_type_name(
			scope->parent, type, name);

		if (match)
			return match;
	}

	return NULL;
}

ofc_sema_scope_t* ofc_sema_scope_find_module_name(
	ofc_sema_scope_t* scope,
	ofc_str_ref_t name)
{
	if (!scope)
		return NULL;

	/* It might be safe to assume that a module is never
	   in the child scope list but for now this gives
	   full coverage. */
	return ofc_sema_scope_find_type_name(
		scope, OFC_SEMA_SCOPE_MODULE, name);
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

bool ofc_sema_scope_list_foreach(
	ofc_sema_scope_list_t* list, void* param,
	bool (*func)(ofc_sema_scope_t* scope, void* param))
{
	if (!list || !func)
		return false;

	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		if (!ofc_sema_scope_foreach_scope(
			list->scope[i], param, func))
			return false;
	}

	return true;
}



bool ofc_sema_scope_foreach_scope(
	ofc_sema_scope_t* scope, void* param,
	bool (*func)(ofc_sema_scope_t* scope, void* param))
{
	if (!scope || !func)
		return false;

	if (scope->child
		&& !ofc_sema_scope_list_foreach(
			scope->child, param, func))
		return false;

	if (scope->decl
		&& !ofc_sema_decl_list_foreach_scope(
			scope->decl, param, func))
		return false;

	return func(scope, param);
}

bool ofc_sema_scope_foreach_decl(
	ofc_sema_scope_t* scope, void* param,
	bool (*func)(ofc_sema_decl_t* decl, void* param))
{
	if (!scope)
		return false;

	if (scope->decl
		&& !ofc_sema_decl_list_foreach(
			scope->decl, param, func))
		return false;

	return true;
}

bool ofc_sema_scope_foreach_stmt(
	ofc_sema_scope_t* scope, void* param,
	bool (*func)(ofc_sema_stmt_t* stmt, void* param))
{
	if (!scope || !func)
		return false;

	if (scope->stmt
		&& !ofc_sema_stmt_list_foreach(
			scope->stmt, param, func))
		return false;

	return true;
}

bool ofc_sema_scope_foreach_expr(
	ofc_sema_scope_t* scope, void* param,
	bool (*func)(ofc_sema_expr_t* expr, void* param))
{
	if (!scope || !func)
		return false;

	if ((scope->type == OFC_SEMA_SCOPE_STMT_FUNC)
		&& scope->expr && !func(scope->expr, param))
		return false;

	if (scope->structure
		&& !ofc_sema_structure_list_foreach_expr(
			scope->structure, param, func))
		return false;

	if (scope->derived_type
		&& !ofc_sema_structure_list_foreach_expr(
			scope->derived_type, param, func))
		return false;

	if (scope->decl
		&& !ofc_sema_decl_list_foreach_expr(
			scope->decl, param, func))
		return false;

	if ((scope->type != OFC_SEMA_SCOPE_STMT_FUNC)
		&& scope->stmt
		&& !ofc_sema_stmt_list_foreach_expr(
			scope->stmt, param, func))
		return false;

	return true;
}

bool ofc_sema_scope_foreach_structure(
	ofc_sema_scope_t* scope, void* param,
	bool (*func)(ofc_sema_structure_t* structure, void* param))
{
	if (!scope)
		return false;

	if (scope->structure
		&& !ofc_sema_structure_list_foreach(
			scope->structure, param, func))
		return false;

	if (scope->derived_type
		&& !ofc_sema_structure_list_foreach(
			scope->derived_type, param, func))
		return false;

	return true;
}



static bool ofc_sema_scope_common_usage_print__decl(
	ofc_sema_decl_t* decl, void* param)
{
	(void)param;

	if (!decl)
		return false;

	if (!decl->was_read
		&& !decl->was_written)
		return true;

	if (!decl->common)
		return true;

	printf("/%.*s/%.*s\n",
		decl->common->name.size,
		decl->common->name.base,
		decl->name.string.size,
		decl->name.string.base);
	return true;
}

static bool ofc_sema_scope_common_usage_print__scope(
	ofc_sema_scope_t* scope, void* param)
{
	if (!scope)
		return false;

	return ofc_sema_scope_foreach_decl(
		scope, param, ofc_sema_scope_common_usage_print__decl);
}

void ofc_sema_scope_common_usage_print(
	const ofc_sema_scope_t* scope)
{
	ofc_sema_scope_common_usage_print__scope(
		(ofc_sema_scope_t*)scope, NULL);
	ofc_sema_scope_foreach_scope(
		(ofc_sema_scope_t*)scope, NULL,
		ofc_sema_scope_common_usage_print__scope);
}
