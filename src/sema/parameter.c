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


ofc_hashmap_t* ofc_sema_parameter_map_create(
	bool case_sensitive)
{
	return ofc_hashmap_create(
		(void*)(case_sensitive
			? ofc_str_ref_ptr_hash
			: ofc_str_ref_ptr_hash_ci),
		(void*)(case_sensitive
			? ofc_str_ref_ptr_equal
			: ofc_str_ref_ptr_equal_ci),
		(void*)ofc_sema_parameter_name,
		(void*)ofc_sema_parameter_delete);
}


ofc_sema_parameter_t* ofc_sema_parameter_create(
	ofc_str_ref_t name,
	ofc_sema_typeval_t* typeval)
{
	ofc_sema_parameter_t* parameter
		= (ofc_sema_parameter_t*)malloc(
			sizeof(ofc_sema_parameter_t));
	if (!parameter) return NULL;

	parameter->name    = name;
	parameter->typeval = typeval;
	return parameter;
}

ofc_sema_parameter_t* ofc_sema_parameter_assign(
	ofc_sema_scope_t* scope,
	const ofc_parse_assign_t* assign)
{
	if (!assign
		|| !assign->name)
		return NULL;

	if (assign->name->type
		!= OFC_PARSE_LHS_VARIABLE)
	{
		ofc_sparse_ref_error(assign->src,
			"Expected parameter name");
		return NULL;
	}

	const ofc_sema_parameter_t* exists
		= ofc_hashmap_find(scope->parameter,
			&assign->name->variable.string);
	if (exists)
	{
		ofc_sparse_ref_error(assign->src,
			"Duplicate PARAMETER definition");
		return NULL;
	}

	ofc_sema_expr_t* expr
		= ofc_sema_expr(scope, assign->init);
	if (!expr)
	{
		ofc_sparse_ref_error(assign->src,
			"Invalid PARAMETER expression");
		return NULL;
	}

	const ofc_sema_typeval_t* ctypeval
		= ofc_sema_expr_constant(expr);
	ofc_sema_typeval_t* typeval
		= ofc_sema_typeval_copy(ctypeval);
	ofc_sema_expr_delete(expr);
	if (!typeval)
	{
		ofc_sparse_ref_error(assign->src,
			"Failed to resolve PARAMETER value");
		return NULL;
	}

	ofc_sema_parameter_t* param
		= ofc_sema_parameter_create(
			assign->name->variable.string, typeval);
	if (!param)
	{
		ofc_sema_typeval_delete(typeval);
		return NULL;
	}

	return param;
}

bool ofc_sema_parameter(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt || !scope || !scope->parameter
		|| (stmt->type != OFC_PARSE_STMT_PARAMETER))
		return false;

	unsigned count = stmt->parameter.list->count;
	if (count == 0)
		return false;

	unsigned i;
	for (i = 0; i < count; i++)
	{
		ofc_sema_parameter_t* param
			= ofc_sema_parameter_assign(
				scope, stmt->parameter.list->assign[i]);
		if (!param) return false;

		if (!ofc_hashmap_add(scope->parameter, param))
		{
			ofc_sema_parameter_delete(param);
			return false;
		}
	}

	return true;
}

bool ofc_sema_parameter_decl(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt || !scope || !scope->parameter
		|| (stmt->type != OFC_PARSE_STMT_DECL))
		return false;

	/* TODO - Implement Fortran 90 style typed PARAMETER declarations. */
	return false;
}

void ofc_sema_parameter_delete(
	ofc_sema_parameter_t* parameter)
{
	if (!parameter)
		return;

	ofc_sema_typeval_delete(parameter->typeval);
	free(parameter);
}


const ofc_str_ref_t* ofc_sema_parameter_name(
	const ofc_sema_parameter_t* parameter)
{
	return (parameter ? &parameter->name : NULL);
}

const ofc_sema_type_t* ofc_sema_parameter_type(
	const ofc_sema_parameter_t* parameter)
{
	if (!parameter || !parameter->typeval)
		return NULL;
	return parameter->typeval->type;
}

const ofc_sema_typeval_t* ofc_sema_parameter_get(
	const ofc_sema_parameter_t* parameter)
{
	return (parameter ? parameter->typeval : NULL);
}


bool ofc_sema_parameter_integer(
	const ofc_sema_parameter_t* parameter,
	int64_t* value)
{
	if (!parameter)
		return false;

	return ofc_sema_typeval_get_integer(
		parameter->typeval, value);
}
