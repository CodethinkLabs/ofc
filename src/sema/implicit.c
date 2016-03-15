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


typedef struct
{
	const ofc_sema_type_t* type;

	bool is_static;
	bool is_automatic;
	bool is_volatile;
	bool is_intrinsic;
	bool is_external;
} ofc_sema_implicit_rule_t;

struct ofc_sema_implicit_s
{
	ofc_sema_implicit_rule_t rule[26];
};


ofc_sema_implicit_t* ofc_sema_implicit_create(void)
{
	ofc_sema_implicit_t* implicit
		= (ofc_sema_implicit_t*)malloc(
			sizeof(ofc_sema_implicit_t));
	if (!implicit) return NULL;

	unsigned i, c;
	for (i = 0, c = 'A'; c <= 'Z'; i++, c++)
	{
		switch (c)
		{
			case 'I':
			case 'J':
			case 'K':
			case 'L':
			case 'M':
			case 'N':
				implicit->rule[i].type
					= ofc_sema_type_create_primitive(
						OFC_SEMA_TYPE_INTEGER, OFC_SEMA_KIND_NONE);
				break;

			default:
				implicit->rule[i].type
					= ofc_sema_type_create_primitive(
						OFC_SEMA_TYPE_REAL, OFC_SEMA_KIND_NONE);
				break;
		}

		implicit->rule[i].is_static    = false;
		implicit->rule[i].is_automatic = false;
		implicit->rule[i].is_volatile  = false;
		implicit->rule[i].is_intrinsic = false;
		implicit->rule[i].is_external  = false;
	}

	return implicit;
}

ofc_sema_implicit_t* ofc_sema_implicit_copy(
	const ofc_sema_implicit_t* implicit)
{
	if (!implicit)
		return NULL;

	ofc_sema_implicit_t* copy
		= (ofc_sema_implicit_t*)malloc(
			sizeof(ofc_sema_implicit_t));
	if (!copy) return NULL;

	unsigned i;
	for (i = 0; i < 26; i++)
		copy->rule[i] = implicit->rule[i];

	return copy;
}

void ofc_sema_implicit_delete(
	ofc_sema_implicit_t* implicit)
{
	free(implicit);
}


bool ofc_sema_implicit(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt)
		return false;

	ofc_sema_implicit_t* implicit
		= ofc_sema_scope_implicit_modify(scope);

	if (stmt->type == OFC_PARSE_STMT_IMPLICIT_NONE)
	{
		unsigned i;
		for (i = 0; i < 26; i++)
			implicit->rule[i].type = NULL;
		return true;
	}

	if ((stmt->type
		!= OFC_PARSE_STMT_IMPLICIT)
		|| !stmt->implicit)
		return false;

	unsigned i;
	for (i = 0; i < stmt->implicit->count; i++)
	{
		const ofc_parse_implicit_t* rule
			= stmt->implicit->rule[i];

		if (rule->undefined)
		{
			unsigned c, m;
			for (c = 'A', m = 1; c <= 'Z'; c++, m <<= 1)
			{
				if ((rule->mask & m) == 0)
					continue;

				implicit->rule[i].type = NULL;
			}
		}
		else
		{
			const ofc_sema_type_t* stype
				= ofc_sema_type(scope, rule->type, NULL);

			unsigned j, c, m;
			for (j = 0, c = 'A', m = 1; c <= 'Z'; j++, c++, m <<= 1)
			{
				if ((rule->mask & m) == 0)
					continue;

				implicit->rule[j].type = stype;

				implicit->rule[j].is_static    |= rule->attr.is_static;
				implicit->rule[j].is_automatic |= rule->attr.is_automatic;
				implicit->rule[j].is_volatile  |= rule->attr.is_volatile;
				implicit->rule[j].is_intrinsic |= rule->attr.is_intrinsic;
				implicit->rule[j].is_external  |= rule->attr.is_external;
			}
		}
	}

	/* Apply new implicit rule to existing arguments. */
	if (scope->args)
	{
		for (i = 0; i < scope->args->count; i++)
		{
			ofc_sema_arg_t arg
				= scope->args->arg[i];
			if (arg.alt_return)
				continue;

			ofc_sema_decl_t* decl
				= ofc_sema_scope_decl_find_modify(
					scope, arg.name.string, true);
			if (!decl || !decl->type_implicit)
				continue;

			bool is_static    = false;
			bool is_automatic = false;
			bool is_volatile  = false;
			bool is_intrinsic = false;
			bool is_external  = false;

			if (ofc_sema_implicit_attr(
				implicit, arg.name, &decl->type,
				&is_static, &is_automatic,
				&is_volatile, &is_intrinsic,
				&is_external))
			{
				if (is_static)
					decl->is_static = true;
				if (is_automatic)
					decl->is_automatic = true;
				if (is_volatile)
					decl->is_volatile = true;
				if (is_external)
					decl->is_external = true;
				if (is_intrinsic)
					decl->is_intrinsic = true;
			}
		}
	}

	return true;
}


bool ofc_sema_implicit_attr(
	const ofc_sema_implicit_t* implicit,
	ofc_sparse_ref_t name,
	const ofc_sema_type_t** type,
	bool* is_static,
	bool* is_automatic,
	bool* is_volatile,
	bool* is_intrinsic,
	bool* is_external)
{
	if (!implicit
		|| ofc_sparse_ref_empty(name)
		|| !isalpha(*name.string.base))
		return false;
	unsigned i = (toupper(*name.string.base) - 'A');
	if (type        ) *type         = implicit->rule[i].type;
	if (is_static   ) *is_static    = implicit->rule[i].is_static;
	if (is_automatic) *is_automatic = implicit->rule[i].is_automatic;
	if (is_volatile ) *is_volatile  = implicit->rule[i].is_volatile;
	if (is_intrinsic) *is_intrinsic = implicit->rule[i].is_intrinsic;
	if (is_external ) *is_external  = implicit->rule[i].is_external;
	return true;
}

