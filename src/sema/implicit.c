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

#include <ofc/sema.h>

struct ofc_sema_implicit_s
{
	ofc_sema_spec_t spec[26];
};



ofc_sema_implicit_t* ofc_sema_implicit_create(void)
{
	ofc_sema_implicit_t* implicit
		= (ofc_sema_implicit_t*)malloc(
			sizeof(ofc_sema_implicit_t));
	if (!implicit) return NULL;

	ofc_sema_spec_t real
		= OFC_SEMA_SPEC_DEFAULT;
	real.type = OFC_SEMA_TYPE_REAL;
	real.type_implicit = false;

	ofc_sema_spec_t integer
		= OFC_SEMA_SPEC_DEFAULT;
	integer.type = OFC_SEMA_TYPE_INTEGER;
	integer.type_implicit = false;

	unsigned i;
	for (i = 0; i < 7; i++)
		implicit->spec[i] = real;

	/* I, J, K, L, M, N */
	for (; i < 14; i++)
		implicit->spec[i] = integer;

	for (; i < 26; i++)
		implicit->spec[i] = real;

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
		copy->spec[i] = implicit->spec[i];

	return copy;
}


bool ofc_sema_implicit_none(ofc_sema_implicit_t* implicit)
{
	if (!implicit)
		return false;

	unsigned i;
	for (i = 0; i < 26; i++)
		implicit->spec[i] = OFC_SEMA_SPEC_DEFAULT;

	return true;
}

bool ofc_sema_implicit_set(
	ofc_sema_implicit_t* implicit,
	ofc_sema_spec_t spec, char c)
{
	if (!implicit || !isalpha(c))
		return false;

	unsigned i = (toupper(c) - 'A');
	implicit->spec[i] = spec;

	return true;
}

ofc_sema_spec_t* ofc_sema_implicit_apply(
	const ofc_sema_implicit_t* implicit,
	ofc_str_ref_t              name,
	const ofc_sema_spec_t*     spec)
{
	if (!implicit || ofc_str_ref_empty(name)
		|| !isalpha(name.base[0]))
		return NULL;

	ofc_sema_spec_t* copy = (spec
		? ofc_sema_spec_copy(spec)
		: ofc_sema_spec_create(name));
	if (!copy) return NULL;

	const ofc_sema_spec_t* i
		= &implicit->spec[toupper(name.base[0]) - 'A'];

	if (copy->type_implicit)
	{
		copy->type_implicit = i->type_implicit;
		copy->type          = i->type;
		copy->kind          = i->kind;
	}
	else if (!i->type_implicit
		&& (copy->type == i->type)
		&& (copy->kind == 0))
	{
		copy->kind = i->kind;
	}

	if ((copy->len == 0)
		&& !copy->len_var)
	{
		copy->len     = i->len;
		copy->len_var = i->len_var;
	}

	copy->is_static    |= i->is_static;
	copy->is_automatic |= i->is_automatic;
	copy->is_volatile  |= i->is_volatile;
	copy->is_intrinsic |= i->is_intrinsic;
	copy->is_external  |= i->is_external;

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
		= scope->implicit;

	if (stmt->type
		== OFC_PARSE_STMT_IMPLICIT_NONE)
		return ofc_sema_implicit_none(implicit);

	if ((stmt->type
		!= OFC_PARSE_STMT_IMPLICIT)
		|| !stmt->implicit)
		return false;

	unsigned i;
	for (i = 0; i < stmt->implicit->count; i++)
	{
		const ofc_parse_implicit_t* rule
			= stmt->implicit->rule[i];

		ofc_sema_spec_t* spec
			= ofc_sema_spec(scope, rule->type);
		if (!spec) return false;

		/* Can't make IMPLICIT rules with array dimensions. */
		if (spec->array)
		{
			ofc_sema_spec_delete(spec);
			return false;
		}

		unsigned c, m;
		for (c = 'A', m = 1; c <= 'Z'; c++, m <<= 1)
		{
			if ((rule->mask & m)
				&& !ofc_sema_implicit_set(
					implicit, *spec, c))
				return false;
		}

		ofc_sema_spec_delete(spec);
	}

	return true;
}
