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
	for (i = 0; i < ('I' - 'A'); i++)
		implicit->spec[i] = real;

	/* I, J, K, L, M, N */
	for (; i <= ('N' - 'A'); i++)
		implicit->spec[i] = integer;

	for (; i <= ('Z' - 'A'); i++)
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

bool ofc_sema_implicit_set_undefined(
	ofc_sema_implicit_t* implicit, char c)
{
	if (!implicit || !isalpha(c))
		return false;

	unsigned i = (toupper(c) - 'A');
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

	if (!ofc_sema_spec_overlay(
		&spec, &implicit->spec[i]))
		return false;

	implicit->spec[i] = spec;
	return true;
}

ofc_sema_spec_t* ofc_sema_implicit_apply(
	const ofc_sema_implicit_t* implicit,
	ofc_sparse_ref_t           name,
	const ofc_sema_spec_t*     spec)
{
	if (!implicit || ofc_sparse_ref_empty(name)
		|| !isalpha(name.string.base[0]))
		return NULL;

	ofc_sema_spec_t* copy = (spec
		? ofc_sema_spec_copy(spec)
		: ofc_sema_spec_create(name));
	if (!copy) return NULL;

	const ofc_sema_spec_t* overlay
		= &implicit->spec[toupper(name.string.base[0]) - 'A'];

	if (!ofc_sema_spec_overlay(
		copy, overlay))
	{
		ofc_sema_spec_delete(copy);
		return NULL;
	}

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

		if (rule->undefined)
		{
			unsigned c, m;
			for (c = 'A', m = 1; c <= 'Z'; c++, m <<= 1)
			{
				if ((rule->mask & m)
					&& !ofc_sema_implicit_set_undefined(
						implicit, c))
					return false;
			}
		}
		else
		{
			ofc_sema_spec_t* spec;
			if (rule->type)
				spec = ofc_sema_spec(scope, rule->type);
			else
				spec = ofc_sema_spec_create(OFC_SPARSE_REF_EMPTY);
			if (!spec) return false;

			spec->is_static    |= rule->attr.is_static;
			spec->is_automatic |= rule->attr.is_automatic;
			spec->is_volatile  |= rule->attr.is_volatile;
			spec->is_intrinsic |= rule->attr.is_intrinsic;
			spec->is_external  |= rule->attr.is_external;

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
	}

	return true;
}
