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


ofc_sema_equiv_t* ofc_sema_equiv_create(void)
{
	ofc_sema_equiv_t* equiv
		= (ofc_sema_equiv_t*)malloc(
			sizeof(ofc_sema_equiv_t));
	if (!equiv) return NULL;

	equiv->count = 0;
	equiv->lhs   = NULL;
	return equiv;
}

void ofc_sema_equiv_delete(
	ofc_sema_equiv_t* equiv)
{
	if (!equiv)
		return;

	unsigned i;
	for (i = 0; i < equiv->count; i++)
		ofc_sema_lhs_delete(equiv->lhs[i]);
	free(equiv->lhs);
	free(equiv);
}


bool ofc_sema_equiv_add(
	ofc_sema_equiv_t* equiv, ofc_sema_lhs_t* lhs)
{
	if (!equiv || !lhs)
		return false;

	ofc_sema_lhs_t** nlhs
		= (ofc_sema_lhs_t**)realloc(equiv->lhs,
			(sizeof(ofc_sema_lhs_t*) * (equiv->count + 1)));
	if (!nlhs) return false;
	equiv->lhs = nlhs;

	ofc_sema_decl_t* decl
		= ofc_sema_lhs_decl(lhs);
	if (decl) decl->is_equiv = true;

	equiv->lhs[equiv->count++] = lhs;
	return true;
}


bool ofc_sema_equiv_print(
	ofc_colstr_t* cs, const ofc_sema_equiv_t* equiv)
{
	if (!equiv)
		return false;

	if (!ofc_colstr_atomic_writef(cs, "EQUIVALENCE")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_colstr_atomic_writef(cs, "("))
		return false;

	unsigned i;
	for (i = 0; i < equiv->count; i++)
	{
		if (i > 0)
		{
			if (!ofc_colstr_atomic_writef(cs, ",")
				|| !ofc_colstr_atomic_writef(cs, " "))
				return false;
		}

		if (!ofc_sema_lhs_print(
			cs, equiv->lhs[i]))
			return false;
	}

	return ofc_colstr_atomic_writef(cs, ")");
}



ofc_sema_equiv_list_t* ofc_sema_equiv_list_create(void)
{
	ofc_sema_equiv_list_t* list
		= (ofc_sema_equiv_list_t*)malloc(
			sizeof(ofc_sema_equiv_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->equiv = NULL;
	return list;
}

void ofc_sema_equiv_list_delete(
	ofc_sema_equiv_list_t* list)
{
	if (!list)
		return;

	unsigned i;
	for (i = 0; i < list->count; i++)
		ofc_sema_equiv_delete(list->equiv[i]);
	free(list->equiv);
	free(list);
}


bool ofc_sema_equiv_list_add(
	ofc_sema_equiv_list_t* list,
	ofc_sema_equiv_t* equiv)
{
	if (!list || !equiv)
		return false;

	ofc_sema_equiv_t** nequiv
		= (ofc_sema_equiv_t**)realloc(list->equiv,
			(sizeof(ofc_sema_equiv_t*) * (list->count + 1)));
	if (!nequiv) return false;
	list->equiv = nequiv;

	list->equiv[list->count++] = equiv;
	return true;
}


bool ofc_sema_equiv_list_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_sema_equiv_list_t* list)
{
	if (!list)
		return false;

	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		if (!ofc_colstr_newline(cs, indent, NULL)
			|| !ofc_sema_equiv_print(cs, list->equiv[i]))
			return false;
	}

	return true;
}
