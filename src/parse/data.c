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

#include "ofc/parse.h"


static ofc_parse_clist_entry_t* ofc_parse_clist_entry(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	ofc_parse_clist_entry_t* entry
		= (ofc_parse_clist_entry_t*)malloc(
			sizeof(ofc_parse_clist_entry_t));
	if (!entry) return NULL;

	unsigned i = ofc_parse_unsigned(
		src, ptr, debug, &entry->repeat);
	if ((i > 0) && (ptr[i] == '*'))
	{
		i += 1;
	}
	else
	{
		entry->repeat = 0;
		i = 0;
		ofc_parse_debug_rewind(debug, dpos);
	}

	unsigned l;
	entry->expr = ofc_parse_expr(
		src, &ptr[i], debug, &l);
	if (!entry->expr)
	{
		free(entry);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}
	i += l;

	if (len) *len = i;
	return entry;
}

static void ofc_parse_clist_entry_delete(
	ofc_parse_clist_entry_t* entry)
{
	if (!entry)
		return;

	ofc_parse_expr_delete(entry->expr);
	free(entry);
}

static bool ofc_parse_clist_entry_print(
	ofc_colstr_t* cs, const ofc_parse_clist_entry_t* entry)
{
	if (!entry)
		return false;

	if ((entry->repeat > 1)
		&& !ofc_colstr_atomic_writef(cs, "%u*", entry->repeat))
		return false;

	return ofc_parse_expr_print(cs, entry->expr);
}



ofc_parse_clist_t* ofc_parse_clist(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = 0;
	if (ptr[i++] != '/')
		return NULL;

	ofc_parse_clist_t* list
		= (ofc_parse_clist_t*)malloc(
			sizeof(ofc_parse_clist_t));
	if (!list) return NULL;

	list->count = 0;
	list->entry = NULL;

	unsigned l = ofc_parse_list(
		src, &ptr[i], debug, ',',
		&list->count, (void***)&list->entry,
		(void*)ofc_parse_clist_entry,
		(void*)ofc_parse_clist_entry_delete);
	if (l == 0)
	{
		/* clist may not be empty. */
		free(list);
		return NULL;
	}
	i += l;

	if (ptr[i++] != '/')
	{
		ofc_parse_clist_delete(list);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}

	if (len) *len = i;
	return list;
}

void ofc_parse_clist_delete(
	ofc_parse_clist_t* list)
{
	if (!list)
		return;

	ofc_parse_list_delete(
		list->count, (void**)list->entry,
		(void*)ofc_parse_clist_entry_delete);
	free(list);
}

bool ofc_parse_clist_print(
	ofc_colstr_t* cs, const ofc_parse_clist_t* list)
{
	if (!list)
		return false;

	return ofc_parse_list_print(cs,
		list->count, (const void**)list->entry,
		(void*)ofc_parse_clist_entry_print);
}



static ofc_parse_data_entry_t* ofc_parse_data_entry(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	ofc_parse_data_entry_t* entry
		= (ofc_parse_data_entry_t*)malloc(
			sizeof(ofc_parse_data_entry_t));
	if (!entry) return NULL;

	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i;
	entry->nlist = ofc_parse_lhs_list(
		src, ptr, debug, &i);
	if (!entry->nlist)
	{
		free(entry);
		return NULL;
	}

	unsigned l;
	entry->clist = ofc_parse_clist(
		src, &ptr[i], debug, &l);
	if (!entry->clist)
	{
		ofc_parse_lhs_list_delete(entry->nlist);
		free(entry);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}
	i += l;

	if (len) *len = i;
	return entry;
}

static void ofc_parse_data_entry_delete(
	ofc_parse_data_entry_t* entry)
{
	if (!entry)
		return;

	ofc_parse_lhs_list_delete(entry->nlist);
	ofc_parse_clist_delete(entry->clist);
	free(entry);
}

static bool ofc_parse_data_entry_print(
	ofc_colstr_t* cs, const ofc_parse_data_entry_t* entry)
{
	if (!entry)
		return false;

	return (ofc_parse_lhs_list_print(cs, entry->nlist, false)
		&& ofc_colstr_atomic_writef(cs, "/")
		&& ofc_parse_clist_print(cs, entry->clist)
		&& ofc_colstr_atomic_writef(cs, "/"));
}


ofc_parse_data_list_t* ofc_parse_data_list(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	ofc_parse_data_list_t* list
		= (ofc_parse_data_list_t*)malloc(
			sizeof(ofc_parse_data_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->entry = NULL;

	unsigned i = ofc_parse_list_seperator_optional(
		src, ptr, debug, ',',
		&list->count, (void***)&list->entry,
		(void*)ofc_parse_data_entry,
		(void*)ofc_parse_data_entry_delete);
	if (i == 0)
	{
		/* data_list may not be empty. */
		free(list);
		return NULL;
	}

	if (len) *len = i;
	return list;
}

void ofc_parse_data_list_delete(
	ofc_parse_data_list_t* list)
{
	if (!list)
		return;

	ofc_parse_list_delete(
		list->count, (void**)list->entry,
		(void*)ofc_parse_data_entry_delete);
	free(list);
}

bool ofc_parse_data_list_print(
	ofc_colstr_t* cs, const ofc_parse_data_list_t* list)
{
	if (!list)
		return false;

	return ofc_parse_list_print(cs,
		list->count, (const void**)list->entry,
		(void*)ofc_parse_data_entry_print);
}
