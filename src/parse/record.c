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


static ofc_parse_record_t* ofc_parse_record(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	unsigned i = 0;
	if (ptr[i++] != '/')
		return 0;

	ofc_parse_record_t* record
		= (ofc_parse_record_t*)malloc(
			sizeof(ofc_parse_record_t));
	if (!record) return NULL;

	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned l = ofc_parse_name(
		src, &ptr[i], debug,
		&record->structure);
	if (l == 0)
	{
		free(record);
		return NULL;
	}
	i += l;

	if (ptr[i++] != '/')
	{
		free(record);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}

	record->name = ofc_parse_lhs(
		src, &ptr[i], debug, &l);
	if (!record->name)
	{
		free(record);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}
	i += l;

	if (len) *len = i;
	return record;
}

static void ofc_parse_record_delete(
	ofc_parse_record_t* record)
{
	if (!record)
		return;

	ofc_parse_lhs_delete(record->name);
	free(record);
}

static bool ofc_parse_record_print(
	ofc_colstr_t* cs, const ofc_parse_record_t* record)
{
	if (!record)
		return false;

	return (ofc_colstr_atomic_writef(cs, "/")
		&& ofc_str_ref_print(cs, record->structure)
		&& ofc_colstr_atomic_writef(cs, "/ ")
		&& ofc_parse_lhs_print(cs, record->name, true));
}



ofc_parse_record_list_t* ofc_parse_record_list(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	ofc_parse_record_list_t* list
		= (ofc_parse_record_list_t*)malloc(
			sizeof(ofc_parse_record_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->record = NULL;

	unsigned i = ofc_parse_list(
		src, ptr, debug, ',',
		&list->count, (void***)&list->record,
		(void*)ofc_parse_record,
		(void*)ofc_parse_record_delete);
	if (i == 0)
	{
		free(list);
		return NULL;
	}

	if (len) *len = i;
	return list;
}

void ofc_parse_record_list_delete(
	ofc_parse_record_list_t* list)
{
	if (!list)
		return;

	ofc_parse_list_delete(
		list->count, (void**)list->record,
		(void*)ofc_parse_record_delete);
	free(list);
}

bool ofc_parse_record_list_print(
	ofc_colstr_t* cs, const ofc_parse_record_list_t* list)
{
	if (!list)
		return false;

	return ofc_parse_list_print(cs,
		list->count, (const void**)list->record,
		(void*)ofc_parse_record_print);
}
