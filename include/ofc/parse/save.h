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

#ifndef __ofc_parse_save_h__
#define __ofc_parse_save_h__

typedef struct
{
	bool is_common;
	union
	{
		ofc_parse_lhs_t* lhs;
		ofc_sparse_ref_t common;
	};
} ofc_parse_save_t;

typedef struct
{
	unsigned           count;
	ofc_parse_save_t** save;
} ofc_parse_save_list_t;


ofc_parse_save_t* ofc_parse_save(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
void ofc_parse_save_delete(
	ofc_parse_save_t* save);
bool ofc_parse_save_print(
	ofc_colstr_t* cs, const ofc_parse_save_t* save);

ofc_parse_save_list_t* ofc_parse_save_list(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
void ofc_parse_save_list_delete(
	ofc_parse_save_list_t* save);
bool ofc_parse_save_list_print(
	ofc_colstr_t* cs, const ofc_parse_save_list_t* list);

#endif
