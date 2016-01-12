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

#ifndef __ofc_sema_arg_h__
#define __ofc_sema_arg_h__

typedef struct
{
	bool             alt_return;
	ofc_sparse_ref_t name;
} ofc_sema_arg_t;

typedef struct
{
	unsigned        count;
	ofc_sema_arg_t* arg;
} ofc_sema_arg_list_t;


ofc_sema_arg_list_t* ofc_sema_arg_list(
	const ofc_parse_call_arg_list_t* plist);
ofc_sema_arg_list_t* ofc_sema_arg_list_stmt_func(
	const ofc_parse_array_index_t* index);
bool ofc_sema_arg_list_print(
	ofc_colstr_t* cs,
	const ofc_sema_arg_list_t* list);
void ofc_sema_arg_list_delete(
	ofc_sema_arg_list_t* list);

#endif
