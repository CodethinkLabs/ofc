/* Copyright 2016 Codethink Ltd.
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

#ifndef __ofc_sema_range_h__
#define __ofc_sema_range_h__

typedef struct
{
	ofc_sparse_ref_t src;

	bool is_range;
	ofc_sema_expr_t* first;
	ofc_sema_expr_t* last;
} ofc_sema_range_t;

typedef struct
{
	ofc_sparse_ref_t src;

	unsigned           count;
	ofc_sema_range_t** range;
} ofc_sema_range_list_t;

ofc_sema_range_t* ofc_sema_range(
	ofc_sema_scope_t* scope,
	ofc_parse_array_range_t* range);
ofc_sema_range_list_t* ofc_sema_range_list(
	ofc_sema_scope_t* scope,
	ofc_parse_array_index_t* index);

bool ofc_sema_range_list_add(
	ofc_sema_range_list_t* list,
	ofc_sema_range_t* range);

bool ofc_sema_range_is_constant(
	ofc_sema_range_t* range);
bool ofc_sema_range_intersects(
	ofc_sema_range_t* a,
	ofc_sema_range_t* b);
bool ofc_sema_range_list_intersects(
	ofc_sema_range_list_t* a,
	ofc_sema_range_list_t* b);

bool ofc_sema_range_print(
	ofc_colstr_t* cs,
	const ofc_sema_range_t* range);
bool ofc_sema_range_list_print(
	ofc_colstr_t* cs,
	const ofc_sema_range_list_t* range);

void ofc_sema_range_delete(
	ofc_sema_range_t* range);
void ofc_sema_range_list_delete(
	ofc_sema_range_list_t* list);
#endif
