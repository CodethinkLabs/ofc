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

#ifndef __ofc_sema_array_h__
#define __ofc_sema_array_h__


typedef struct
{
	ofc_sema_expr_t* first;
	ofc_sema_expr_t* last;
} ofc_sema_array_dims_t;

typedef struct
{
	bool                  scan;
	unsigned              dimensions;
	ofc_sema_array_dims_t segment[0];
} ofc_sema_array_t;

ofc_sema_array_t* ofc_sema_array(
	ofc_sema_scope_t*              scope,
	const ofc_parse_array_index_t* index);
ofc_sema_array_t* ofc_sema_array_scan(
	ofc_sema_scope_t*              scope,
	const ofc_parse_array_index_t* index);
ofc_sema_array_t* ofc_sema_array_array(
	ofc_sema_scope_t* scope,
	const ofc_parse_expr_list_t* list);

ofc_sema_array_t* ofc_sema_array_copy_replace(
	const ofc_sema_array_t* array,
	const ofc_sema_decl_t*  replace,
	const ofc_sema_expr_t*  with);
ofc_sema_array_t* ofc_sema_array_copy(
	const ofc_sema_array_t* array);

void ofc_sema_array_delete(ofc_sema_array_t* array);

bool ofc_sema_array_compare(
	const ofc_sema_array_t* a,
	const ofc_sema_array_t* b);

bool ofc_sema_array_total(
	const ofc_sema_array_t* array,
	unsigned* total);

bool ofc_sema_array_print(
	ofc_colstr_t* cs,
	const ofc_sema_array_t* array);
bool ofc_sema_array_print_size(
	ofc_colstr_t* cs,
	const ofc_sema_array_t* array);
bool ofc_sema_array_print_brackets(
	ofc_colstr_t* cs,
	const ofc_sema_array_t* array);

bool ofc_sema_array_foreach_expr(
	ofc_sema_array_t* array, void* param,
	bool (*func)(ofc_sema_expr_t* expr, void* param));


typedef struct
{
	unsigned         dimensions;
	ofc_sema_expr_t* index[0];
} ofc_sema_array_index_t;

bool ofc_sema_array_index_print(
	ofc_colstr_t* cs,
	const ofc_sema_array_index_t* index);

ofc_sema_array_index_t* ofc_sema_array_index(
	ofc_sema_scope_t*              scope,
	const ofc_sema_array_t*        array,
	const ofc_parse_array_index_t* index);
ofc_sema_array_index_t* ofc_sema_array_index_copy_replace(
	const ofc_sema_array_index_t* index,
	const ofc_sema_decl_t*        replace,
	const ofc_sema_expr_t*        with);
ofc_sema_array_index_t* ofc_sema_array_index_copy(
	const ofc_sema_array_index_t* index);
void ofc_sema_array_index_delete(
	ofc_sema_array_index_t* index);

ofc_sema_array_index_t* ofc_sema_array_index_from_offset(
	const ofc_sema_decl_t* decl, unsigned offset);

bool ofc_sema_array_index_offset(
	const ofc_sema_decl_t*        decl,
	const ofc_sema_array_index_t* index,
	unsigned* offset);

bool ofc_sema_array_index_compare(
	const ofc_sema_array_index_t* a,
	const ofc_sema_array_index_t* b);


typedef struct
{
	bool is_index;
	ofc_sema_expr_t* first;
	ofc_sema_expr_t* last;
	ofc_sema_expr_t* stride;
} ofc_sema_array_segment_t;

typedef struct
{
	unsigned                 dimensions;
	ofc_sema_array_segment_t segment[0];
} ofc_sema_array_slice_t;

ofc_sema_array_slice_t* ofc_sema_array_slice(
	ofc_sema_scope_t*              scope,
	const ofc_sema_array_t*        array,
	const ofc_parse_array_index_t* index);
ofc_sema_array_slice_t* ofc_sema_array_slice_copy_replace(
	const ofc_sema_array_slice_t* slice,
	const ofc_sema_decl_t*        replace,
	const ofc_sema_expr_t*        with);
ofc_sema_array_slice_t* ofc_sema_array_slice_copy(
	const ofc_sema_array_slice_t* slice);
void ofc_sema_array_slice_delete(
	ofc_sema_array_slice_t* slice);

bool ofc_sema_array_slice_compare(
	const ofc_sema_array_slice_t* a,
	const ofc_sema_array_slice_t* b);

ofc_sema_array_t* ofc_sema_array_slice_dims(
	const ofc_sema_array_slice_t* slice,
	const ofc_sema_array_t* array);

ofc_sema_array_index_t* ofc_sema_array_slice_index_from_offset(
	const ofc_sema_array_slice_t* slice, unsigned offset);

bool ofc_sema_array_slice_print(
	ofc_colstr_t* cs,
	const ofc_sema_array_slice_t* slice);

#endif
