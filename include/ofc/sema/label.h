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

#ifndef __ofc_sema_label_h__
#define __ofc_sema_label_h__

typedef enum
{
	OFC_SEMA_LABEL_STMT = 0,
	OFC_SEMA_LABEL_END_BLOCK,
	OFC_SEMA_LABEL_END_SCOPE,

	OFC_SEMA_LABEL_COUNT
} ofc_sema_label_e;

typedef struct
{
	ofc_sema_label_e type;
	unsigned         number;

	union
	{
		const ofc_sema_scope_t* scope;
		const ofc_sema_stmt_t*  stmt;
	};

	bool used;
} ofc_sema_label_t;

ofc_sparse_ref_t ofc_sema_label_src(
	const ofc_sema_label_t* label);


typedef struct
{
	unsigned size;
	unsigned count;

	ofc_sema_label_t** label;

	/* These references all labels. */
	ofc_hashmap_t* map;

	/* This owns the statement labels. */
	ofc_hashmap_t* stmt;

	/* These own the end block/scope labels. */
	ofc_hashmap_t* end_block;
	ofc_hashmap_t* end_scope;
} ofc_sema_label_map_t;

ofc_sema_label_map_t* ofc_sema_label_map_create(void);
void ofc_sema_label_map_delete(
	ofc_sema_label_map_t* label_map);

bool ofc_sema_label_map_add_stmt(
	ofc_sema_label_map_t* map, unsigned label,
	const ofc_sema_stmt_t* stmt);
bool ofc_sema_label_map_add_end_block(
	ofc_sema_label_map_t* map, unsigned label,
	const ofc_sema_stmt_t* end_block);
bool ofc_sema_label_map_add_end_scope(
	ofc_sema_label_map_t* map, unsigned label,
	const ofc_sema_scope_t* end_scope);

const ofc_sema_label_t* ofc_sema_label_map_find(
	const ofc_sema_label_map_t* map, unsigned label);
ofc_sema_label_t* ofc_sema_label_map_find_modify(
	ofc_sema_label_map_t* map, unsigned label);

const ofc_sema_label_t* ofc_sema_label_map_find_stmt(
	const ofc_sema_label_map_t* map,
	const ofc_sema_stmt_t*      stmt);
const ofc_sema_label_t* ofc_sema_label_map_find_end_block(
	const ofc_sema_label_map_t* map,
	const ofc_sema_stmt_t*      stmt);
const ofc_sema_label_t* ofc_sema_label_map_find_end_scope(
	const ofc_sema_label_map_t* map,
	const ofc_sema_scope_t*     scope);

void ofc_sema_label_map_remove(
	ofc_sema_label_map_t* map, ofc_sema_label_t* label);

#endif
