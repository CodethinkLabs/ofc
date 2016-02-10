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
	OFC_SEMA_LABEL_FORMAT,
	OFC_SEMA_LABEL_END_BLOCK,

	OFC_SEMA_LABEL_COUNT
} ofc_sema_label_e;

typedef struct
{
	ofc_sema_label_e type;
	unsigned         number;

	union
	{
		const ofc_sema_stmt_t* stmt;
		ofc_sema_format_t*     format;
	};
} ofc_sema_label_t;

struct ofc_sema_format_label_list_s
{
	unsigned count;
	ofc_sema_label_t** format;
};

typedef struct
{
	/* This references statements and formats. */
	ofc_hashmap_t*                label;

	/* This owns the statement labels. */
	ofc_hashmap_t*                stmt;

	/* This owns the end block labels. */
	ofc_hashmap_t*                end_block;

	/* This owns the format labels. */
	ofc_sema_format_label_list_t* format;
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
bool ofc_sema_label_map_add_format(
	const ofc_parse_stmt_t* stmt,
	ofc_sema_label_map_t* map, unsigned label,
	ofc_sema_format_t* format);

const ofc_sema_label_t* ofc_sema_label_map_find(
	const ofc_sema_label_map_t* map, unsigned label);
const ofc_sema_label_t* ofc_sema_label_map_find_stmt(
	const ofc_sema_label_map_t* map,
	const ofc_sema_stmt_t*      stmt);
const ofc_sema_label_t* ofc_sema_label_map_find_end_block(
	const ofc_sema_label_map_t* map,
	const ofc_sema_stmt_t*      stmt);

ofc_sema_format_label_list_t* ofc_sema_format_label_list_create(void);
void ofc_sema_format_label_list_delete(
	ofc_sema_format_label_list_t* label_list);

bool ofc_sema_format_label_list_add(
	ofc_sema_format_label_list_t* list,
	ofc_sema_label_t* format);
bool ofc_sema_format_label_list_print(
	ofc_colstr_t* cs, unsigned indent,
	ofc_sema_format_label_list_t* list);
bool ofc_sema_format_label_print(ofc_colstr_t* cs,
	ofc_sema_label_t* label);

#endif
