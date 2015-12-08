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

#ifndef __ofc_sema_structure_h__
#define __ofc_sema_structure_h__

typedef struct
{
	bool is_vax;
	bool is_union;

	struct
	{
		unsigned                count;
		const ofc_sema_type_t** type;
		ofc_str_ref_t*          name;
	} member;

	bool locked;
} ofc_sema_structure_t;

ofc_sema_structure_t* ofc_sema_structure_create(bool is_vax);
ofc_sema_structure_t* ofc_sema_structure_create_union(void);

bool ofc_sema_structure_append(
	ofc_sema_structure_t*  structure,
	const ofc_sema_type_t* type, ofc_str_ref_t name);

void ofc_sema_structure_delete(ofc_sema_structure_t* structure);

uint8_t ofc_sema_structure_hash(
	const ofc_sema_structure_t* structure);

bool ofc_sema_structure_compare(
	const ofc_sema_structure_t* a,
	const ofc_sema_structure_t* b);

bool ofc_sema_structure_size(
	const ofc_sema_structure_t* structure,
	unsigned* size);
bool ofc_sema_structure_elem_count(
	const ofc_sema_structure_t* structure,
	unsigned* count);

#endif
