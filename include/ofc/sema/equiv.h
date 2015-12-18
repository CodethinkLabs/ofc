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

#ifndef __ofc_sema_equiv_h__
#define __ofc_sema_equiv_h__

typedef struct
{
	unsigned         count;
	ofc_sema_lhs_t** lhs;
} ofc_sema_equiv_t;

ofc_sema_equiv_t* ofc_sema_equiv_create(void);
void ofc_sema_equiv_delete(
	ofc_sema_equiv_t* equiv);

bool ofc_sema_equiv_add(
	ofc_sema_equiv_t* equiv, ofc_sema_lhs_t* lhs);

bool ofc_sema_equiv_print(
	ofc_colstr_t* cs, const ofc_sema_equiv_t* equiv);


typedef struct
{
	unsigned           count;
	ofc_sema_equiv_t** equiv;
} ofc_sema_equiv_list_t;

ofc_sema_equiv_list_t* ofc_sema_equiv_list_create(void);
void ofc_sema_equiv_list_delete(
	ofc_sema_equiv_list_t* list);

bool ofc_sema_equiv_list_add(
	ofc_sema_equiv_list_t* list,
	ofc_sema_equiv_t* equiv);

bool ofc_sema_equiv_list_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_sema_equiv_list_t* list);

#endif
