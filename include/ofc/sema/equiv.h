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
	ofc_hashmap_t*   map;

	unsigned refcnt;
} ofc_sema_equiv_t;

bool ofc_sema_equiv(
	ofc_sema_lhs_t* a,
	ofc_sema_lhs_t* b);

void ofc_sema_equiv_delete(
	ofc_sema_equiv_t* equiv);

#endif
