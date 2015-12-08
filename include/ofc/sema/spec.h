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

#ifndef __ofc_sema_spec_h__
#define __ofc_sema_spec_h__

struct ofc_sema_spec_s
{
	ofc_str_ref_t name;

	bool            type_implicit;
	ofc_sema_type_e type;

	unsigned kind;

	unsigned len;
	bool     len_var;

	ofc_sema_array_t* array;

	bool is_static;
	bool is_automatic;
	bool is_volatile;
	bool is_intrinsic;
	bool is_external;

	ofc_sema_common_t* common;
	unsigned           common_offset;
};

const ofc_sema_spec_t OFC_SEMA_SPEC_DEFAULT;

ofc_sema_spec_t* ofc_sema_spec_create(ofc_str_ref_t name);
ofc_sema_spec_t* ofc_sema_spec(
	ofc_sema_scope_t*       scope,
	const ofc_parse_type_t* ptype);
ofc_sema_spec_t* ofc_sema_spec_copy(
	const ofc_sema_spec_t* spec);
void ofc_sema_spec_delete(
	ofc_sema_spec_t* spec);

ofc_hashmap_t* ofc_sema_spec_map_create(
	bool case_sensitive);

#endif
