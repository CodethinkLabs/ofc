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

#ifndef __ofc_sema_parameter_h__
#define __ofc_sema_parameter_h__

typedef struct
{
	ofc_str_ref_t       name;
	ofc_sema_typeval_t* typeval;
} ofc_sema_parameter_t;

ofc_hashmap_t* ofc_sema_parameter_map_create(
	bool case_sensitive);

bool ofc_sema_parameter(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
bool ofc_sema_parameter_decl(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
void ofc_sema_parameter_delete(
	ofc_sema_parameter_t* parameter);

ofc_sema_parameter_t* ofc_sema_parameter_create(
	ofc_str_ref_t name,
	ofc_sema_typeval_t* typeval);
ofc_sema_parameter_t* ofc_sema_parameter_assign(
	ofc_sema_scope_t* scope,
	const ofc_parse_assign_t* assign);

const ofc_str_ref_t* ofc_sema_parameter_name(
	const ofc_sema_parameter_t* parameter);

const ofc_sema_type_t* ofc_sema_parameter_type(
	const ofc_sema_parameter_t* parameter);

const ofc_sema_typeval_t* ofc_sema_parameter_get(
	const ofc_sema_parameter_t* parameter);

bool ofc_sema_parameter_int32(
	const ofc_sema_parameter_t* parameter,
	int32_t* value);

#endif
