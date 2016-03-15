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

#ifndef __ofc_sema_implicit_h__
#define __ofc_sema_implicit_h__

ofc_sema_implicit_t* ofc_sema_implicit_create(void);
ofc_sema_implicit_t* ofc_sema_implicit_copy(
	const ofc_sema_implicit_t* implicit);
void ofc_sema_implicit_delete(
	ofc_sema_implicit_t* implicit);

bool ofc_sema_implicit(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);

const ofc_sema_type_t* ofc_sema_implicit_type(
	const ofc_sema_implicit_t* implicit,
	ofc_sparse_ref_t name);
bool ofc_sema_implicit_static(
	const ofc_sema_implicit_t* implicit,
	ofc_sparse_ref_t name);
bool ofc_sema_implicit_automatic(
	const ofc_sema_implicit_t* implicit,
	ofc_sparse_ref_t name);
bool ofc_sema_implicit_volatile(
	const ofc_sema_implicit_t* implicit,
	ofc_sparse_ref_t name);
bool ofc_sema_implicit_intrinsic(
	const ofc_sema_implicit_t* implicit,
	ofc_sparse_ref_t name);
bool ofc_sema_implicit_external(
	const ofc_sema_implicit_t* implicit,
	ofc_sparse_ref_t name);

bool ofc_sema_implicit_attr(
	const ofc_sema_implicit_t* implicit,
	ofc_sparse_ref_t name,
	const ofc_sema_type_t** type,
	bool* is_static,
	bool* is_automatic,
	bool* is_volatile,
	bool* is_intrinsic,
	bool* is_external);

#endif
