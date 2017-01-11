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

#ifndef __ofc_sema_intrinsic_h__
#define __ofc_sema_intrinsic_h__

typedef struct ofc_sema_intrinsic_s ofc_sema_intrinsic_t;

bool ofc_sema_intrinsic_name_reserved(const char* name);

const ofc_sema_intrinsic_t* ofc_sema_intrinsic(
	ofc_str_ref_t name, bool case_sensitive);

ofc_sema_dummy_arg_list_t* ofc_sema_intrinsic_cast(
	ofc_sparse_ref_t src,
	const ofc_sema_intrinsic_t* intrinsic,
	const ofc_sema_dummy_arg_list_t* args);

ofc_sema_typeval_t* ofc_sema_intrinsic_constant(
	const ofc_sema_intrinsic_t* intrinsic,
	const ofc_sema_dummy_arg_list_t* args);

const ofc_sema_type_t* ofc_sema_intrinsic_type(
	const ofc_sema_intrinsic_t* intrinsic,
	const ofc_sema_dummy_arg_list_t* args);

const ofc_sema_intrinsic_t* ofc_sema_intrinsic_cast_func(
	const ofc_sema_type_t* type);

bool ofc_sema_intrinsic_is_specific(
	const ofc_sema_intrinsic_t* func);

bool ofc_sema_intrinsic_print(
	ofc_colstr_t* cs,
	const ofc_sema_intrinsic_t* intrinsic);

#endif
