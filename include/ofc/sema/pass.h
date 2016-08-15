/* Copyright 2016 Codethink Ltd.
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

#ifndef __ofc_sema_pass_h__
#define __ofc_sema_pass_h__

#include"ofc/sema_pass_opts.h"

bool ofc_sema_pass_struct_type(
	ofc_sema_scope_t* scope);
bool ofc_sema_pass_unref_label(
	ofc_sema_scope_t* scope);
bool ofc_sema_pass_unlabelled_format(
	ofc_sema_scope_t* scope);
bool ofc_sema_pass_unlabelled_continue(
	ofc_sema_scope_t* scope);
bool ofc_sema_pass_char_transfer(
	ofc_sema_scope_t* scope);
bool ofc_sema_pass_unused_common(
	ofc_sema_scope_t* scope);
bool ofc_sema_pass_unused_decl(
	ofc_sema_scope_t* scope);
bool ofc_sema_pass_integer_logical(
	ofc_sema_scope_t* scope);

bool ofc_sema_run_passes(
	ofc_file_t* file,
	ofc_sema_pass_opts_t* sema_pass_opts,
	ofc_sema_scope_t* scope);

#endif
