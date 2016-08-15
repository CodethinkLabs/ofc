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

#ifndef __ofc_sema_pass_opts_h__
#define __ofc_sema_pass_opts_h__

typedef struct
{
	bool struct_type;
	bool char_transfer;
	bool unref_label;
	bool unlabelled_format;
	bool unlabelled_continue;
	bool unused_common;
	bool integer_logical;

	bool unused_decl;
} ofc_sema_pass_opts_t;

static const ofc_sema_pass_opts_t
	OFC_SEMA_PASS_OPTS_DEFAULT =
{
	.struct_type         = true,
	.char_transfer       = true,
	.unref_label         = true,
	.unlabelled_format   = true,
	.unlabelled_continue = true,
	.unused_common       = true,
	.integer_logical     = true,

	.unused_decl         = false,
};

#endif
