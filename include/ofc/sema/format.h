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

#ifndef __ofc_sema_format_h__
#define __ofc_sema_format_h__

#include "ofc/sema/type.h"

typedef struct
{
	const ofc_parse_format_desc_list_t* src;
	ofc_parse_format_desc_list_t*       format;
	bool is_default_possible;
} ofc_sema_format_t;

const char* ofc_sema_format_str_rep(
	const ofc_parse_format_desc_e type);

bool ofc_sema_compare_desc_expr_type(
	ofc_parse_format_desc_e type_desc,
	ofc_sema_type_e type_expr);

bool ofc_sema_format_desc(
	const ofc_parse_format_desc_t* desc);

const ofc_sema_type_t* ofc_sema_format_desc_type(
	const ofc_parse_format_desc_t* desc);

ofc_parse_format_desc_t* ofc_sema_format_desc_set_def(
	const ofc_parse_format_desc_t* desc,
	const ofc_sema_expr_t* expr,
	const ofc_sema_lhs_t* lhs);
#endif
