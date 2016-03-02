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

#ifndef __ofc_parse_type_h__
#define __ofc_parse_type_h__

#include "expr.h"

typedef enum
{
	OFC_PARSE_TYPE_NONE = 0,
	OFC_PARSE_TYPE_LOGICAL,
	OFC_PARSE_TYPE_CHARACTER,
	OFC_PARSE_TYPE_INTEGER,
	OFC_PARSE_TYPE_REAL,
	OFC_PARSE_TYPE_DOUBLE_PRECISION,
	OFC_PARSE_TYPE_COMPLEX,
	OFC_PARSE_TYPE_DOUBLE_COMPLEX,
	OFC_PARSE_TYPE_BYTE,
	OFC_PARSE_TYPE_TYPE,
	OFC_PARSE_TYPE_RECORD,

	OFC_PARSE_TYPE_COUNT,
} ofc_parse_type_e;

typedef struct
{
	ofc_sparse_ref_t           src;

	ofc_parse_type_e           type;
	ofc_sparse_ref_t           type_name;
	unsigned                   size;
	ofc_parse_expr_t*          count_expr;
	bool                       count_var;
	ofc_parse_call_arg_list_t* params;
} ofc_parse_type_t;

const char* ofc_parse_type_str_rep(
	const ofc_parse_type_e type);

ofc_parse_type_t* ofc_parse_type(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
void ofc_parse_type_delete(ofc_parse_type_t* type);

bool ofc_parse_type_print(
	ofc_colstr_t* cs, const ofc_parse_type_t* type, bool colons);
bool ofc_parse_type_print_f77(
	ofc_colstr_t* cs, const ofc_parse_type_t* type);

#endif
