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

#ifndef __ofc_parse_file_h__
#define __ofc_parse_file_h__

#include <ofc/parse.h>

typedef struct
{
	ofc_sparse_t*          source;
	ofc_parse_stmt_list_t* stmt;
} ofc_parse_file_t;

bool ofc_parse_file_include(
	const ofc_sparse_t*    src,
	ofc_parse_stmt_list_t* list,
	ofc_parse_debug_t*     debug);

ofc_parse_file_t* ofc_parse_file(ofc_sparse_t* src);
void ofc_parse_file_delete(ofc_parse_file_t* file);

bool ofc_parse_file_print(
	ofc_colstr_t* cs,
	const ofc_parse_file_t* file);

#endif
