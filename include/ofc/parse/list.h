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

#ifndef __ofc_parse_list_h__
#define __ofc_parse_list_h__

#include "../string.h"

unsigned ofc_parse_list(
	const ofc_sparse_t* sparse, const char* ptr,
	ofc_parse_debug_t* debug,
	char seperator, unsigned* elem_count, void*** elem,
	void* (*elem_parse)(const ofc_sparse_t*, const char*, ofc_parse_debug_t*, unsigned*),
	void (*elem_delete)(void*));

unsigned ofc_parse_list_seperator_optional(
	const ofc_sparse_t* sparse, const char* ptr,
	ofc_parse_debug_t* debug,
	char seperator, unsigned* elem_count, void*** elem,
	void* (*elem_parse)(const ofc_sparse_t*, const char*, ofc_parse_debug_t*, unsigned*),
	void (*elem_delete)(void*));

bool ofc_parse_list_copy(
	unsigned* dst_count, void*** dst,
	unsigned  src_count, const void** src,
	void* (*elem_copy)(const void*),
	void (*elem_delete)(void*));

void ofc_parse_list_delete(
	unsigned elem_count, void** elem,
	void (*elem_delete)(void*));

bool ofc_parse_list_print(
	ofc_colstr_t* cs,
	unsigned elem_count, const void** elem,
	bool (*elem_print)(ofc_colstr_t*, const void*));

#endif
