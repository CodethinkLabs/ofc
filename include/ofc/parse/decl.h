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

#ifndef __ofc_parse_decl_h__
#define __ofc_parse_decl_h__

typedef struct
{
    bool is_static;
	bool is_automatic;
	bool is_volatile;
	bool is_intrinsic;
	bool is_external;
} ofc_parse_decl_attr_t;

static const ofc_parse_decl_attr_t ofc_parse_decl_attr_default =
{
	.is_static    = false,
	.is_automatic = false,
	.is_volatile  = false,
	.is_intrinsic = false,
	.is_external  = false,
};

unsigned ofc_parse_decl_attr(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_decl_attr_t* attr);
bool ofc_parse_decl_attr_print(
	ofc_colstr_t* cs, const ofc_parse_decl_attr_t* attr);


typedef struct
{
	ofc_sparse_ref_t record;

	ofc_parse_lhs_t* lhs;

	ofc_parse_expr_t*      init_expr;
	ofc_parse_expr_list_t* init_clist;
} ofc_parse_decl_t;

typedef struct
{
	unsigned           count;
	ofc_parse_decl_t** decl;
} ofc_parse_decl_list_t;


ofc_parse_decl_t* ofc_parse_decl(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
ofc_parse_decl_t* ofc_parse_decl_f90(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
ofc_parse_decl_t* ofc_parse_decl_record(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
void ofc_parse_decl_delete(
	ofc_parse_decl_t* decl);
bool ofc_parse_decl_print(
	ofc_colstr_t* cs, const ofc_parse_decl_t* decl);

ofc_parse_decl_list_t* ofc_parse_decl_list(
	const ofc_sparse_t* src, const char* ptr,
	bool is_f90,
	ofc_parse_debug_t* debug,
	unsigned* len);
ofc_parse_decl_list_t* ofc_parse_decl_list_record(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
void ofc_parse_decl_list_delete(
	ofc_parse_decl_list_t* decl);
bool ofc_parse_decl_list_print(
	ofc_colstr_t* cs, const ofc_parse_decl_list_t* list);

#endif
