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

#ifndef __ofc_parse_lhs_h__
#define __ofc_parse_lhs_h__

#include <stdint.h>
#include "../str_ref.h"


typedef enum
{
	OFC_PARSE_LHS_VARIABLE,
	OFC_PARSE_LHS_ARRAY,
	OFC_PARSE_LHS_STAR_LEN,
	OFC_PARSE_LHS_MEMBER_TYPE,
	OFC_PARSE_LHS_MEMBER_STRUCTURE,
	OFC_PARSE_LHS_IMPLICIT_DO,
	OFC_PARSE_LHS_ALIAS,
} ofc_parse_lhs_e;


struct ofc_parse_lhs_s
{
	ofc_parse_lhs_e type;

	ofc_sparse_ref_t src;

	union
	{
		ofc_sparse_ref_t variable;

		struct
		{
			ofc_parse_lhs_t* parent;

			union
			{
				struct
				{
					ofc_parse_array_index_t* index;
				} array;

				struct
				{
					ofc_sparse_ref_t name;
				} member;

				struct
				{
					ofc_parse_expr_t* len;
					bool              var;
				} star_len;
			};
		};

		struct
		{
			ofc_sparse_ref_t target;
			ofc_sparse_ref_t name;
		} alias;

		ofc_parse_lhs_implicit_do_t* implicit_do;
	};
};

typedef struct
{
	unsigned          count;
	ofc_parse_lhs_t** lhs;
} ofc_parse_lhs_list_t;

ofc_parse_lhs_t* ofc_parse_lhs_star_len(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
ofc_parse_lhs_t* ofc_parse_lhs_variable(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
ofc_parse_lhs_t* ofc_parse_lhs(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
ofc_parse_lhs_t* ofc_parse_lhs_copy(
	ofc_parse_lhs_t* lhs);
void ofc_parse_lhs_delete(
	ofc_parse_lhs_t* lhs);

bool ofc_parse_lhs_print(
	ofc_colstr_t* cs, const ofc_parse_lhs_t* lhs,
	bool is_decl);

bool ofc_parse_lhs_base_name(
	const ofc_parse_lhs_t lhs,
	ofc_sparse_ref_t* name);

bool ofc_parse_lhs_possible_function_call(
	const ofc_parse_lhs_t lhs);

ofc_parse_lhs_list_t* ofc_parse_lhs_list(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
ofc_parse_lhs_list_t* ofc_parse_lhs_list_bracketed(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
ofc_parse_lhs_list_t* ofc_parse_lhs_alias_list(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
ofc_parse_lhs_list_t* ofc_parse_lhs_list_copy(
	const ofc_parse_lhs_list_t* list);
void ofc_parse_lhs_list_delete(
	ofc_parse_lhs_list_t* list);
bool ofc_parse_lhs_list_print(
	ofc_colstr_t* cs, const ofc_parse_lhs_list_t* list,
	bool is_decl);
bool ofc_parse_lhs_list_bracketed_print(
	ofc_colstr_t* cs, const ofc_parse_lhs_list_t* list,
	bool is_decl);

#endif
