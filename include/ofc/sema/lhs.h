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

#ifndef __ofc_sema_lhs_h__
#define __ofc_sema_lhs_h__

typedef enum
{
	OFC_SEMA_LHS_DECL,
	OFC_SEMA_LHS_ARRAY_INDEX,
	OFC_SEMA_LHS_ARRAY_SLICE,
	OFC_SEMA_LHS_SUBSTRING,
	OFC_SEMA_LHS_STRUCTURE_MEMBER,
} ofc_sema_lhs_e;

struct ofc_sema_lhs_s
{
	ofc_sema_lhs_e type;

	ofc_sparse_ref_t src;

	union
	{
		ofc_sema_decl_t* decl;

		struct
		{
			ofc_sema_lhs_t* parent;

			union
			{
				ofc_sema_array_slice_t* slice;

				ofc_sema_array_index_t* index;

				struct
				{
					ofc_sema_expr_t* first;
					ofc_sema_expr_t* last;
				} substring;

				ofc_str_ref_t member;
			};
		};
	};

	const ofc_sema_type_t* data_type;

	unsigned refcnt;
};

ofc_sema_lhs_t* ofc_sema_lhs(
	ofc_sema_scope_t* scope,
	const ofc_parse_lhs_t* lhs);
ofc_sema_lhs_t* ofc_sema_lhs_from_expr(
	ofc_sema_scope_t* scope,
	ofc_parse_expr_t* expr);
ofc_sema_lhs_t* ofc_sema_lhs_in_expr(
	ofc_sema_scope_t* scope,
	const ofc_parse_lhs_t* lhs);
bool ofc_sema_lhs_reference(
	ofc_sema_lhs_t* lhs);
void ofc_sema_lhs_delete(
	ofc_sema_lhs_t* lhs);

bool ofc_sema_lhs_init(
	const ofc_sema_scope_t* scope,
	ofc_sema_lhs_t* lhs,
	const ofc_sema_expr_t* init);
bool ofc_sema_lhs_init_array(
	ofc_sema_lhs_t* lhs,
	const ofc_sema_array_t* array,
	unsigned count,
	const ofc_sema_expr_t** init);

bool ofc_sema_lhs_mark_used(
	ofc_sema_lhs_t* lhs);

bool ofc_sema_lhs_compare(
	const ofc_sema_lhs_t* a,
	const ofc_sema_lhs_t* b);

ofc_sema_decl_t* ofc_sema_lhs_decl(
	ofc_sema_lhs_t* lhs);
const ofc_sema_type_t* ofc_sema_lhs_type(
	const ofc_sema_lhs_t* lhs);

bool ofc_sema_lhs_elem_count(
	const ofc_sema_lhs_t* lhs,
	unsigned* count);

bool ofc_sema_lhs_equiv(
	ofc_sema_lhs_t* a,
	ofc_sema_lhs_t* b);

bool ofc_sema_lhs_print(
	ofc_colstr_t* cs,
	const ofc_sema_lhs_t* lhs);

#endif
