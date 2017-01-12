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

#ifndef __ofc_sema_dummy_arg_h__
#define __ofc_sema_dummy_arg_h__

typedef enum
{
	OFC_SEMA_DUMMY_ARG_EXTERNAL,
	OFC_SEMA_DUMMY_ARG_EXPR,

	OFC_SEMA_DUMMY_ARG_COUNT

} ofc_sema_dummy_arg_e;

typedef struct
{
	ofc_sema_dummy_arg_e type;

	ofc_sparse_ref_t src;

	union
	{
		const ofc_sema_external_t* external;
		ofc_sema_expr_t*     expr;
	};
} ofc_sema_dummy_arg_t;

typedef struct
{
	unsigned               count;
	ofc_sema_dummy_arg_t** dummy_arg;
} ofc_sema_dummy_arg_list_t;

ofc_sema_dummy_arg_t* ofc_sema_dummy_arg(
	ofc_sema_scope_t* scope,
	const ofc_parse_expr_t* expr);
ofc_sema_dummy_arg_t* ofc_sema_dummy_arg_alt_return(
	ofc_sema_scope_t* scope,
	const ofc_parse_expr_t* expr);
ofc_sema_dummy_arg_t* ofc_sema_dummy_arg_copy(
	const ofc_sema_dummy_arg_t* dummy_arg);
ofc_sema_dummy_arg_t* ofc_sema_dummy_arg_cast(
	const ofc_sema_dummy_arg_t* dummy_arg,
	const ofc_sema_type_t* type);
void ofc_sema_dummy_arg_delete(
	ofc_sema_dummy_arg_t* arg);

bool ofc_sema_dummy_arg_compare(
	const ofc_sema_dummy_arg_t* a,
	const ofc_sema_dummy_arg_t* b);

bool ofc_sema_dummy_arg_is_alt_return(
	const ofc_sema_dummy_arg_t* dummy_arg);
bool ofc_sema_dummy_arg_is_external(
	const ofc_sema_dummy_arg_t* dummy_arg);
bool ofc_sema_dummy_arg_is_expr(
	const ofc_sema_dummy_arg_t* dummy_arg);

ofc_sema_expr_t* ofc_sema_dummy_arg_get_expr(
	ofc_sema_dummy_arg_t* dummy_arg);
ofc_sema_dummy_arg_t* ofc_sema_dummy_arg_wrap_expr(
	ofc_sema_expr_t* expr);

const ofc_sema_type_t* ofc_sema_dummy_arg_type(
	const ofc_sema_dummy_arg_t* dummy_arg);

bool ofc_sema_dummy_arg_mark_used(
	ofc_sema_dummy_arg_t* dummy_arg);

ofc_sema_dummy_arg_t* ofc_sema_dummy_arg_copy_replace(
	const ofc_sema_dummy_arg_t* dummy_arg,
	const ofc_sema_decl_t* replace,
	const ofc_sema_expr_t* with);
ofc_sema_dummy_arg_list_t* ofc_sema_dummy_arg_list_copy_replace(
	const ofc_sema_dummy_arg_list_t* list,
	const ofc_sema_decl_t* replace,
	const ofc_sema_expr_t* with);

ofc_sema_dummy_arg_list_t* ofc_sema_dummy_arg_list_create(void);
void ofc_sema_dummy_arg_list_delete(
	ofc_sema_dummy_arg_list_t* list);
bool ofc_sema_dummy_arg_list_add(
	ofc_sema_dummy_arg_list_t* list,
	ofc_sema_dummy_arg_t* dummy_arg);
bool ofc_sema_dummy_arg_list_compare(
	const ofc_sema_dummy_arg_list_t* a,
	const ofc_sema_dummy_arg_list_t* b);
bool ofc_sema_dummy_arg_list_foreach_expr(
	ofc_sema_dummy_arg_list_t* list, void* param,
	bool (*func)(ofc_sema_expr_t* expr, void* param));

bool ofc_sema_dummy_arg_list_print(
	ofc_colstr_t* cs,
	const ofc_sema_dummy_arg_list_t* list);

#endif
