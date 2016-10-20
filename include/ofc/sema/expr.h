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

#ifndef __ofc_sema_expr_h__
#define __ofc_sema_expr_h__

typedef enum
{
	OFC_SEMA_EXPR_CONSTANT = 0,
	OFC_SEMA_EXPR_LHS,
	OFC_SEMA_EXPR_CAST,
	OFC_SEMA_EXPR_INTRINSIC,
	OFC_SEMA_EXPR_FUNCTION,
	OFC_SEMA_EXPR_IMPLICIT_DO,
	OFC_SEMA_EXPR_ARRAY,
	OFC_SEMA_EXPR_RESHAPE,

	OFC_SEMA_EXPR_POWER,
	OFC_SEMA_EXPR_MULTIPLY,
	OFC_SEMA_EXPR_CONCAT,
	OFC_SEMA_EXPR_DIVIDE,
	OFC_SEMA_EXPR_ADD,
	OFC_SEMA_EXPR_SUBTRACT,
	OFC_SEMA_EXPR_NEGATE,
	OFC_SEMA_EXPR_EQ,
	OFC_SEMA_EXPR_NE,
	OFC_SEMA_EXPR_LT,
	OFC_SEMA_EXPR_LE,
	OFC_SEMA_EXPR_GT,
	OFC_SEMA_EXPR_GE,
	OFC_SEMA_EXPR_NOT,
	OFC_SEMA_EXPR_AND,
	OFC_SEMA_EXPR_OR,
	OFC_SEMA_EXPR_XOR,
	OFC_SEMA_EXPR_EQV,
	OFC_SEMA_EXPR_NEQV,

	OFC_SEMA_EXPR_COUNT
} ofc_sema_expr_e;

struct ofc_sema_expr_s
{
	ofc_sema_expr_e type;

	ofc_sparse_ref_t src;

	ofc_sema_typeval_t* constant;
	ofc_sema_label_t*   label;

	bool brackets;

	unsigned repeat;

	bool is_alt_return;
	bool is_label;
	bool is_format;

	union
	{
		ofc_sema_lhs_t* lhs;

		struct
		{
			const ofc_sema_type_t* type;
			ofc_sema_expr_t*       expr;
		} cast;

		struct
		{
			ofc_sema_expr_t* a;
			ofc_sema_expr_t* b;
		};

		struct
		{
			union
			{
				const ofc_sema_intrinsic_t* intrinsic;
				const ofc_sema_decl_t*      function;
			};
			ofc_sema_dummy_arg_list_t* args;
		};

		struct
		{
			ofc_sema_expr_list_t* expr;
			ofc_sema_decl_t* iter;
			ofc_sema_expr_t* init;
			ofc_sema_expr_t* last;
			ofc_sema_expr_t* step;

			bool     count_var;
			unsigned count;
		} implicit_do;

		ofc_sema_expr_list_t* array;

		struct
		{
			ofc_sema_expr_list_t* source;
			ofc_sema_array_t*     shape;
		} reshape;
	};
};

struct ofc_sema_expr_list_s
{
	unsigned          count;
	ofc_sema_expr_t** expr;
};

ofc_sema_expr_t* ofc_sema_expr(
	ofc_sema_scope_t* scope,
	const ofc_parse_expr_t* expr);
ofc_sema_expr_t* ofc_sema_expr_repeat(
	ofc_sema_scope_t* scope,
	const ofc_parse_expr_t* expr);
ofc_sema_expr_t* ofc_sema_expr_dummy_arg(
	ofc_sema_scope_t* scope,
	const ofc_parse_expr_t* expr);
ofc_sema_expr_t* ofc_sema_expr_label(
	ofc_sema_scope_t* scope,
	const ofc_parse_expr_t* expr);
ofc_sema_expr_t* ofc_sema_expr_alt_return(
	ofc_sema_scope_t* scope,
	const ofc_parse_expr_t* expr);

ofc_sema_expr_t* ofc_sema_expr_copy_replace(
	const ofc_sema_expr_t* expr,
	const ofc_sema_decl_t* replace,
	const ofc_sema_expr_t* with);
ofc_sema_expr_t* ofc_sema_expr_copy(
	const ofc_sema_expr_t* expr);

ofc_sema_expr_t* ofc_sema_expr_cast(
	ofc_sema_expr_t* expr,
	const ofc_sema_type_t* type);
ofc_sema_expr_t* ofc_sema_expr_cast_intrinsic(
	ofc_sema_expr_t* expr, const ofc_sema_type_t* type);

ofc_sema_expr_t* ofc_sema_expr_typeval(
	ofc_sema_typeval_t* typeval);

ofc_sema_expr_t* ofc_sema_expr_integer(
	int value, ofc_sema_kind_e kind);

ofc_sema_expr_t* ofc_sema_expr_wrap_lhs(
	ofc_sema_lhs_t* lhs);

void ofc_sema_expr_delete(
	ofc_sema_expr_t* expr);


const ofc_sema_array_t* ofc_sema_expr_array(
	const ofc_sema_expr_t* expr);
ofc_sema_structure_t* ofc_sema_expr_structure(
	const ofc_sema_expr_t* expr);
bool ofc_sema_expr_elem_count(
	const ofc_sema_expr_t* expr,
	unsigned* count);
ofc_sema_expr_t* ofc_sema_expr_elem_get(
	const ofc_sema_expr_t* expr, unsigned offset);

bool ofc_sema_expr_compare(
	const ofc_sema_expr_t* a,
	const ofc_sema_expr_t* b);
bool ofc_sema_expr_compare_def_one(
	const ofc_sema_expr_t* a,
	const ofc_sema_expr_t* b);

ofc_sema_expr_t* ofc_sema_expr_add(
	const ofc_sema_expr_t* a,
	const ofc_sema_expr_t* b);
ofc_sema_expr_t* ofc_sema_expr_sub(
	const ofc_sema_expr_t* a,
	const ofc_sema_expr_t* b);
ofc_sema_expr_t* ofc_sema_expr_div(
	const ofc_sema_expr_t* a,
	const ofc_sema_expr_t* b);

uint8_t ofc_sema_expr_hash(
	const ofc_sema_expr_t* expr);

const ofc_sema_type_t* ofc_sema_expr_type(
	const ofc_sema_expr_t* expr);
bool ofc_sema_expr_type_is_character(
	const ofc_sema_expr_t* expr);
bool ofc_sema_expr_type_is_integer(
	const ofc_sema_expr_t* expr);

const ofc_sema_typeval_t* ofc_sema_expr_constant(
	const ofc_sema_expr_t* expr);
bool ofc_sema_expr_is_constant(
	const ofc_sema_expr_t* expr);

bool ofc_sema_expr_validate_uint(
	const ofc_sema_expr_t* expr);

bool ofc_sema_expr_resolve_uint(
	const ofc_sema_expr_t* expr,
	unsigned* value);
bool ofc_sema_expr_resolve_int(
	const ofc_sema_expr_t* expr,
	int* value);


ofc_sema_expr_list_t* ofc_sema_expr_list(
	ofc_sema_scope_t*            scope,
	const ofc_parse_expr_list_t* list);
ofc_sema_expr_list_t* ofc_sema_expr_list_label(
	ofc_sema_scope_t*            scope,
	const ofc_parse_expr_list_t* list);
ofc_sema_expr_list_t* ofc_sema_expr_list_clist(
	ofc_sema_scope_t*            scope,
	const ofc_parse_expr_list_t* clist);
ofc_sema_expr_list_t* ofc_sema_expr_list_io(
	ofc_sema_scope_t*            scope,
	const ofc_parse_expr_list_t* iolist);
ofc_sema_expr_list_t* ofc_sema_expr_list_create(void);
void ofc_sema_expr_list_delete(
	ofc_sema_expr_list_t* list);
ofc_sema_expr_list_t* ofc_sema_expr_list_copy_replace(
	const ofc_sema_expr_list_t* list,
	const ofc_sema_decl_t*      replace,
	const ofc_sema_expr_t*      with);
ofc_sema_expr_list_t* ofc_sema_expr_list_copy(
	const ofc_sema_expr_list_t* list);
bool ofc_sema_expr_list_add(
	ofc_sema_expr_list_t* list,
	ofc_sema_expr_t* expr);
bool ofc_sema_expr_list_add_list(
	ofc_sema_expr_list_t* alist,
	ofc_sema_expr_list_t* blist);
unsigned ofc_sema_expr_list_count(
	const ofc_sema_expr_list_t* list);
bool ofc_sema_expr_list_elem_count(
	const ofc_sema_expr_list_t* list, unsigned* count);
ofc_sema_expr_t* ofc_sema_expr_list_elem_get(
	const ofc_sema_expr_list_t* list, unsigned offset);
bool ofc_sema_expr_list_compare(
	const ofc_sema_expr_list_t* a,
	const ofc_sema_expr_list_t* b);

bool ofc_sema_expr_foreach(
	ofc_sema_expr_t* expr, void* param,
	bool (*func)(ofc_sema_expr_t* expr, void* param));
bool ofc_sema_expr_list_foreach(
	ofc_sema_expr_list_t* list, void* param,
	bool (*func)(ofc_sema_expr_t* expr, void* param));

bool ofc_sema_expr_print(
	ofc_colstr_t* cs,
	const ofc_sema_expr_t* expr);
bool ofc_sema_expr_list_print(
	ofc_colstr_t* cs,
	const ofc_sema_expr_list_t* expr_list);

#endif
