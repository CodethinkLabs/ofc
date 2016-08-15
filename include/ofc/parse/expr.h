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

#ifndef __ofc_parse_expr_h__
#define __ofc_parse_expr_h__

typedef enum
{
	OFC_PARSE_EXPR_CONSTANT,
	OFC_PARSE_EXPR_VARIABLE,
	OFC_PARSE_EXPR_BRACKETS,
	OFC_PARSE_EXPR_UNARY,
	OFC_PARSE_EXPR_BINARY,
	OFC_PARSE_EXPR_IMPLICIT_DO,
	OFC_PARSE_EXPR_ARRAY,
	OFC_PARSE_EXPR_RESHAPE,
} ofc_parse_expr_e;


typedef struct ofc_parse_expr_list_s ofc_parse_expr_list_t;

struct ofc_parse_expr_s
{
	ofc_parse_expr_e type;

	ofc_sparse_ref_t src;

	union
	{
		ofc_parse_literal_t literal;
		ofc_parse_lhs_t*    variable;

		struct
		{
			ofc_parse_expr_t* expr;
		} brackets;

		struct
		{
			ofc_parse_expr_t*    a;
			ofc_parse_operator_e operator;
		} unary;

		struct
		{
			ofc_parse_expr_t*    a;
			ofc_parse_expr_t*    b;
			ofc_parse_operator_e operator;
		} binary;

		ofc_parse_expr_implicit_do_t* implicit_do;

		ofc_parse_expr_list_t* array;

		struct
		{
			ofc_parse_expr_t* source;
			ofc_parse_expr_t* shape;
			ofc_parse_expr_t* pad;
			ofc_parse_expr_t* order;
		} reshape;
	};
};

struct ofc_parse_expr_list_s
{
	ofc_sparse_ref_t   src;
	unsigned           count;
	ofc_parse_expr_t** expr;
};


unsigned ofc_parse_expr_precedence(ofc_parse_expr_t* expr);

/* Returns a literal unsigned integer wrapped in an expression type. */
ofc_parse_expr_t* ofc_parse_expr_integer(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);

/* Returns a literal unsigned integer or non-array variable as an expr type. */
ofc_parse_expr_t* ofc_parse_expr_integer_variable(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug, unsigned* len);

ofc_parse_expr_t* ofc_parse_expr(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
ofc_parse_expr_t* ofc_parse_expr_id(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);

void ofc_parse_expr_delete(
	ofc_parse_expr_t* expr);
ofc_parse_expr_t* ofc_parse_expr_copy(
	const ofc_parse_expr_t* expr);

bool ofc_parse_expr_print(
	ofc_colstr_t* cs, const ofc_parse_expr_t* expr);

ofc_parse_expr_list_t* ofc_parse_expr_list(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
ofc_parse_expr_list_t* ofc_parse_expr_clist(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
ofc_parse_expr_list_t* ofc_parse_expr_list_copy(
	const ofc_parse_expr_list_t* list);
void ofc_parse_expr_list_delete(
	ofc_parse_expr_list_t* list);

bool ofc_parse_expr_list_print(
	ofc_colstr_t* cs, const ofc_parse_expr_list_t* expr);

#endif
