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

#ifndef __ofc_parse_implicit_do_h__
#define __ofc_parse_implicit_do_h__

/* http://docs.oracle.com/cd/E19957-01/805-4939/6j4m0vn85/index.html#c40000f814617 */

struct ofc_parse_expr_implicit_do_s
{
	ofc_parse_expr_list_t* dlist;
	ofc_parse_expr_t*      iter;
	ofc_parse_expr_t*      init;
	ofc_parse_expr_t*      limit;
	ofc_parse_expr_t*      step;
};

struct ofc_parse_lhs_implicit_do_s
{
	ofc_parse_lhs_list_t* dlist;
	ofc_parse_expr_t*     iter;
	ofc_parse_expr_t*     init;
	ofc_parse_expr_t*     limit;
	ofc_parse_expr_t*     step;
};

ofc_parse_expr_implicit_do_t* ofc_parse_expr_implicit_do(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
ofc_parse_lhs_implicit_do_t* ofc_parse_lhs_implicit_do(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);

ofc_parse_expr_implicit_do_t* ofc_parse_expr_implicit_do_copy(
	ofc_parse_expr_implicit_do_t* id);
ofc_parse_lhs_implicit_do_t* ofc_parse_lhs_implicit_do_copy(
	ofc_parse_lhs_implicit_do_t* id);

void ofc_parse_expr_implicit_do_delete(
	ofc_parse_expr_implicit_do_t* id);
void ofc_parse_lhs_implicit_do_delete(
	ofc_parse_lhs_implicit_do_t* id);

bool ofc_parse_expr_implicit_do_print(
	ofc_colstr_t* cs, const ofc_parse_expr_implicit_do_t* id);
bool ofc_parse_lhs_implicit_do_print(
	ofc_colstr_t* cs, const ofc_parse_lhs_implicit_do_t* id);

#endif
