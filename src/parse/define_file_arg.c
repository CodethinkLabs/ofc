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

#include "ofc/parse.h"


ofc_parse_define_file_arg_t* ofc_parse_define_file_arg(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug, unsigned* len)
{
	ofc_parse_define_file_arg_t* define_file_arg
		= (ofc_parse_define_file_arg_t*)malloc(
			sizeof(ofc_parse_define_file_arg_t));
	if (!define_file_arg) return NULL;

	define_file_arg->unit = NULL;
	define_file_arg->rec  = NULL;
	define_file_arg->len  = NULL;
	define_file_arg->ascv = NULL;

	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = 0;
	unsigned l;
	define_file_arg->unit = ofc_parse_expr_integer_variable(
		src, &ptr[i], debug, &l);
	if (!define_file_arg->unit)
	{
		free(define_file_arg);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}
	i += l;

	if (ptr[i++] != '(')
	{
		ofc_parse_define_file_arg_delete(define_file_arg);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}

	define_file_arg->rec = ofc_parse_expr_integer_variable(
		src, &ptr[i], debug, &l);
	if (!define_file_arg->rec)
	{
		ofc_parse_define_file_arg_delete(define_file_arg);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}
	i += l;

	if (ptr[i++] != ',')
	{
		ofc_parse_define_file_arg_delete(define_file_arg);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}

	define_file_arg->len = ofc_parse_expr_integer_variable(
		src, &ptr[i], debug, &l);
	if (!define_file_arg->len)
	{
		ofc_parse_define_file_arg_delete(define_file_arg);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}
	i += l;

	if ((ptr[i++] != ',')
		|| (toupper(ptr[i++]) != 'U')
		|| (ptr[i++] != ','))
	{
		ofc_parse_define_file_arg_delete(define_file_arg);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}

	define_file_arg->ascv = ofc_parse_lhs(
		src, &ptr[i], debug, &l);
	if (!define_file_arg->ascv)
	{
		ofc_parse_define_file_arg_delete(define_file_arg);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}
	i += l;

	if (ptr[i++] != ')')
	{
		ofc_parse_define_file_arg_delete(define_file_arg);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}

	if (len) *len = i;
	return define_file_arg;
}

void ofc_parse_define_file_arg_delete(
	ofc_parse_define_file_arg_t* define_file_arg)
{
	if (!define_file_arg)
		return;

	ofc_parse_expr_delete(define_file_arg->unit);
	ofc_parse_expr_delete(define_file_arg->rec);
	ofc_parse_expr_delete(define_file_arg->len);
	ofc_parse_lhs_delete(define_file_arg->ascv);
	free(define_file_arg);
}

bool ofc_parse_define_file_arg_print(
	ofc_colstr_t* cs, const ofc_parse_define_file_arg_t* define_file_arg)
{
	if (!define_file_arg)
		return false;

	return (ofc_parse_expr_print(cs, define_file_arg->unit)
			&& ofc_colstr_atomic_writef(cs, "( ")
			&& ofc_parse_expr_print(cs, define_file_arg->rec)
			&& ofc_colstr_atomic_writef(cs, ", ")
			&& ofc_parse_expr_print(cs, define_file_arg->len)
			&& ofc_colstr_atomic_writef(cs, ", U, ")
			&& ofc_parse_lhs_print(cs, define_file_arg->ascv, false)
			&& ofc_colstr_atomic_writef(cs, ")"));
}


ofc_parse_define_file_arg_list_t* ofc_parse_define_file_arg_list(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug, unsigned* len)
{
	ofc_parse_define_file_arg_list_t* list
		= (ofc_parse_define_file_arg_list_t*)malloc(
			sizeof(ofc_parse_define_file_arg_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->define_file_arg = NULL;

	unsigned i = ofc_parse_list(src, ptr, debug, ',',
		&list->count, (void***)&list->define_file_arg,
		(void*)ofc_parse_define_file_arg,
		(void*)ofc_parse_define_file_arg_delete);
	if (i == 0)
	{
		free(list);
		return NULL;
	}

	if (len) *len = i;
	return list;
}

void ofc_parse_define_file_arg_list_delete(
	ofc_parse_define_file_arg_list_t* list)
{
	if (!list)
		return;

	ofc_parse_list_delete(
		list->count, (void**)list->define_file_arg,
		(void*)ofc_parse_define_file_arg_delete);
	free(list);
}

bool ofc_parse_define_file_arg_list_print(
	ofc_colstr_t* cs, const ofc_parse_define_file_arg_list_t* list)
{
	return ofc_parse_list_print(
		cs, list->count, (const void**)list->define_file_arg,
		(void*)ofc_parse_define_file_arg_print);
}
