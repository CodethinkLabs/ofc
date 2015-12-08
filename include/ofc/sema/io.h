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

#ifndef __ofc_sema_io_h__
#define __ofc_sema_io_h__

typedef enum
{
	OFC_SEMA_CALL_ARG_DIRECT = 0,
	OFC_SEMA_CALL_ARG_SEQUENTIAL,
	OFC_SEMA_CALL_ARG_READ,
	OFC_SEMA_CALL_ARG_WRITE,
	OFC_SEMA_CALL_ARG_READWRITE,
	OFC_SEMA_CALL_ARG_BLANK_NULL,
	OFC_SEMA_CALL_ARG_BLANK_ZERO,
	OFC_SEMA_CALL_ARG_DELIM_APOSTROPHE,
	OFC_SEMA_CALL_ARG_DELIM_QUOTE,
	OFC_SEMA_CALL_ARG_DELIM_NONE,
	OFC_SEMA_CALL_ARG_FORMATTED,
	OFC_SEMA_CALL_ARG_UNFORMATTED,
	OFC_SEMA_CALL_ARG_PAD_YES,
	OFC_SEMA_CALL_ARG_PAD_NO,
	OFC_SEMA_CALL_ARG_ASIS,
	OFC_SEMA_CALL_ARG_REWIND,
	OFC_SEMA_CALL_ARG_APPEND,
	OFC_SEMA_CALL_ARG_OLD,
	OFC_SEMA_CALL_ARG_NEW,
	OFC_SEMA_CALL_ARG_UNKNOWN,
	OFC_SEMA_CALL_ARG_REPLACE,
	OFC_SEMA_CALL_ARG_SCRATCH,
	OFC_SEMA_CALL_ARG_ADVANCE_YES,
	OFC_SEMA_CALL_ARG_ADVANCE_NO,
	OFC_SEMA_CALL_ARG_KEEP,
	OFC_SEMA_CALL_ARG_DELETE,

	OFC_SEMA_CALL_ARG_COUNT
} ofc_sema_call_arg_e;

bool ofc_sema_io_compare_types(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt,
	ofc_sema_expr_t** expr,
	const ofc_sema_type_t* type,
	ofc_parse_format_desc_list_t* format_list,
	unsigned* offset);

ofc_sema_expr_list_t* ofc_sema_iolist(
	ofc_sema_scope_t* scope,
	ofc_parse_expr_list_t* parse_iolist);
unsigned ofc_sema_iolist_count(
	ofc_sema_expr_list_t* iolist);

ofc_parse_format_desc_list_t* ofc_sema_io_data_format(
	ofc_sema_format_t* format, unsigned iolist_len);
unsigned ofc_sema_io_data_format_count(
	ofc_sema_format_t* format);

bool ofc_sema_io_format_iolist_compare(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt,
	ofc_parse_format_desc_list_t* format_list,
	ofc_sema_expr_list_t* iolist);

bool ofc_sema_io_check_label(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt,
	const char* name, unsigned name_size,
	bool is_format, ofc_sema_expr_t* expr,
	const ofc_sema_label_t** label_dst);
#endif
