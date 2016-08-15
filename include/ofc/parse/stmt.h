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

#ifndef __ofc_parse_stmt_h__
#define __ofc_parse_stmt_h__


typedef struct
{
	unsigned           count, size;
	ofc_parse_stmt_t** stmt;
} ofc_parse_stmt_list_t;

typedef enum
{
	OFC_PARSE_STMT_EMPTY,
	OFC_PARSE_STMT_ERROR,
	OFC_PARSE_STMT_INCLUDE,
	OFC_PARSE_STMT_USE,
	OFC_PARSE_STMT_PROGRAM,
	OFC_PARSE_STMT_SUBROUTINE,
	OFC_PARSE_STMT_FUNCTION,
	OFC_PARSE_STMT_MODULE,
	OFC_PARSE_STMT_BLOCK_DATA,
	OFC_PARSE_STMT_IMPLICIT_NONE,
	OFC_PARSE_STMT_IMPLICIT,
	OFC_PARSE_STMT_CONTAINS,
	OFC_PARSE_STMT_CALL,
	OFC_PARSE_STMT_ENTRY,
	OFC_PARSE_STMT_DECL,
	OFC_PARSE_STMT_DIMENSION,
	OFC_PARSE_STMT_EQUIVALENCE,
	OFC_PARSE_STMT_COMMON,
	OFC_PARSE_STMT_NAMELIST,
	OFC_PARSE_STMT_ASSIGNMENT,
	OFC_PARSE_STMT_CONTINUE,
	OFC_PARSE_STMT_CYCLE,
	OFC_PARSE_STMT_EXIT,
	OFC_PARSE_STMT_STOP,
	OFC_PARSE_STMT_PAUSE,
	OFC_PARSE_STMT_RETURN,
	OFC_PARSE_STMT_DECL_ATTR_EXTERNAL,
	OFC_PARSE_STMT_DECL_ATTR_INTRINSIC,
	OFC_PARSE_STMT_DECL_ATTR_AUTOMATIC,
	OFC_PARSE_STMT_DECL_ATTR_STATIC,
	OFC_PARSE_STMT_DECL_ATTR_VOLATILE,
	OFC_PARSE_STMT_POINTER,
	OFC_PARSE_STMT_GO_TO,
	OFC_PARSE_STMT_GO_TO_ASSIGNED,
	OFC_PARSE_STMT_GO_TO_COMPUTED,
	OFC_PARSE_STMT_IF_COMPUTED,
	OFC_PARSE_STMT_IF_STATEMENT,
	OFC_PARSE_STMT_IF_THEN,
	OFC_PARSE_STMT_SELECT_CASE,
	OFC_PARSE_STMT_DO_LABEL,
	OFC_PARSE_STMT_DO_BLOCK,
	OFC_PARSE_STMT_DO_WHILE,
	OFC_PARSE_STMT_DO_WHILE_BLOCK,
	OFC_PARSE_STMT_TYPE,
	OFC_PARSE_STMT_STRUCTURE,
	OFC_PARSE_STMT_UNION,
	OFC_PARSE_STMT_MAP,
	OFC_PARSE_STMT_SEQUENCE,
	OFC_PARSE_STMT_IO_OPEN,
	OFC_PARSE_STMT_IO_INQUIRE,
	OFC_PARSE_STMT_IO_REWIND,
	OFC_PARSE_STMT_IO_BACKSPACE,
	OFC_PARSE_STMT_IO_READ,
	OFC_PARSE_STMT_IO_WRITE,
	OFC_PARSE_STMT_IO_END_FILE,
	OFC_PARSE_STMT_IO_CLOSE,
	OFC_PARSE_STMT_IO_PRINT,
	OFC_PARSE_STMT_IO_TYPE,
	OFC_PARSE_STMT_IO_ENCODE,
	OFC_PARSE_STMT_IO_DECODE,
	OFC_PARSE_STMT_IO_ACCEPT,
	OFC_PARSE_STMT_IO_DEFINE_FILE,
	OFC_PARSE_STMT_FORMAT,
	OFC_PARSE_STMT_DATA,
	OFC_PARSE_STMT_SAVE,
	OFC_PARSE_STMT_PARAMETER,
	OFC_PARSE_STMT_ASSIGN,
	OFC_PARSE_STMT_PUBLIC,
	OFC_PARSE_STMT_PRIVATE,
} ofc_parse_stmt_e;

struct ofc_parse_stmt_s
{
	ofc_parse_stmt_e type;

	unsigned label;

	ofc_sparse_ref_t src;

	union
	{
		struct
		{
			ofc_file_t*   file;
			ofc_sparse_t* src;
		} include;

		struct
		{
			ofc_sparse_ref_t       module;
			ofc_parse_lhs_list_t*  rename;
			ofc_parse_decl_list_t* only;
		} use;

		ofc_parse_implicit_list_t* implicit;

		struct
		{
			ofc_sparse_ref_t           name;
			ofc_parse_call_arg_list_t* args;
		} call_entry;

		struct
		{
			bool                     save;
			bool                     parameter;
			bool                     is_public;
			bool                     is_private;
			ofc_parse_array_index_t* dimension;
			ofc_parse_type_t*        type;
			ofc_parse_decl_list_t*   decl;
		} decl;

		ofc_parse_common_group_list_t* common_namelist;

		ofc_parse_lhs_list_t* dimension;

		struct
		{
			unsigned               count;
			ofc_parse_lhs_list_t** group;
		} equivalence;

		ofc_parse_assign_t* assignment;

		struct
		{
			ofc_parse_expr_t* value;
		} stop_pause_return;

		struct
		{
			unsigned           count;
			ofc_sparse_ref_t** name;
		} decl_attr;

		ofc_parse_pointer_list_t* pointer;

		struct
		{
			ofc_parse_expr_t*      cond;
			ofc_parse_expr_list_t* label;
		} go_to_list;

		struct
		{
			ofc_parse_expr_t* label;
		} go_to;

		struct
		{
			ofc_parse_expr_t*      cond;
			ofc_parse_expr_list_t* label;
		} if_comp;

		struct
		{
			ofc_parse_expr_t*  cond;
			ofc_parse_stmt_t*  stmt;
		} if_stmt;

		struct
		{
			ofc_parse_expr_t*      cond;
			ofc_parse_stmt_list_t* block_then;
			ofc_parse_stmt_list_t* block_else;

			bool end_if_has_label;
			unsigned end_if_label;
		} if_then;

		struct
		{
			ofc_parse_expr_t* case_expr;

			unsigned                  count;
			ofc_parse_array_index_t** case_value;
			ofc_parse_stmt_list_t**   case_block;

			bool end_select_case_has_label;
			unsigned end_select_case_label;
		} select_case;

		struct
		{
			ofc_parse_expr_t*   end_label;
			ofc_parse_assign_t* init;
			ofc_parse_expr_t*   last;
			ofc_parse_expr_t*   step;
		} do_label;

		struct
		{
			ofc_parse_assign_t*    init;
			ofc_parse_expr_t*      last;
			ofc_parse_expr_t*      step;
			ofc_parse_stmt_list_t* block;

			bool end_do_has_label;
			unsigned end_do_label;
		} do_block;

		struct
		{
			ofc_parse_expr_t* end_label;
			ofc_parse_expr_t* cond;
		} do_while;

		struct
		{
			ofc_parse_expr_t*      cond;
			ofc_parse_stmt_list_t* block;

			bool end_do_has_label;
			unsigned end_do_label;
		} do_while_block;

		struct
		{
			ofc_sparse_ref_t       name;
			ofc_parse_stmt_list_t* block;
		} structure;

		struct
		{
			ofc_parse_call_arg_list_t* params;
			bool                       has_brakets;
			ofc_parse_expr_list_t*     iolist;
		} io;

		struct
		{
			ofc_parse_expr_t*      format;
			bool                   format_asterisk;
			ofc_parse_expr_list_t* iolist;
		} io_print;

		struct
		{
			ofc_parse_call_arg_list_t* params;
			bool                       has_brakets;
			ofc_parse_lhs_list_t*      iolist;
		} io_read;

		struct
		{
			ofc_parse_define_file_arg_list_t* args;
		} io_define_file;

		ofc_parse_format_desc_list_t* format;

		ofc_parse_data_list_t* data;

		struct
		{
			ofc_parse_save_list_t* list;
		} save;

		struct
		{
			ofc_parse_assign_list_t* list;
		} parameter;

		struct
		{
			ofc_parse_lhs_list_t* list;
		} public_private;

		struct
		{
			ofc_parse_expr_t* label;
			ofc_sparse_ref_t  variable;
		} assign;

		struct
		{
			/* type is only set for functions. */
			ofc_parse_type_t*          type;
			ofc_sparse_ref_t           name;
			/* args is only set for functions and subroutines. */
			ofc_parse_call_arg_list_t* args;
			ofc_parse_stmt_list_t*     body;

			bool     end_has_label;
			unsigned end_label;
		} program;

		struct
		{
			bool                has_keyword;
			ofc_parse_keyword_e keyword;
		} cycle_exit;
	};
};


ofc_parse_stmt_t* ofc_parse_stmt(
	ofc_parse_stmt_list_t* list,
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
void ofc_parse_stmt_delete(
	ofc_parse_stmt_t* stmt);

bool ofc_parse_stmt_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_parse_stmt_t* stmt);

bool ofc_parse_stmt_sublist(
	ofc_parse_stmt_list_t* list,
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
ofc_parse_stmt_list_t* ofc_parse_stmt_list(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);

ofc_parse_stmt_list_t* ofc_parse_stmt_list_create(void);
void ofc_parse_stmt_list_delete(
	ofc_parse_stmt_list_t* list);

bool ofc_parse_stmt_list_add(
	ofc_parse_stmt_list_t* list,
	ofc_parse_stmt_t* stmt);

bool ofc_parse_stmt_list_foreach(
	const ofc_parse_stmt_list_t* list, void* context,
	bool (*callback)(const ofc_parse_stmt_t* stmt, void* context));

bool ofc_parse_stmt_list_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_parse_stmt_list_t* list);

bool ofc_parse_stmt_list_contains_error(
	const ofc_parse_stmt_list_t* list);

#endif
