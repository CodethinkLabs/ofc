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

#ifndef __ofc_sema_stmt_h__
#define __ofc_sema_stmt_h__

/* Some standard libraries define stdin and stdout as macros. */
#ifdef stdin
#undef stdin
#endif

#ifdef stdout
#undef stdout
#endif


typedef enum
{
	OFC_SEMA_STMT_ASSIGNMENT = 0,
	OFC_SEMA_STMT_ASSIGN,
	OFC_SEMA_STMT_IO_FORMAT,
	OFC_SEMA_STMT_IO_WRITE,
	OFC_SEMA_STMT_IO_READ,
	OFC_SEMA_STMT_IO_PRINT,
	OFC_SEMA_STMT_IO_REWIND,
	OFC_SEMA_STMT_IO_END_FILE,
	OFC_SEMA_STMT_IO_BACKSPACE,
	OFC_SEMA_STMT_IO_OPEN,
	OFC_SEMA_STMT_IO_CLOSE,
	OFC_SEMA_STMT_IO_INQUIRE,
	OFC_SEMA_STMT_CONTINUE,
	OFC_SEMA_STMT_CONTAINS,
	OFC_SEMA_STMT_IF_COMPUTED,
	OFC_SEMA_STMT_IF_STATEMENT,
	OFC_SEMA_STMT_IF_THEN,
	OFC_SEMA_STMT_SELECT_CASE,
	OFC_SEMA_STMT_STOP,
	OFC_SEMA_STMT_PAUSE,
	OFC_SEMA_STMT_GO_TO,
	OFC_SEMA_STMT_GO_TO_COMPUTED,
	OFC_SEMA_STMT_DO_LABEL,
	OFC_SEMA_STMT_DO_BLOCK,
	OFC_SEMA_STMT_DO_WHILE,
	OFC_SEMA_STMT_DO_WHILE_BLOCK,
	OFC_SEMA_STMT_CALL,
	OFC_SEMA_STMT_RETURN,
	OFC_SEMA_STMT_ENTRY,
	OFC_SEMA_STMT_CYCLE,
	OFC_SEMA_STMT_EXIT,

	/* Enum order must mach ofc_sema_stmt__name array
       in src/sema/stmt.c for error printing. */

	OFC_SEMA_STMT_COUNT
} ofc_sema_stmt_e;

struct ofc_sema_stmt_s
{
	ofc_sema_stmt_e type;

	ofc_sparse_ref_t src;

	union
	{
		ofc_sema_expr_t* alt_return;

		struct
		{
			ofc_sema_lhs_t*  dest;
			ofc_sema_expr_t* expr;
		} assignment;

		struct
		{
			const ofc_sema_decl_t* dest;
			ofc_sema_expr_t*       label;
		} assign;

		struct
		{
			ofc_sema_expr_t*      cond;
			ofc_sema_expr_list_t* label;
		} if_comp;

		struct
		{
			ofc_sema_expr_t* cond;
			ofc_sema_stmt_t* stmt;
		} if_stmt;

		struct
		{
			ofc_sema_expr_t*      cond;
			ofc_sema_stmt_list_t* block_then;
			ofc_sema_stmt_list_t* block_else;
		} if_then;

		struct
		{
			ofc_sema_expr_t* case_expr;

			unsigned                count;
			ofc_sema_range_list_t** case_value;
			ofc_sema_stmt_list_t**  case_block;
		} select_case;

		struct
		{
			const ofc_parse_format_desc_list_t* src;
			ofc_parse_format_desc_list_t*       format;
			bool is_default_possible;
		} io_format;

		struct
		{
			ofc_sema_expr_t* unit;
			bool             stdout;

			/* TODO - Namelist. */
			ofc_sema_expr_t*   format;
			bool               format_ldio;
			bool               formatted;

			ofc_sema_expr_t* iostat;
			ofc_sema_expr_t* rec;
			ofc_sema_expr_t* err;
			ofc_sema_expr_t* advance;

			ofc_sema_expr_list_t* iolist;
		} io_write;

		struct
		{
			ofc_sema_expr_t* unit;
			bool             stdin;

			/* TODO - Namelist. */
			ofc_sema_expr_t*   format;
			bool               format_ldio;
			bool               formatted;

			ofc_sema_expr_t* iostat;
			ofc_sema_expr_t* rec;
			ofc_sema_expr_t* err;

			ofc_sema_expr_t* advance;

			ofc_sema_expr_t* end;
			ofc_sema_expr_t* eor;
			ofc_sema_expr_t* size;

			ofc_sema_lhs_list_t* iolist;
		} io_read;

		struct
		{
			ofc_sema_expr_t*      format;
			bool                  format_asterisk;
			ofc_sema_expr_list_t* iolist;
		} io_print;

		struct
		{
			ofc_sema_expr_t* unit;
			ofc_sema_expr_t* iostat;
			ofc_sema_expr_t* err;
		} io_position;

		struct
		{
			ofc_sema_expr_t* unit;
			ofc_sema_expr_t* iostat;
			ofc_sema_expr_t* err;
			ofc_sema_expr_t* recl;

			ofc_sema_expr_t* access;
			ofc_sema_expr_t* action;
			ofc_sema_expr_t* blank;
			ofc_sema_expr_t* delim;
			ofc_sema_expr_t* file;
			ofc_sema_expr_t* form;
			ofc_sema_expr_t* pad;
			ofc_sema_expr_t* position;
			ofc_sema_expr_t* status;
		} io_open;

		struct
		{
			ofc_sema_expr_t* unit;
			ofc_sema_expr_t* iostat;
			ofc_sema_expr_t* err;
			ofc_sema_expr_t* status;
			ofc_sema_call_arg_e status_type;
		} io_close;

		struct
		{
			ofc_sema_expr_t* unit;
			ofc_sema_expr_t* file;
			ofc_sema_expr_t* err;

			ofc_sema_lhs_t* access;
			ofc_sema_lhs_t* action;
			ofc_sema_lhs_t* blank;
			ofc_sema_lhs_t* delim;
			ofc_sema_lhs_t* direct;
			ofc_sema_lhs_t* exist;
			ofc_sema_lhs_t* form;
			ofc_sema_lhs_t* formatted;
			ofc_sema_lhs_t* iostat;
			ofc_sema_lhs_t* name;
			ofc_sema_lhs_t* named;
			ofc_sema_lhs_t* nextrec;
			ofc_sema_lhs_t* number;
			ofc_sema_lhs_t* opened;
			ofc_sema_lhs_t* pad;
			ofc_sema_lhs_t* position;
			ofc_sema_lhs_t* read;
			ofc_sema_lhs_t* readwrite;
			ofc_sema_lhs_t* recl;
			ofc_sema_lhs_t* sequential;
			ofc_sema_lhs_t* unformatted;
			ofc_sema_lhs_t* write;
		} io_inquire;

		struct
		{
			ofc_sema_expr_t* str;
		} stop_pause;

		struct
		{
			ofc_sema_expr_t*      label;
			ofc_sema_expr_list_t* allow;
		} go_to;

		struct
		{
			ofc_sema_expr_t*      cond;
			ofc_sema_expr_list_t* label;
		} go_to_comp;

		struct
		{
			ofc_sema_expr_t* end_label;
			ofc_sema_lhs_t*  iter;
			ofc_sema_expr_t* init;
			ofc_sema_expr_t* last;
			ofc_sema_expr_t* step;
		} do_label;

		struct
		{
			ofc_sema_lhs_t*       iter;
			ofc_sema_expr_t*      init;
			ofc_sema_expr_t*      last;
			ofc_sema_expr_t*      step;
			ofc_sema_stmt_list_t* block;
		} do_block;

		struct
		{
			ofc_sema_expr_t* end_label;
			ofc_sema_expr_t* cond;
		} do_while;

		struct
		{
			ofc_sema_expr_t*      cond;
			ofc_sema_stmt_list_t* block;
		} do_while_block;

		struct
		{
			const ofc_sema_decl_t*     subroutine;
			ofc_sema_dummy_arg_list_t* args;
		} call;

		struct
		{
			ofc_str_ref_t        name;
			ofc_sema_arg_list_t* args;
		} entry;
	};
};


ofc_sema_stmt_t* ofc_sema_stmt(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);

ofc_sema_stmt_t* ofc_sema_stmt_alloc(
	ofc_sema_stmt_t stmt);
void ofc_sema_stmt_delete(
	ofc_sema_stmt_t* stmt);

/* Declaration statement analysis. */
bool ofc_sema_stmt_data(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
bool ofc_sema_stmt_dimension(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
bool ofc_sema_stmt_equivalence_spec(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
bool ofc_sema_stmt_equivalence(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
bool ofc_sema_stmt_common(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
bool ofc_sema_stmt_decl_attr(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
bool ofc_sema_stmt_intrinsic(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
bool ofc_sema_stmt_save(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
bool ofc_sema_stmt_use(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
bool ofc_sema_stmt_public(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
bool ofc_sema_stmt_private(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);

/* Execution statement analysis. */
ofc_sema_stmt_t* ofc_sema_stmt_assignment(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
ofc_sema_stmt_t* ofc_sema_stmt_assign(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
ofc_sema_stmt_t* ofc_sema_stmt_io_format(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
ofc_sema_stmt_t* ofc_sema_stmt_io_write(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
ofc_sema_stmt_t* ofc_sema_stmt_io_read(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
ofc_sema_stmt_t* ofc_sema_stmt_io_print(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
ofc_sema_stmt_t* ofc_sema_stmt_io_position(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
ofc_sema_stmt_t* ofc_sema_stmt_io_open(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
ofc_sema_stmt_t* ofc_sema_stmt_io_close(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
ofc_sema_stmt_t* ofc_sema_stmt_io_inquire(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
ofc_sema_stmt_t* ofc_sema_stmt_if(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
ofc_sema_stmt_t* ofc_sema_stmt_select_case(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
ofc_sema_stmt_t* ofc_sema_stmt_stop_pause(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
ofc_sema_stmt_t* ofc_sema_stmt_go_to(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
ofc_sema_stmt_t* ofc_sema_stmt_do(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
ofc_sema_stmt_t* ofc_sema_stmt_cycle_exit(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
ofc_sema_stmt_t* ofc_sema_stmt_call(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
ofc_sema_stmt_t* ofc_sema_stmt_return(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
ofc_sema_stmt_t* ofc_sema_stmt_entry(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);

bool ofc_sema_stmt_is_stmt_func(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);

struct ofc_sema_stmt_list_s
{
	unsigned          count;
	ofc_sema_stmt_t** stmt;
};


ofc_sema_stmt_list_t* ofc_sema_stmt_list_create(void);
ofc_sema_stmt_list_t* ofc_sema_stmt_list(
	ofc_sema_scope_t* scope,
	ofc_sema_stmt_t*  block,
	const ofc_parse_stmt_list_t* body);

void ofc_sema_stmt_list_delete(
	ofc_sema_stmt_list_t* list);

bool ofc_sema_stmt_list_add(
	ofc_sema_stmt_list_t* list,
	ofc_sema_stmt_t* stmt);

bool ofc_sema_stmt_list_remove(
	ofc_sema_stmt_list_t* list,
	ofc_sema_stmt_t* stmt);

unsigned ofc_sema_stmt_list_count(
	const ofc_sema_stmt_list_t* list);

bool ofc_sema_stmt_list_foreach(
	ofc_sema_stmt_list_t* list, void* param,
	bool (*func)(ofc_sema_stmt_t* stmt, void* param));

bool ofc_sema_stmt_foreach_expr(
	ofc_sema_stmt_t* stmt, void* param,
	bool (*func)(ofc_sema_expr_t* expr, void* param));
bool ofc_sema_stmt_list_foreach_expr(
	ofc_sema_stmt_list_t* list, void* param,
	bool (*func)(ofc_sema_expr_t* expr, void* param));

bool ofc_sema_stmt_print(
	ofc_colstr_t* cs, unsigned indent,
	ofc_sema_label_map_t* label_map,
	const ofc_sema_stmt_t* stmt);

bool ofc_sema_stmt_list_print(
	ofc_colstr_t* cs, unsigned indent,
	ofc_sema_label_map_t* label_map,
	const ofc_sema_stmt_list_t* stmt_list);

#endif
