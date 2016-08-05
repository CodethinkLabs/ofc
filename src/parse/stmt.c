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

unsigned ofc_parse_stmt_include(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt,
	ofc_parse_stmt_list_t* list);
unsigned ofc_parse_stmt_use(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_program(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_subroutine(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_function(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_module(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_block_data(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_implicit(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_call(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_entry(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_decl(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);

unsigned ofc_parse_stmt_common(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_namelist(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_dimension(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_virtual(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_equivalence(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);

unsigned ofc_parse_stmt_continue(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_contains(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_cycle_exit(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_stop(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_pause(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_return(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_go_to(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_if(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_select_case(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_do(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_while_do_block(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_format(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_data(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_save(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_parameter(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_assign(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_pointer(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_public(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_private(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);

unsigned ofc_parse_stmt_decl_attr_external(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_decl_attr_intrinsic(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_decl_attr_automatic(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_decl_attr_static(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_decl_attr_volatile(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);

unsigned ofc_parse_stmt_type(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_structure(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_union(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_map(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_sequence(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);

unsigned ofc_parse_stmt_io_open(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_io_inquire(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_io_rewind(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_io_backspace(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_io_read(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_io_write(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_io_end_file(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_io_close(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_io_print_type(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_io_encode(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_io_decode(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_io_accept(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);
unsigned ofc_parse_stmt_io_define_file(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt);



static void ofc_parse_stmt__cleanup(
	ofc_parse_stmt_t stmt)
{
	switch (stmt.type)
	{
		case OFC_PARSE_STMT_INCLUDE:
			ofc_sparse_delete(stmt.include.src);
			ofc_file_delete(stmt.include.file);
			break;
		case OFC_PARSE_STMT_USE:
			ofc_parse_lhs_list_delete(stmt.use.rename);
			ofc_parse_decl_list_delete(stmt.use.only);
			break;
		case OFC_PARSE_STMT_PROGRAM:
		case OFC_PARSE_STMT_SUBROUTINE:
		case OFC_PARSE_STMT_FUNCTION:
		case OFC_PARSE_STMT_MODULE:
		case OFC_PARSE_STMT_BLOCK_DATA:
			ofc_parse_stmt_list_delete(stmt.program.body);
			ofc_parse_call_arg_list_delete(stmt.program.args);
			ofc_parse_type_delete(stmt.program.type);
			break;
		case OFC_PARSE_STMT_ASSIGNMENT:
			ofc_parse_assign_delete(stmt.assignment);
			break;
		case OFC_PARSE_STMT_IMPLICIT:
			ofc_parse_implicit_list_delete(stmt.implicit);
			break;
		case OFC_PARSE_STMT_CALL:
		case OFC_PARSE_STMT_ENTRY:
			ofc_parse_call_arg_list_delete(stmt.call_entry.args);
			break;
		case OFC_PARSE_STMT_DECL:
			ofc_parse_array_index_delete(stmt.decl.dimension);
			ofc_parse_type_delete(stmt.decl.type);
			ofc_parse_decl_list_delete(stmt.decl.decl);
			break;
		case OFC_PARSE_STMT_COMMON:
		case OFC_PARSE_STMT_NAMELIST:
			ofc_parse_common_group_list_delete(stmt.common_namelist);
			break;
		case OFC_PARSE_STMT_DIMENSION:
			ofc_parse_lhs_list_delete(stmt.dimension);
			break;
		case OFC_PARSE_STMT_EQUIVALENCE:
			ofc_parse_list_delete(
				stmt.equivalence.count,
				(void**)stmt.equivalence.group,
				(void*)ofc_parse_lhs_list_delete);
			break;
		case OFC_PARSE_STMT_STOP:
		case OFC_PARSE_STMT_PAUSE:
		case OFC_PARSE_STMT_RETURN:
			ofc_parse_expr_delete(stmt.stop_pause_return.value);
			break;
		case OFC_PARSE_STMT_DECL_ATTR_EXTERNAL:
		case OFC_PARSE_STMT_DECL_ATTR_INTRINSIC:
		case OFC_PARSE_STMT_DECL_ATTR_AUTOMATIC:
		case OFC_PARSE_STMT_DECL_ATTR_STATIC:
		case OFC_PARSE_STMT_DECL_ATTR_VOLATILE:
			ofc_parse_list_delete(
				stmt.decl_attr.count,
				(void**)stmt.decl_attr.name,
				free);
			break;
		case OFC_PARSE_STMT_POINTER:
			ofc_parse_pointer_list_delete(
				stmt.pointer);
			break;
		case OFC_PARSE_STMT_GO_TO:
			ofc_parse_expr_delete(stmt.go_to.label);
			break;
		case OFC_PARSE_STMT_GO_TO_ASSIGNED:
		case OFC_PARSE_STMT_GO_TO_COMPUTED:
			ofc_parse_expr_delete(stmt.go_to_list.cond);
			ofc_parse_expr_list_delete(stmt.go_to_list.label);
			break;
		case OFC_PARSE_STMT_IF_COMPUTED:
			ofc_parse_expr_delete(stmt.if_comp.cond);
			ofc_parse_expr_list_delete(stmt.if_comp.label);
			break;
		case OFC_PARSE_STMT_IF_STATEMENT:
			ofc_parse_expr_delete(stmt.if_stmt.cond);
			ofc_parse_stmt_delete(stmt.if_stmt.stmt);
			break;
		case OFC_PARSE_STMT_IF_THEN:
			ofc_parse_expr_delete(stmt.if_then.cond);
			ofc_parse_stmt_list_delete(stmt.if_then.block_then);
			ofc_parse_stmt_list_delete(stmt.if_then.block_else);
			break;
		case OFC_PARSE_STMT_SELECT_CASE:
			ofc_parse_expr_delete(
				stmt.select_case.case_expr);
			ofc_parse_list_delete(
				stmt.select_case.count,
				(void**)stmt.select_case.case_value,
				(void*)ofc_parse_array_index_delete);
			ofc_parse_list_delete(
				stmt.select_case.count,
				(void**)stmt.select_case.case_block,
				(void*)ofc_parse_stmt_list_delete);
			break;
		case OFC_PARSE_STMT_DO_LABEL:
			ofc_parse_expr_delete(stmt.do_label.end_label);
			ofc_parse_assign_delete(stmt.do_label.init);
			ofc_parse_expr_delete(stmt.do_label.last);
			ofc_parse_expr_delete(stmt.do_label.step);
			break;
		case OFC_PARSE_STMT_DO_BLOCK:
			ofc_parse_assign_delete(stmt.do_block.init);
			ofc_parse_expr_delete(stmt.do_block.last);
			ofc_parse_expr_delete(stmt.do_block.step);
			ofc_parse_stmt_list_delete(stmt.do_block.block);
			break;
		case OFC_PARSE_STMT_DO_WHILE:
			ofc_parse_expr_delete(stmt.do_label.end_label);
			ofc_parse_expr_delete(stmt.do_while.cond);
			break;
		case OFC_PARSE_STMT_DO_WHILE_BLOCK:
			ofc_parse_expr_delete(stmt.do_while_block.cond);
			ofc_parse_stmt_list_delete(stmt.do_while_block.block);
			break;
		case OFC_PARSE_STMT_TYPE:
		case OFC_PARSE_STMT_STRUCTURE:
		case OFC_PARSE_STMT_UNION:
		case OFC_PARSE_STMT_MAP:
			ofc_parse_stmt_list_delete(stmt.structure.block);
			break;
		case OFC_PARSE_STMT_IO_READ:
			ofc_parse_call_arg_list_delete(stmt.io_read.params);
			ofc_parse_lhs_list_delete(stmt.io_read.iolist);
			break;
		case OFC_PARSE_STMT_IO_REWIND:
		case OFC_PARSE_STMT_IO_BACKSPACE:
		case OFC_PARSE_STMT_IO_WRITE:
		case OFC_PARSE_STMT_IO_END_FILE:
		case OFC_PARSE_STMT_IO_CLOSE:
		case OFC_PARSE_STMT_IO_OPEN:
		case OFC_PARSE_STMT_IO_INQUIRE:
		case OFC_PARSE_STMT_IO_ENCODE:
		case OFC_PARSE_STMT_IO_DECODE:
			ofc_parse_call_arg_list_delete(stmt.io.params);
			ofc_parse_expr_list_delete(stmt.io.iolist);
			break;
		case OFC_PARSE_STMT_IO_PRINT:
		case OFC_PARSE_STMT_IO_TYPE:
		case OFC_PARSE_STMT_IO_ACCEPT:
			ofc_parse_expr_delete(stmt.io_print.format);
			ofc_parse_expr_list_delete(stmt.io_print.iolist);
			break;
		case OFC_PARSE_STMT_IO_DEFINE_FILE:
			ofc_parse_define_file_arg_list_delete(stmt.io_define_file.args);
			break;
		case OFC_PARSE_STMT_FORMAT:
			ofc_parse_format_desc_list_delete(stmt.format);
			break;
		case OFC_PARSE_STMT_DATA:
			ofc_parse_data_list_delete(stmt.data);
			break;
		case OFC_PARSE_STMT_SAVE:
			ofc_parse_save_list_delete(stmt.save.list);
			break;
		case OFC_PARSE_STMT_PARAMETER:
			ofc_parse_assign_list_delete(stmt.parameter.list);
			break;
		case OFC_PARSE_STMT_ASSIGN:
			ofc_parse_expr_delete(stmt.assign.label);
			break;
		case OFC_PARSE_STMT_PUBLIC:
		case OFC_PARSE_STMT_PRIVATE:
			ofc_parse_lhs_list_delete(stmt.public_private.list);
			break;

		default:
			break;
	}
}

static ofc_parse_stmt_t* ofc_parse_stmt__alloc(
	ofc_parse_stmt_t stmt)
{
	ofc_parse_stmt_t* astmt
		= (ofc_parse_stmt_t*)malloc(
			sizeof(ofc_parse_stmt_t));
	if (!astmt) return NULL;

	*astmt = stmt;
	return astmt;
}



ofc_parse_stmt_t* ofc_parse_stmt(
	ofc_parse_stmt_list_t* list,
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	if (ptr[0] == '\0')
		return NULL;

	ofc_parse_stmt_t stmt;
	stmt.type  = OFC_PARSE_STMT_EMPTY;

	/* TODO - Allow handling of label 0? */
	stmt.label = 0;
	ofc_sparse_label_find(src, ptr, &stmt.label);

	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = 0;

	if (i == 0) i = ofc_parse_stmt_function(src, ptr, debug, &stmt);
	if (i == 0) i = ofc_parse_stmt_decl(src, ptr, debug, &stmt);

	/* Drop incomplete statements. */
	if ((i > 0) && (stmt.type != OFC_PARSE_STMT_ERROR)
		&& !ofc_is_end_statement(&ptr[i], NULL))
	{
		ofc_parse_stmt__cleanup(stmt);
		i = 0;
		ofc_parse_debug_rewind(debug, dpos);
	}

	switch (toupper(ptr[0]))
	{
		case 'A':
			if (i == 0) i = ofc_parse_stmt_assign(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_decl_attr_automatic(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_io_accept(src, ptr, debug, &stmt);
			break;

		case 'B':
			if (i == 0) i = ofc_parse_stmt_io_backspace(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_block_data(src, ptr, debug, &stmt);
			break;

		case 'C':
			if (i == 0) i = ofc_parse_stmt_continue(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_contains(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_cycle_exit(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_call(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_common(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_io_close(src, ptr, debug, &stmt);
			break;

		case 'D':
			if (i == 0) i = ofc_parse_stmt_do(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_data(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_dimension(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_io_decode(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_io_define_file(src, ptr, debug, &stmt);
			break;

		case 'E':
			if (i == 0) i = ofc_parse_stmt_equivalence(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_cycle_exit(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_io_end_file(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_decl_attr_external(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_entry(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_io_encode(src, ptr, debug, &stmt);
			break;

		case 'F':
			if (i == 0) i = ofc_parse_stmt_format(src, ptr, debug, &stmt);
			break;

		case 'G':
			if (i == 0) i = ofc_parse_stmt_go_to(src, ptr, debug, &stmt);
			break;

		case 'I':
			if (i == 0) i = ofc_parse_stmt_implicit(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_if(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_decl_attr_intrinsic(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_io_inquire(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_include(src, ptr, debug, &stmt, list);
			break;

		case 'M':
			if (i == 0) i = ofc_parse_stmt_map(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_module(src, ptr, debug, &stmt);
			break;

		case 'N':
			if (i == 0) i = ofc_parse_stmt_namelist(src, ptr, debug, &stmt);
			break;

		case 'O':
			if (i == 0) i = ofc_parse_stmt_io_open(src, ptr, debug, &stmt);
			break;

		case 'P':
			if (i == 0) i = ofc_parse_stmt_parameter(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_program(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_pause(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_io_print_type(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_pointer(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_public(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_private(src, ptr, debug, &stmt);
			break;

		case 'R':
			if (i == 0) i = ofc_parse_stmt_return(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_io_read(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_io_rewind(src, ptr, debug, &stmt);
			break;

		case 'S':
			if (i == 0) i = ofc_parse_stmt_subroutine(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_stop(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_save(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_decl_attr_static(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_structure(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_sequence(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_select_case(src, ptr, debug, &stmt);
			break;

		case 'T':
			if (i == 0) i = ofc_parse_stmt_type(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_io_print_type(src, ptr, debug, &stmt);
			break;

		case 'U':
			if (i == 0) i = ofc_parse_stmt_union(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_use(src, ptr, debug, &stmt);
			break;

		case 'V':
			if (i == 0) i = ofc_parse_stmt_virtual(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_decl_attr_volatile(src, ptr, debug, &stmt);
			break;

		case 'W':
			if (i == 0) i = ofc_parse_stmt_io_write(src, ptr, debug, &stmt);
			if (i == 0) i = ofc_parse_stmt_while_do_block(src, ptr, debug, &stmt);
			break;

		default:
			break;
	}

	/* Drop incomplete statements, they may be an assignment. */
	if ((i > 0) && (stmt.type != OFC_PARSE_STMT_ERROR)
		&& !ofc_is_end_statement(&ptr[i], NULL))
	{
		ofc_parse_stmt__cleanup(stmt);
		i = 0;
		ofc_parse_debug_rewind(debug, dpos);
	}

	if (i == 0)
	{
		stmt.assignment = ofc_parse_assign(src, ptr, debug, &i);
		if (stmt.assignment)
		{
			if (!stmt.assignment->init)
			{
				i = 0;
				ofc_parse_assign_delete(stmt.assignment);
			}
			else
			{
				stmt.type = OFC_PARSE_STMT_ASSIGNMENT;
			}
		}
	}

	unsigned l = 0;
	if ((stmt.type != OFC_PARSE_STMT_ERROR)
		&& !ofc_is_end_statement(&ptr[i], &l))
	{
		if (i == 0)
		{
			ofc_parse_debug_rewind(debug, dpos);
			return NULL;
		}

		ofc_parse_debug_warning(debug,
			ofc_sparse_ref(src, &ptr[i], 0),
			"Expected newline or semicolon after statement");
		ofc_parse_stmt__cleanup(stmt);
		return NULL;
	}
	i += l;

	stmt.src = ofc_sparse_ref(src, ptr, i);

	ofc_parse_stmt_t* astmt
		= ofc_parse_stmt__alloc(stmt);
	if (!astmt)
	{
		ofc_parse_stmt__cleanup(stmt);
		return NULL;
	}

	if (len) *len = i;
	return astmt;
}

void ofc_parse_stmt_delete(
	ofc_parse_stmt_t* stmt)
{
	if (!stmt)
		return;

	ofc_parse_stmt__cleanup(*stmt);
	free(stmt);
}



bool ofc_parse_stmt_program_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_parse_stmt_t* stmt);
bool ofc_parse_stmt_contains_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt);
bool ofc_parse_stmt_use_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt);
bool ofc_parse_stmt_assign_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt);
bool ofc_parse_stmt_decl_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt);
bool ofc_parse_stmt_if_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_parse_stmt_t* stmt);
bool ofc_parse_stmt_select_case_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_parse_stmt_t* stmt);
bool ofc_parse_stmt_stop_pause_return_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt);
bool ofc_parse_stmt_format_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt);
bool ofc_parse_stmt_data_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt);
bool ofc_parse_stmt_common_namelist_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt);
bool ofc_parse_stmt_implicit_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt);
bool ofc_parse_stmt_save_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt);
bool ofc_parse_stmt_parameter_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt);
bool ofc_parse_stmt_public_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt);
bool ofc_parse_stmt_private_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt);
bool ofc_parse_stmt_continue_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt);
bool ofc_parse_stmt_cycle_exit_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt);
bool ofc_parse_stmt_dimension_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt);
bool ofc_parse_stmt_call_entry_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt);
bool ofc_parse_stmt_equivalence_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt);
bool ofc_parse_stmt_do_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_parse_stmt_t* stmt);
bool ofc_parse_stmt_decl_attr_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt);
bool ofc_parse_stmt_pointer_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt);
bool ofc_parse_stmt_go_to_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt);
bool ofc_parse_stmt_structure_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_parse_stmt_t* stmt);
bool ofc_parse_stmt_sequence_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt);
bool ofc_parse_stmt_io_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt);
bool ofc_parse_stmt_print_accept_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt);
bool ofc_parse_stmt_define_file_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt);


bool ofc_parse_stmt_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_parse_stmt_t* stmt)
{
	if (stmt->type == OFC_PARSE_STMT_EMPTY)
		return true;

	switch(stmt->type)
	{
		case OFC_PARSE_STMT_INCLUDE:
			/* We no longer print INCLUDEs since the content is printed. */
			break;

		case OFC_PARSE_STMT_USE:
			if (!ofc_parse_stmt_use_print(cs, stmt))
				return false;
			break;
		case OFC_PARSE_STMT_PROGRAM:
		case OFC_PARSE_STMT_SUBROUTINE:
		case OFC_PARSE_STMT_FUNCTION:
		case OFC_PARSE_STMT_MODULE:
		case OFC_PARSE_STMT_BLOCK_DATA:
			if (!ofc_parse_stmt_program_print(
				cs, indent, stmt))
				return false;
			break;
		case OFC_PARSE_STMT_IMPLICIT_NONE:
		case OFC_PARSE_STMT_IMPLICIT:
			if (!ofc_parse_stmt_implicit_print(cs, stmt))
				return false;
			break;
		case OFC_PARSE_STMT_CONTAINS:
			if (!ofc_parse_stmt_contains_print(cs, stmt))
				return false;
			break;
		case OFC_PARSE_STMT_CALL:
		case OFC_PARSE_STMT_ENTRY:
			if (!ofc_parse_stmt_call_entry_print(cs, stmt))
				return false;
			break;
		case OFC_PARSE_STMT_DECL:
			if (!ofc_parse_stmt_decl_print(cs, stmt))
				return false;
			break;
		case OFC_PARSE_STMT_DIMENSION:
			if (!ofc_parse_stmt_dimension_print(cs, stmt))
				return false;
			break;
		case OFC_PARSE_STMT_EQUIVALENCE:
			if (!ofc_parse_stmt_equivalence_print(cs, stmt))
				return false;
			break;
		case OFC_PARSE_STMT_COMMON:
		case OFC_PARSE_STMT_NAMELIST:
			if (!ofc_parse_stmt_common_namelist_print(cs, stmt))
				return false;
			break;
		case OFC_PARSE_STMT_ASSIGNMENT:
			if (!ofc_parse_assign_print(
				cs, stmt->assignment))
				return false;
			break;
		case OFC_PARSE_STMT_CONTINUE:
			if (!ofc_parse_stmt_continue_print(cs, stmt))
				return false;
			break;
		case OFC_PARSE_STMT_CYCLE:
		case OFC_PARSE_STMT_EXIT:
			if (!ofc_parse_stmt_cycle_exit_print(cs, stmt))
				return false;
			break;
		case OFC_PARSE_STMT_STOP:
		case OFC_PARSE_STMT_PAUSE:
		case OFC_PARSE_STMT_RETURN:
			if (!ofc_parse_stmt_stop_pause_return_print(cs, stmt))
				return false;
			break;
		case OFC_PARSE_STMT_DECL_ATTR_EXTERNAL:
		case OFC_PARSE_STMT_DECL_ATTR_INTRINSIC:
		case OFC_PARSE_STMT_DECL_ATTR_AUTOMATIC:
		case OFC_PARSE_STMT_DECL_ATTR_STATIC:
		case OFC_PARSE_STMT_DECL_ATTR_VOLATILE:
			if (!ofc_parse_stmt_decl_attr_print(cs, stmt))
				return false;
			break;
		case OFC_PARSE_STMT_POINTER:
			if (!ofc_parse_stmt_pointer_print(cs, stmt))
				return false;
			break;
		case OFC_PARSE_STMT_GO_TO:
		case OFC_PARSE_STMT_GO_TO_ASSIGNED:
		case OFC_PARSE_STMT_GO_TO_COMPUTED:
			if (!ofc_parse_stmt_go_to_print(cs, stmt))
				return false;
			break;
		case OFC_PARSE_STMT_IF_COMPUTED:
		case OFC_PARSE_STMT_IF_STATEMENT:
		case OFC_PARSE_STMT_IF_THEN:
			if (!ofc_parse_stmt_if_print(cs, indent, stmt))
				return false;
			break;
		case OFC_PARSE_STMT_SELECT_CASE:
			if (!ofc_parse_stmt_select_case_print(cs, indent, stmt))
				return false;
			break;
		case OFC_PARSE_STMT_DO_LABEL:
		case OFC_PARSE_STMT_DO_BLOCK:
		case OFC_PARSE_STMT_DO_WHILE:
		case OFC_PARSE_STMT_DO_WHILE_BLOCK:
			if (!ofc_parse_stmt_do_print(cs, indent, stmt))
				return false;
			break;
		case OFC_PARSE_STMT_TYPE:
		case OFC_PARSE_STMT_STRUCTURE:
		case OFC_PARSE_STMT_UNION:
		case OFC_PARSE_STMT_MAP:
			if (!ofc_parse_stmt_structure_print(cs, indent, stmt))
				return false;
			break;
		case OFC_PARSE_STMT_SEQUENCE:
			if (!ofc_parse_stmt_sequence_print(cs, stmt))
				return false;
			break;
		case OFC_PARSE_STMT_IO_OPEN:
		case OFC_PARSE_STMT_IO_INQUIRE:
		case OFC_PARSE_STMT_IO_REWIND:
		case OFC_PARSE_STMT_IO_BACKSPACE:
		case OFC_PARSE_STMT_IO_READ:
		case OFC_PARSE_STMT_IO_WRITE:
		case OFC_PARSE_STMT_IO_END_FILE:
		case OFC_PARSE_STMT_IO_CLOSE:
			ofc_parse_stmt_io_print(cs, stmt);
			break;
		case OFC_PARSE_STMT_IO_PRINT:
		case OFC_PARSE_STMT_IO_TYPE:
		case OFC_PARSE_STMT_IO_ACCEPT:
			ofc_parse_stmt_print_accept_print(cs, stmt);
			break;
		case OFC_PARSE_STMT_IO_DEFINE_FILE:
			ofc_parse_stmt_define_file_print(cs, stmt);
			break;
		case OFC_PARSE_STMT_FORMAT:
			if (!ofc_parse_stmt_format_print(cs, stmt))
				return false;
			break;
		case OFC_PARSE_STMT_DATA:
			if (!ofc_parse_stmt_data_print(cs, stmt))
				return false;
			break;
		case OFC_PARSE_STMT_SAVE:
			if (!ofc_parse_stmt_save_print(cs, stmt))
				return false;
			break;
		case OFC_PARSE_STMT_PARAMETER:
			if (!ofc_parse_stmt_parameter_print(cs, stmt))
				return false;
			break;
		case OFC_PARSE_STMT_ASSIGN:
			if (!ofc_parse_stmt_assign_print(cs, stmt))
				return false;
			break;
		case OFC_PARSE_STMT_PUBLIC:
			if (!ofc_parse_stmt_public_print(cs, stmt))
				return false;
			break;
		case OFC_PARSE_STMT_PRIVATE:
			if (!ofc_parse_stmt_private_print(cs, stmt))
				return false;
			break;
		default:
			return false;
	}

	return true;
}



bool ofc_parse_stmt_sublist(
	ofc_parse_stmt_list_t* list,
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	if (!list)
		return false;

	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = 0;
	while (true)
	{
		unsigned slen;
		ofc_parse_stmt_t* stmt = ofc_parse_stmt(
			list, src, &ptr[i], debug, &slen);
		if (!stmt) break;
		i += slen;

		if (!ofc_parse_stmt_list_add(list, stmt))
		{
			ofc_parse_debug_rewind(debug, dpos);
			ofc_parse_stmt_delete(stmt);
			return false;
		}

		if (stmt->type == OFC_PARSE_STMT_ERROR)
			break;
	}

	if (len) *len = i;
	return true;
}

ofc_parse_stmt_list_t* ofc_parse_stmt_list(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	ofc_parse_stmt_list_t* list
		= ofc_parse_stmt_list_create();
	if (!list) return NULL;


	if (!ofc_parse_stmt_sublist(
		list, src, ptr, debug, len))
	{
		ofc_parse_stmt_list_delete(list);
		return NULL;
	}

	return list;
}



ofc_parse_stmt_list_t* ofc_parse_stmt_list_create(void)
{
	ofc_parse_stmt_list_t* list
		= (ofc_parse_stmt_list_t*)malloc(
			sizeof(ofc_parse_stmt_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->size  = 0;
	list->stmt = NULL;
	return list;
}

void ofc_parse_stmt_list_delete(
	ofc_parse_stmt_list_t* list)
{
	if (!list)
		return;

	unsigned i;
	for (i = 0; i < list->count; i++)
		ofc_parse_stmt_delete(list->stmt[i]);
	free(list->stmt);
	free(list);
}

bool ofc_parse_stmt_list_add(
	ofc_parse_stmt_list_t* list,
	ofc_parse_stmt_t* stmt)
{
	if (!list || !stmt)
		return false;

	if (list->count >= list->size)
	{
		unsigned nsize = (list->size << 1);
		if (nsize == 0) nsize = 16;

		ofc_parse_stmt_t** nstmt
			= (ofc_parse_stmt_t**)realloc(list->stmt,
				(nsize * sizeof(ofc_parse_stmt_t*)));
		if (!nstmt) return false;

		list->stmt = nstmt;
		list->size = nsize;
	}

	list->stmt[list->count++] = stmt;
	return true;
}

bool ofc_parse_stmt_list_foreach(
	const ofc_parse_stmt_list_t* list, void* context,
	bool (*callback)(const ofc_parse_stmt_t* stmt, void* context))
{
	if (!list || !callback)
		return false;

	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		const ofc_parse_stmt_t* stmt = list->stmt[i];

		if (!callback(stmt, context))
			return false;

		if (!stmt)
			continue;

		switch (stmt->type)
		{
			case OFC_PARSE_STMT_IF_THEN:
				if (stmt->if_then.block_then
					&& !ofc_parse_stmt_list_foreach(
						stmt->if_then.block_then,
						context, callback))
					return false;
				if (stmt->if_then.block_else
					&& !ofc_parse_stmt_list_foreach(
						stmt->if_then.block_else,
						context, callback))
					return false;
				break;

			case OFC_PARSE_STMT_SELECT_CASE:
				if (stmt->select_case.case_block)
				{
					unsigned i;
					for (i = 0; i < stmt->select_case.count; i++)
					{
						if (stmt->select_case.case_block[i]
							&& !ofc_parse_stmt_list_foreach(
								stmt->select_case.case_block[i],
								context, callback))
							return false;
					}
				}
				break;

			case OFC_PARSE_STMT_DO_BLOCK:
				if (stmt->do_block.block
					&& !ofc_parse_stmt_list_foreach(
						stmt->do_block.block,
						context, callback))
					return false;
				break;

			case OFC_PARSE_STMT_DO_WHILE_BLOCK:
				if (stmt->do_while_block.block
					&& !ofc_parse_stmt_list_foreach(
						stmt->do_while_block.block,
						context, callback))
					return false;
				break;

			default:
				break;
		}
	}

	return true;
}



bool ofc_parse_stmt_list_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_parse_stmt_list_t* list)
{
	if (!list)
		return false;

	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		if (!ofc_colstr_newline(cs, indent,
			(list->stmt[i]->label > 0
				? &list->stmt[i]->label : NULL)))
			return false;

		if (!ofc_parse_stmt_print(
			cs, indent, list->stmt[i]))
			return false;
	}

	return true;
}


bool ofc_parse_stmt_list_contains_error(
	const ofc_parse_stmt_list_t* list)
{
	if (!list || (list->count == 0)
		|| !list->stmt[list->count - 1])
		return false;

	return (list->stmt[list->count - 1]->type
		== OFC_PARSE_STMT_ERROR);
}
