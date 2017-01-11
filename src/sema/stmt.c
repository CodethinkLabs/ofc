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

#include "ofc/sema.h"

bool ofc_sema_stmt_assignment_print(ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt);
bool ofc_sema_stmt_if_comp_print(ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt);
bool ofc_sema_stmt_if_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_sema_stmt_t* stmt);
bool ofc_sema_stmt_if_then_print(
	ofc_colstr_t* cs, unsigned indent,
	ofc_sema_label_map_t* label_map,
	const ofc_sema_stmt_t* stmt);
bool ofc_sema_stmt_select_case_print(
	ofc_colstr_t* cs, unsigned indent,
	ofc_sema_label_map_t* label_map,
	const ofc_sema_stmt_t* stmt);
bool ofc_sema_stmt_do_label_print(ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt);
bool ofc_sema_stmt_do_block_print(
	ofc_colstr_t* cs, unsigned indent,
	ofc_sema_label_map_t* label_map,
	const ofc_sema_stmt_t* stmt);
bool ofc_sema_stmt_do_while_print(ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt);
bool ofc_sema_stmt_do_while_block_print(
	ofc_colstr_t* cs, unsigned indent,
	ofc_sema_label_map_t* label_map,
	const ofc_sema_stmt_t* stmt);
bool ofc_sema_stmt_cycle_exit_print(ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt);
bool ofc_sema_go_to_print(ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt);
bool ofc_sema_go_to_computed_print(ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt);
bool ofc_sema_stmt_stop_pause_print(ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt);
bool ofc_sema_stmt_assign_print(ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt);
bool ofc_sema_stmt_call_print(ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt);
bool ofc_sema_stmt_io_format_print(ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt);
bool ofc_sema_stmt_write_print(ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt);
bool ofc_sema_stmt_read_print(ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt);
bool ofc_sema_stmt_io_position_print(ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt);
bool ofc_sema_stmt_io_close_print(ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt);
bool ofc_sema_stmt_print_print(ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt);
bool ofc_sema_stmt_io_open_print(ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt);
bool ofc_sema_stmt_io_inquire_print(ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt);
bool ofc_sema_stmt_return_print(ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt);
bool ofc_sema_stmt_entry_print(ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt);



static ofc_sema_stmt_t* ofc_sema_stmt_simple(
	ofc_parse_stmt_e type)
{
	ofc_sema_stmt_e st;
	switch (type)
	{
		case OFC_PARSE_STMT_CONTINUE:
			st = OFC_SEMA_STMT_CONTINUE;
			break;

		case OFC_PARSE_STMT_CONTAINS:
			st = OFC_SEMA_STMT_CONTAINS;
			break;

		default:
			return NULL;
	}

	ofc_sema_stmt_t* stmt
		= (ofc_sema_stmt_t*)malloc(
			sizeof(ofc_sema_stmt_t));
	if (!stmt) return NULL;

	stmt->type = st;
	return stmt;
}


ofc_sema_stmt_t* ofc_sema_stmt(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	ofc_sema_stmt_t* s = NULL;
	switch (stmt->type)
	{
		case OFC_PARSE_STMT_ASSIGNMENT:
			s = ofc_sema_stmt_assignment(
				scope, stmt);
			break;

		case OFC_PARSE_STMT_ASSIGN:
			s = ofc_sema_stmt_assign(
				scope, stmt);
			break;

		case OFC_PARSE_STMT_CONTINUE:
			s = ofc_sema_stmt_simple(stmt->type);
			break;

		case OFC_PARSE_STMT_CONTAINS:
			s = ofc_sema_stmt_simple(stmt->type);
			break;

		case OFC_PARSE_STMT_FORMAT:
			s = ofc_sema_stmt_io_format(
				scope, stmt);
			break;

		case OFC_PARSE_STMT_IO_WRITE:
			s = ofc_sema_stmt_io_write(
				scope, stmt);
			break;

		case OFC_PARSE_STMT_IO_READ:
			s = ofc_sema_stmt_io_read(
				scope, stmt);
			break;

		case OFC_PARSE_STMT_IO_TYPE:
		case OFC_PARSE_STMT_IO_PRINT:
			s = ofc_sema_stmt_io_print(
				scope, stmt);
			break;
		case OFC_PARSE_STMT_IO_REWIND:
		case OFC_PARSE_STMT_IO_END_FILE:
		case OFC_PARSE_STMT_IO_BACKSPACE:
			s = ofc_sema_stmt_io_position(
				scope, stmt);
			break;
		case OFC_PARSE_STMT_IO_OPEN:
			s = ofc_sema_stmt_io_open(
				scope, stmt);
			break;
		case OFC_PARSE_STMT_IO_CLOSE:
			s = ofc_sema_stmt_io_close(
				scope, stmt);
			break;
		case OFC_PARSE_STMT_IO_INQUIRE:
			s = ofc_sema_stmt_io_inquire(
				scope, stmt);
			break;
		case OFC_PARSE_STMT_IF_THEN:
		case OFC_PARSE_STMT_IF_STATEMENT:
		case OFC_PARSE_STMT_IF_COMPUTED:
			s = ofc_sema_stmt_if(scope, stmt);
			break;

		case OFC_PARSE_STMT_SELECT_CASE:
			s = ofc_sema_stmt_select_case(scope, stmt);
			break;

		case OFC_PARSE_STMT_STOP:
		case OFC_PARSE_STMT_PAUSE:
			s = ofc_sema_stmt_stop_pause(scope, stmt);
			break;

		case OFC_PARSE_STMT_GO_TO:
		case OFC_PARSE_STMT_GO_TO_ASSIGNED:
		case OFC_PARSE_STMT_GO_TO_COMPUTED:
			s = ofc_sema_stmt_go_to(scope, stmt);
			break;

		case OFC_PARSE_STMT_DO_LABEL:
		case OFC_PARSE_STMT_DO_BLOCK:
		case OFC_PARSE_STMT_DO_WHILE:
		case OFC_PARSE_STMT_DO_WHILE_BLOCK:
			s = ofc_sema_stmt_do(scope, stmt);
			break;

		case OFC_PARSE_STMT_CYCLE:
		case OFC_PARSE_STMT_EXIT:
			s = ofc_sema_stmt_cycle_exit(scope, stmt);
			break;

		case OFC_PARSE_STMT_CALL:
			s = ofc_sema_stmt_call(scope, stmt);
			break;

		case OFC_PARSE_STMT_RETURN:
			s = ofc_sema_stmt_return(scope, stmt);
			break;

		case OFC_PARSE_STMT_ENTRY:
			s = ofc_sema_stmt_entry(scope, stmt);
			break;

		case OFC_PARSE_STMT_SEQUENCE:
			ofc_sparse_ref_error(stmt->src,
				"SEQUENCE only valid inside TYPE");
			break;

		default:
			ofc_sparse_ref_error(stmt->src,
				"Unsuported statement");
			break;
	}

	if (s) s->src = stmt->src;
	return s;
}

void ofc_sema_stmt_delete(
	ofc_sema_stmt_t* stmt)
{
	if (!stmt)
		return;

	switch (stmt->type)
	{
		case OFC_SEMA_STMT_ASSIGNMENT:
			ofc_sema_expr_delete(
				stmt->assignment.expr);
			ofc_sema_lhs_delete(
				stmt->assignment.dest);
			break;
		case OFC_SEMA_STMT_ASSIGN:
			ofc_sema_expr_delete(
				stmt->assign.label);
			break;
		case OFC_SEMA_STMT_IO_FORMAT:
			ofc_parse_format_desc_list_delete(
				stmt->io_format.format);
			break;
		case OFC_SEMA_STMT_IO_WRITE:
			ofc_sema_expr_delete(
				stmt->io_write.unit);
			ofc_sema_expr_delete(
				stmt->io_write.format);
			ofc_sema_expr_delete(
				stmt->io_write.iostat);
			ofc_sema_expr_delete(
				stmt->io_write.rec);
			ofc_sema_expr_delete(
				stmt->io_write.err);
			ofc_sema_expr_delete(
				stmt->io_write.advance);
			ofc_sema_expr_list_delete(
				stmt->io_write.iolist);
			break;
		case OFC_SEMA_STMT_IO_READ:
			ofc_sema_expr_delete(
				stmt->io_read.unit);
			ofc_sema_expr_delete(
				stmt->io_read.format);
			ofc_sema_expr_delete(
				stmt->io_read.iostat);
			ofc_sema_expr_delete(
				stmt->io_read.rec);
			ofc_sema_expr_delete(
				stmt->io_read.err);
			ofc_sema_expr_delete(
				stmt->io_read.advance);
			ofc_sema_expr_delete(
				stmt->io_read.end);
			ofc_sema_expr_delete(
				stmt->io_read.eor);
			ofc_sema_expr_delete(
				stmt->io_read.size);
			ofc_sema_lhs_list_delete(
				stmt->io_read.iolist);
			break;
		case OFC_SEMA_STMT_IO_PRINT:
			ofc_sema_expr_delete(
				stmt->io_print.format);
			ofc_sema_expr_list_delete(
				stmt->io_print.iolist);
			break;
		case OFC_SEMA_STMT_IO_REWIND:
		case OFC_SEMA_STMT_IO_END_FILE:
		case OFC_SEMA_STMT_IO_BACKSPACE:
			ofc_sema_expr_delete(
				stmt->io_position.unit);
			ofc_sema_expr_delete(
				stmt->io_position.iostat);
			ofc_sema_expr_delete(
				stmt->io_position.err);
			break;
		case OFC_SEMA_STMT_IO_OPEN:
			ofc_sema_expr_delete(
				stmt->io_open.unit);
			ofc_sema_expr_delete(
				stmt->io_open.access);
			ofc_sema_expr_delete(
				stmt->io_open.action);
			ofc_sema_expr_delete(
				stmt->io_open.blank);
			ofc_sema_expr_delete(
				stmt->io_open.delim);
			ofc_sema_expr_delete(
				stmt->io_open.err);
			ofc_sema_expr_delete(
				stmt->io_open.file);
			ofc_sema_expr_delete(
				stmt->io_open.form);
			ofc_sema_expr_delete(
				stmt->io_open.iostat);
			ofc_sema_expr_delete(
				stmt->io_open.pad);
			ofc_sema_expr_delete(
				stmt->io_open.position);
			ofc_sema_expr_delete(
				stmt->io_open.recl);
			ofc_sema_expr_delete(
				stmt->io_open.status);
			break;
		case OFC_SEMA_STMT_IO_CLOSE:
			ofc_sema_expr_delete(
				stmt->io_close.unit);
			ofc_sema_expr_delete(
				stmt->io_close.iostat);
			ofc_sema_expr_delete(
				stmt->io_close.err);
			ofc_sema_expr_delete(
				stmt->io_close.status);
			break;
		case OFC_SEMA_STMT_IO_INQUIRE:
			ofc_sema_expr_delete(
				stmt->io_inquire.unit);
			ofc_sema_expr_delete(
				stmt->io_inquire.file);
			ofc_sema_expr_delete(
				stmt->io_inquire.err);
			ofc_sema_lhs_delete(
				stmt->io_inquire.access);
			ofc_sema_lhs_delete(
				stmt->io_inquire.action);
			ofc_sema_lhs_delete(
				stmt->io_inquire.blank);
			ofc_sema_lhs_delete(
				stmt->io_inquire.delim);
			ofc_sema_lhs_delete(
				stmt->io_inquire.direct);
			ofc_sema_lhs_delete(
				stmt->io_inquire.exist);
			ofc_sema_lhs_delete(
				stmt->io_inquire.form);
			ofc_sema_lhs_delete(
				stmt->io_inquire.formatted);
			ofc_sema_lhs_delete(
				stmt->io_inquire.iostat);
			ofc_sema_lhs_delete(
				stmt->io_inquire.name);
			ofc_sema_lhs_delete(
				stmt->io_inquire.named);
			ofc_sema_lhs_delete(
				stmt->io_inquire.nextrec);
			ofc_sema_lhs_delete(
				stmt->io_inquire.number);
			ofc_sema_lhs_delete(
				stmt->io_inquire.opened);
			ofc_sema_lhs_delete(
				stmt->io_inquire.pad);
			ofc_sema_lhs_delete(
				stmt->io_inquire.position);
			ofc_sema_lhs_delete(
				stmt->io_inquire.read);
			ofc_sema_lhs_delete(
				stmt->io_inquire.readwrite);
			ofc_sema_lhs_delete(
				stmt->io_inquire.recl);
			ofc_sema_lhs_delete(
				stmt->io_inquire.sequential);
			ofc_sema_lhs_delete(
				stmt->io_inquire.unformatted);
			ofc_sema_lhs_delete(
				stmt->io_inquire.write);
			break;
		case OFC_SEMA_STMT_IF_COMPUTED:
			ofc_sema_expr_delete(
				stmt->if_comp.cond);
			ofc_sema_expr_list_delete(
				stmt->if_comp.label);
			break;
		case OFC_SEMA_STMT_IF_STATEMENT:
			ofc_sema_expr_delete(
				stmt->if_stmt.cond);
			ofc_sema_stmt_delete(
				stmt->if_stmt.stmt);
			break;
		case OFC_SEMA_STMT_IF_THEN:
			ofc_sema_expr_delete(
				stmt->if_then.cond);
			ofc_sema_stmt_list_delete(
				stmt->if_then.block_then);
			ofc_sema_stmt_list_delete(
				stmt->if_then.block_else);
			break;
		case OFC_SEMA_STMT_SELECT_CASE:
			{
				ofc_sema_expr_delete(
					stmt->select_case.case_expr);
				unsigned i;
				for (i = 0; i < stmt->select_case.count; i++)
				{
					ofc_sema_range_list_delete(
						stmt->select_case.case_value[i]);
					ofc_sema_stmt_list_delete(
						stmt->select_case.case_block[i]);
				}
				free(stmt->select_case.case_value);
				free(stmt->select_case.case_block);
			}
			break;
		case OFC_SEMA_STMT_STOP:
		case OFC_SEMA_STMT_PAUSE:
			ofc_sema_expr_delete(
				stmt->stop_pause.str);
			break;
		case OFC_SEMA_STMT_GO_TO:
			ofc_sema_expr_list_delete(
				stmt->go_to.allow);
			ofc_sema_expr_delete(
				stmt->go_to.label);
			break;
		case OFC_SEMA_STMT_GO_TO_COMPUTED:
			ofc_sema_expr_list_delete(
				stmt->go_to_comp.label);
			ofc_sema_expr_delete(
				stmt->go_to_comp.cond);
			break;
		case OFC_SEMA_STMT_DO_LABEL:
			ofc_sema_expr_delete(
				stmt->do_label.end_label);
			ofc_sema_lhs_delete(
				stmt->do_label.iter);
			ofc_sema_expr_delete(
				stmt->do_label.init);
			ofc_sema_expr_delete(
				stmt->do_label.last);
			ofc_sema_expr_delete(
				stmt->do_label.step);
			break;
		case OFC_SEMA_STMT_DO_BLOCK:
			ofc_sema_lhs_delete(
				stmt->do_block.iter);
			ofc_sema_expr_delete(
				stmt->do_block.init);
			ofc_sema_expr_delete(
				stmt->do_block.last);
			ofc_sema_expr_delete(
				stmt->do_block.step);
			ofc_sema_stmt_list_delete(
				stmt->do_block.block);
			break;
		case OFC_SEMA_STMT_DO_WHILE:
			ofc_sema_expr_delete(
				stmt->do_while.end_label);
			ofc_sema_expr_delete(
				stmt->do_while.cond);
			break;
		case OFC_SEMA_STMT_DO_WHILE_BLOCK:
			ofc_sema_expr_delete(
				stmt->do_while_block.cond);
			ofc_sema_stmt_list_delete(
				stmt->do_while_block.block);
			break;
		case OFC_SEMA_STMT_CALL:
			ofc_sema_dummy_arg_list_delete(stmt->call.args);
			break;
		case OFC_SEMA_STMT_RETURN:
			ofc_sema_expr_delete(
				stmt->alt_return);
			break;
		case OFC_SEMA_STMT_ENTRY:
			ofc_sema_arg_list_delete(
				stmt->entry.args);
			break;
		default:
			break;
	}

	free(stmt);
}



ofc_sema_stmt_list_t* ofc_sema_stmt_list_create(void)
{
	ofc_sema_stmt_list_t* list
		= (ofc_sema_stmt_list_t*)malloc(
			sizeof(ofc_sema_stmt_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->stmt  = NULL;
	return list;
}


static bool ofc_parse_stmt_is_exec(
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	switch (stmt->type)
	{
		case OFC_PARSE_STMT_EMPTY:
		case OFC_PARSE_STMT_INCLUDE:
		case OFC_PARSE_STMT_SUBROUTINE:
		case OFC_PARSE_STMT_FUNCTION:
		case OFC_PARSE_STMT_PROGRAM:
		case OFC_PARSE_STMT_BLOCK_DATA:
		case OFC_PARSE_STMT_PARAMETER:
		case OFC_PARSE_STMT_IMPLICIT_NONE:
		case OFC_PARSE_STMT_IMPLICIT:
		case OFC_PARSE_STMT_DECL:
		case OFC_PARSE_STMT_DATA:
		case OFC_PARSE_STMT_DIMENSION:
		case OFC_PARSE_STMT_EQUIVALENCE:
		case OFC_PARSE_STMT_COMMON:
		case OFC_PARSE_STMT_DECL_ATTR_AUTOMATIC:
		case OFC_PARSE_STMT_DECL_ATTR_STATIC:
		case OFC_PARSE_STMT_DECL_ATTR_VOLATILE:
		case OFC_PARSE_STMT_DECL_ATTR_EXTERNAL:
		case OFC_PARSE_STMT_DECL_ATTR_INTRINSIC:
		case OFC_PARSE_STMT_SAVE:
		case OFC_PARSE_STMT_TYPE:
		case OFC_PARSE_STMT_STRUCTURE:
		case OFC_PARSE_STMT_UNION:
		case OFC_PARSE_STMT_MAP:
		case OFC_PARSE_STMT_NAMELIST:
		case OFC_PARSE_STMT_POINTER:
		case OFC_PARSE_STMT_PUBLIC:
		case OFC_PARSE_STMT_PRIVATE:
			return false;

		default:
			break;
	}

	return true;
}

static bool ofc_sema_stmt_list__entry(
	ofc_sema_scope_t* scope,
	ofc_sema_stmt_list_t* list,
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	if (stmt->type == OFC_PARSE_STMT_EMPTY)
		return true;

	if (ofc_sema_stmt_is_stmt_func(scope, stmt))
		return ofc_sema_scope_stmt_func(scope, stmt);

	switch (stmt->type)
	{
		case OFC_PARSE_STMT_INCLUDE:
			/* Has no semantic effect. */
			return true;

		case OFC_PARSE_STMT_SUBROUTINE:
			return ofc_sema_scope_subroutine(scope, stmt);

		case OFC_PARSE_STMT_FUNCTION:
			return ofc_sema_scope_function(scope, stmt);

		case OFC_PARSE_STMT_PROGRAM:
			return ofc_sema_scope_program(scope, stmt);

		case OFC_PARSE_STMT_MODULE:
			return ofc_sema_scope_module(scope, stmt);

		case OFC_PARSE_STMT_BLOCK_DATA:
			return ofc_sema_scope_block_data(scope, stmt);

		case OFC_PARSE_STMT_PARAMETER:
			return ofc_sema_parameter(scope, stmt);

		case OFC_PARSE_STMT_IMPLICIT_NONE:
		case OFC_PARSE_STMT_IMPLICIT:
			return ofc_sema_implicit(scope, stmt);

		case OFC_PARSE_STMT_DECL:
			return ofc_sema_decl(scope, stmt);

		case OFC_PARSE_STMT_DATA:
			return ofc_sema_stmt_data(scope, stmt);

		case OFC_PARSE_STMT_DIMENSION:
			return ofc_sema_stmt_dimension(scope, stmt);

		case OFC_PARSE_STMT_EQUIVALENCE:
			/* We handle this next pass. */
			return true;

		case OFC_PARSE_STMT_COMMON:
			return ofc_sema_stmt_common(scope, stmt);

		case OFC_PARSE_STMT_DECL_ATTR_AUTOMATIC:
		case OFC_PARSE_STMT_DECL_ATTR_STATIC:
		case OFC_PARSE_STMT_DECL_ATTR_VOLATILE:
			return ofc_sema_stmt_decl_attr(scope, stmt);
		case OFC_PARSE_STMT_DECL_ATTR_INTRINSIC:
			return ofc_sema_stmt_intrinsic(scope, stmt);
		case OFC_PARSE_STMT_DECL_ATTR_EXTERNAL:
			return ofc_sema_stmt_external(scope, stmt);

		case OFC_PARSE_STMT_SAVE:
			return ofc_sema_stmt_save(scope, stmt);

		case OFC_PARSE_STMT_TYPE:
		case OFC_PARSE_STMT_STRUCTURE:
		case OFC_PARSE_STMT_UNION:
		case OFC_PARSE_STMT_MAP:
			return ofc_sema_structure(scope, stmt);

		case OFC_PARSE_STMT_USE:
			return ofc_sema_stmt_use(scope, stmt);

		case OFC_PARSE_STMT_NAMELIST:
		case OFC_PARSE_STMT_POINTER:
			ofc_sparse_ref_error(stmt->src,
				"Unsupported statement");
			/* TODO - Support these statements. */
			return false;

		case OFC_PARSE_STMT_PUBLIC:
			return ofc_sema_stmt_public(scope, stmt);

		case OFC_PARSE_STMT_PRIVATE:
			return ofc_sema_stmt_private(scope, stmt);

		default:
			break;
	}

	if (!scope)
		return false;

	switch (scope->type)
	{
		case OFC_SEMA_SCOPE_GLOBAL:
		case OFC_SEMA_SCOPE_BLOCK_DATA:
			ofc_sparse_ref_error(stmt->src,
				"Unexpected executable statement in scope.");
			return false;
		default:
			break;
	}

	if (stmt->label != 0)
	{
		if (ofc_sema_label_map_find(
			scope->label, stmt->label))
		{
			ofc_sparse_ref_error(stmt->src,
				"Duplicate label definition");
			return false;
		}
	}

	ofc_sema_stmt_t* s
		= ofc_sema_stmt(scope, stmt);

	/* Statement analysis failed,
	   should already have printed an error. */
	if (!s) return false;

	if ((s->type == OFC_SEMA_STMT_CONTINUE)
		&& (stmt->label == 0))
	{
		ofc_sparse_ref_warning(stmt->src,
			"Unlabelled CONTINUE statement has no effect");
	}

	if (!ofc_sema_stmt_list_add(list, s))
	{
		ofc_sema_stmt_delete(s);
		return false;
	}

	if ((stmt->label != 0)
		&& !ofc_sema_label_map_add_stmt(
			scope->label, stmt->label, s))
	{
		/* This should never happen. */
		abort();
	}

	return true;
}

static bool ofc_sema_stmt_list__type_scan(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt) return false;

	if (stmt->type != OFC_PARSE_STMT_DECL)
		return true;

	return ofc_sema_decl_type_scan(scope, stmt);
}

ofc_sema_stmt_list_t* ofc_sema_stmt_list(
	ofc_sema_scope_t* scope,
	ofc_sema_stmt_t*  block,
	const ofc_parse_stmt_list_t* body)
{
	if (!body)
		return NULL;

	ofc_sema_stmt_list_t* list
		= ofc_sema_stmt_list_create();;
	if (!list) return NULL;

	/* Scan ahead for typed declarations. */
	if (scope->args)
	{
		unsigned i;
		for (i = 0; i < body->count; i++)
		{
			if (!ofc_sema_stmt_list__type_scan(
				scope, body->stmt[i]))
			{
				ofc_sema_stmt_list_delete(list);
				return NULL;
			}
		}
	}

	unsigned i;
	for (i = 0; i < body->count; i++)
	{
		if (!ofc_sema_stmt_list__entry(
			scope, list, body->stmt[i]))
		{
			ofc_sema_stmt_list_delete(list);
			return NULL;
		}
	}

	/* Add labels from non-executable statements. */
	unsigned j;
	for (i = 0, j = 0; i < body->count; i++)
	{
		const ofc_parse_stmt_t* stmt
			= body->stmt[i];
		if (!stmt) continue;

		if (ofc_parse_stmt_is_exec(stmt))
		{
			j += 1;
			continue;
		}

		if (stmt->label == 0)
			continue;

		bool success;
		if (j < list->count)
		{
			success = ofc_sema_label_map_add_stmt(
				scope->label, stmt->label, list->stmt[j]);

		}
		else if (block)
		{
			success = ofc_sema_label_map_add_end_block(
				scope->label, stmt->label, block);
		}
		else
		{
			success = ofc_sema_label_map_add_end_scope(
					scope->label, stmt->label, scope);
		}

		if (!success)
		{
			ofc_sema_stmt_list_delete(list);
			return NULL;
		}

		ofc_sparse_ref_warning(stmt->src,
			"Label %u points to non-executable statement", stmt->label);
	}

	return list;
}

ofc_sema_stmt_t* ofc_sema_stmt_alloc(
	ofc_sema_stmt_t stmt)
{
	ofc_sema_stmt_t* astmt
		= (ofc_sema_stmt_t*)malloc(
			sizeof(ofc_sema_stmt_t));
	if (!astmt) return NULL;

	*astmt = stmt;
	return astmt;
}

void ofc_sema_stmt_list_delete(
	ofc_sema_stmt_list_t* list)
{
	if (!list)
		return;

	unsigned i;
	for (i = 0; i < list->count; i++)
		ofc_sema_stmt_delete(list->stmt[i]);
	free(list->stmt);

	free(list);
}

bool ofc_sema_stmt_list_add(
	ofc_sema_stmt_list_t* list,
	ofc_sema_stmt_t* stmt)
{
	if (!list || !stmt)
		return false;

	ofc_sema_stmt_t** nstmt
		= (ofc_sema_stmt_t**)realloc(list->stmt,
			(sizeof(ofc_sema_stmt_t*) * (list->count + 1)));
	if (!nstmt) return NULL;
	list->stmt = nstmt;

	list->stmt[list->count++] = stmt;

	return true;
}

bool ofc_sema_stmt_list_remove(
	ofc_sema_stmt_list_t* list,
	ofc_sema_stmt_t* stmt)
{
	if (!list || !stmt)
		return false;

	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		if (list->stmt[i] == stmt)
		{
			list->stmt[i] = NULL;
			return true;
		}
	}

	return false;
}

unsigned ofc_sema_stmt_list_count(
	const ofc_sema_stmt_list_t* list)
{
	return (list ? list->count : 0);
}

bool ofc_sema_stmt_list_foreach(
	ofc_sema_stmt_list_t* list, void* param,
	bool (*func)(ofc_sema_stmt_t* stmt, void* param))
{
	if (!list || !func)
		return false;

	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		if (!list->stmt[i]) continue;

		ofc_sema_stmt_t* stmt = list->stmt[i];

		if (!func(stmt, param))
			return false;

		if (!stmt) continue;

		switch (stmt->type)
		{
			case OFC_SEMA_STMT_IF_THEN:
				if (stmt->if_then.block_then
					&& !ofc_sema_stmt_list_foreach(
						stmt->if_then.block_then, param, func))
					return false;
				if (stmt->if_then.block_else
					&& !ofc_sema_stmt_list_foreach(
						stmt->if_then.block_else, param, func))
					return false;
				break;

			case OFC_SEMA_STMT_SELECT_CASE:
				if (stmt->select_case.case_block)
				{
					unsigned i;
					for (i = 0; i < stmt->select_case.count; i++)
					{
						if (stmt->select_case.case_block[i]
							&& !ofc_sema_stmt_list_foreach(
								stmt->select_case.case_block[i],
								param, func))
							return false;
					}
				}
				break;

			case OFC_SEMA_STMT_DO_BLOCK:
				if (stmt->do_block.block
					&& !ofc_sema_stmt_list_foreach(
						stmt->do_block.block, param, func))
					return false;
				break;

			case OFC_SEMA_STMT_DO_WHILE_BLOCK:
				if (stmt->do_while_block.block
					&& !ofc_sema_stmt_list_foreach(
						stmt->do_while_block.block, param, func))
					return false;
				break;

			default:
				break;
		}
	}

	return true;
}


bool ofc_sema_stmt_foreach_expr(
	ofc_sema_stmt_t* stmt, void* param,
	bool (*func)(ofc_sema_expr_t* expr, void* param))
{
	if (!stmt || !func)
		return false;

	switch (stmt->type)
	{
		case OFC_SEMA_STMT_ASSIGNMENT:
			if (stmt->assignment.expr && !ofc_sema_expr_foreach(
				stmt->assignment.expr, param, func))
				return false;
			break;

		case OFC_SEMA_STMT_ASSIGN:
			if (stmt->assign.label && !ofc_sema_expr_foreach(
				stmt->assign.label, param, func))
				return false;
			break;

		case OFC_SEMA_STMT_IO_FORMAT:
			break;

		case OFC_SEMA_STMT_IO_WRITE:
			if (stmt->io_write.unit && !ofc_sema_expr_foreach(
				stmt->io_write.unit, param, func))
				return false;
			if (stmt->io_write.format && !ofc_sema_expr_foreach(
				stmt->io_write.format, param, func))
				return false;
			if (stmt->io_write.iostat && !ofc_sema_expr_foreach(
				stmt->io_write.iostat, param, func))
				return false;
			if (stmt->io_write.rec && !ofc_sema_expr_foreach(
				stmt->io_write.rec, param, func))
				return false;
			if (stmt->io_write.err && !ofc_sema_expr_foreach(
				stmt->io_write.err, param, func))
				return false;
			if (stmt->io_write.advance && !ofc_sema_expr_foreach(
				stmt->io_write.advance, param, func))
				return false;
			if (stmt->io_write.iolist && !ofc_sema_expr_list_foreach(
				stmt->io_write.iolist, param, func))
				return false;
			break;

		case OFC_SEMA_STMT_IO_READ:
			if (stmt->io_read.unit && !ofc_sema_expr_foreach(
				stmt->io_read.unit, param, func))
				return false;
			if (stmt->io_read.format && !ofc_sema_expr_foreach(
				stmt->io_read.format, param, func))
				return false;
			if (stmt->io_read.iostat && !ofc_sema_expr_foreach(
				stmt->io_read.iostat, param, func))
				return false;
			if (stmt->io_read.rec && !ofc_sema_expr_foreach(
				stmt->io_read.rec, param, func))
				return false;
			if (stmt->io_read.err && !ofc_sema_expr_foreach(
				stmt->io_read.err, param, func))
				return false;
			if (stmt->io_read.advance && !ofc_sema_expr_foreach(
				stmt->io_read.advance, param, func))
				return false;
			if (stmt->io_read.end && !ofc_sema_expr_foreach(
				stmt->io_read.end, param, func))
				return false;
			if (stmt->io_read.eor && !ofc_sema_expr_foreach(
				stmt->io_read.eor, param, func))
				return false;
			if (stmt->io_read.size && !ofc_sema_expr_foreach(
				stmt->io_read.size, param, func))
				return false;
			break;

		case OFC_SEMA_STMT_IO_PRINT:
			if (stmt->io_print.format && !ofc_sema_expr_foreach(
				stmt->io_print.format, param, func))
				return false;
			if (stmt->io_print.iolist
				&& !ofc_sema_expr_list_foreach(
					stmt->io_print.iolist, param, func))
				return false;
			break;

		case OFC_SEMA_STMT_IO_REWIND:
		case OFC_SEMA_STMT_IO_END_FILE:
		case OFC_SEMA_STMT_IO_BACKSPACE:
			if (stmt->io_position.unit && !ofc_sema_expr_foreach(
				stmt->io_position.unit, param, func))
				return false;
			if (stmt->io_position.iostat && !ofc_sema_expr_foreach(
				stmt->io_position.iostat, param, func))
				return false;
			if (stmt->io_position.err && !ofc_sema_expr_foreach(
				stmt->io_position.err, param, func))
				return false;
			break;

		case OFC_SEMA_STMT_IO_OPEN:
			if (stmt->io_open.unit && !ofc_sema_expr_foreach(
				stmt->io_open.unit, param, func))
				return false;
			if (stmt->io_open.iostat && !ofc_sema_expr_foreach(
				stmt->io_open.iostat, param, func))
				return false;
			if (stmt->io_open.err && !ofc_sema_expr_foreach(
				stmt->io_open.err, param, func))
				return false;
			if (stmt->io_open.recl && !ofc_sema_expr_foreach(
				stmt->io_open.recl, param, func))
				return false;
			if (stmt->io_open.access && !ofc_sema_expr_foreach(
				stmt->io_open.access, param, func))
				return false;
			if (stmt->io_open.action && !ofc_sema_expr_foreach(
				stmt->io_open.action, param, func))
				return false;
			if (stmt->io_open.blank && !ofc_sema_expr_foreach(
				stmt->io_open.blank, param, func))
				return false;
			if (stmt->io_open.delim && !ofc_sema_expr_foreach(
				stmt->io_open.delim, param, func))
				return false;
			if (stmt->io_open.file && !ofc_sema_expr_foreach(
				stmt->io_open.file, param, func))
				return false;
			if (stmt->io_open.form && !ofc_sema_expr_foreach(
				stmt->io_open.form, param, func))
				return false;
			if (stmt->io_open.pad && !ofc_sema_expr_foreach(
				stmt->io_open.pad, param, func))
				return false;
			if (stmt->io_open.position && !ofc_sema_expr_foreach(
				stmt->io_open.position, param, func))
				return false;
			if (stmt->io_open.status && !ofc_sema_expr_foreach(
				stmt->io_open.status, param, func))
				return false;
			break;

		case OFC_SEMA_STMT_IO_CLOSE:
			if (stmt->io_close.unit && !ofc_sema_expr_foreach(
				stmt->io_close.unit, param, func))
				return false;
			if (stmt->io_close.iostat && !ofc_sema_expr_foreach(
				stmt->io_close.iostat, param, func))
				return false;
			if (stmt->io_close.err && !ofc_sema_expr_foreach(
				stmt->io_close.err, param, func))
				return false;
			if (stmt->io_close.status && !ofc_sema_expr_foreach(
				stmt->io_close.status, param, func))
				return false;
			break;

		case OFC_SEMA_STMT_IO_INQUIRE:
			if (stmt->io_inquire.unit && !ofc_sema_expr_foreach(
				stmt->io_inquire.unit, param, func))
				return false;
			if (stmt->io_inquire.file && !ofc_sema_expr_foreach(
				stmt->io_inquire.file, param, func))
				return false;
			if (stmt->io_inquire.err && !ofc_sema_expr_foreach(
				stmt->io_inquire.err, param, func))
				return false;
			break;

		case OFC_SEMA_STMT_CONTINUE:
		case OFC_SEMA_STMT_CONTAINS:
			break;

		case OFC_SEMA_STMT_IF_COMPUTED:
			if (stmt->if_comp.cond && !ofc_sema_expr_foreach(
				stmt->if_comp.cond, param, func))
				return false;
			if (stmt->if_comp.label && !ofc_sema_expr_list_foreach(
				stmt->if_comp.label, param, func))
				return false;
			break;

		case OFC_SEMA_STMT_IF_STATEMENT:
			if (stmt->if_stmt.cond && !ofc_sema_expr_foreach(
				stmt->if_stmt.cond, param, func))
				return false;
			if (stmt->if_stmt.stmt && !ofc_sema_stmt_foreach_expr(
				stmt->if_stmt.stmt, param, func))
				return false;
			break;

		case OFC_SEMA_STMT_IF_THEN:
			if (stmt->if_then.cond && !ofc_sema_expr_foreach(
				stmt->if_then.cond, param, func))
				return false;
			if (stmt->if_then.block_then && !ofc_sema_stmt_list_foreach_expr(
				stmt->if_then.block_then, param, func))
				return false;
			if (stmt->if_then.block_else && !ofc_sema_stmt_list_foreach_expr(
				stmt->if_then.block_else, param, func))
				return false;
			break;

		case OFC_SEMA_STMT_SELECT_CASE:
			{
				if (stmt->select_case.case_expr && !ofc_sema_expr_foreach(
					stmt->select_case.case_expr, param, func))
					return false;
				unsigned i;
				for (i = 0; i < stmt->select_case.count; i++)
				{
					if (stmt->select_case.case_block[i] && !ofc_sema_stmt_list_foreach_expr(
						stmt->select_case.case_block[i], param, func))
						return false;
				}
			}
			break;

		case OFC_SEMA_STMT_STOP:
		case OFC_SEMA_STMT_PAUSE:
			if (stmt->stop_pause.str && !ofc_sema_expr_foreach(
				stmt->stop_pause.str, param, func))
				return false;
			break;

		case OFC_SEMA_STMT_GO_TO:
			if (stmt->go_to.label && !ofc_sema_expr_foreach(
				stmt->go_to.label, param, func))
				return false;
			if (stmt->go_to.allow && !ofc_sema_expr_list_foreach(
				stmt->go_to.allow, param, func))
				return false;
			break;

		case OFC_SEMA_STMT_GO_TO_COMPUTED:
			if (stmt->go_to_comp.cond && !ofc_sema_expr_foreach(
				stmt->go_to_comp.cond, param, func))
				return false;
			if (stmt->go_to_comp.label && !ofc_sema_expr_list_foreach(
				stmt->go_to_comp.label, param, func))
				return false;
			break;

		case OFC_SEMA_STMT_DO_LABEL:
			if (stmt->do_label.end_label && !ofc_sema_expr_foreach(
				stmt->do_label.end_label, param, func))
				return false;
			if (stmt->do_label.init && !ofc_sema_expr_foreach(
				stmt->do_label.init, param, func))
				return false;
			if (stmt->do_label.last && !ofc_sema_expr_foreach(
				stmt->do_label.last, param, func))
				return false;
			if (stmt->do_label.step && !ofc_sema_expr_foreach(
				stmt->do_label.step, param, func))
				return false;
			break;

		case OFC_SEMA_STMT_DO_BLOCK:
			if (stmt->do_block.init && !ofc_sema_expr_foreach(
				stmt->do_block.init, param, func))
				return false;
			if (stmt->do_block.last && !ofc_sema_expr_foreach(
				stmt->do_block.last, param, func))
				return false;
			if (stmt->do_block.step && !ofc_sema_expr_foreach(
				stmt->do_block.step, param, func))
				return false;
			if (stmt->do_block.block && !ofc_sema_stmt_list_foreach_expr(
				stmt->do_block.block, param, func))
				return false;
			break;

		case OFC_SEMA_STMT_DO_WHILE:
			if (stmt->do_while.end_label && !ofc_sema_expr_foreach(
				stmt->do_while.end_label, param, func))
				return false;
			if (stmt->do_while.cond && !ofc_sema_expr_foreach(
				stmt->do_while.cond, param, func))
				return false;
			break;

		case OFC_SEMA_STMT_DO_WHILE_BLOCK:
			if (stmt->do_while_block.cond && !ofc_sema_expr_foreach(
				stmt->do_while_block.cond, param, func))
				return false;
			if (stmt->do_while_block.block && !ofc_sema_stmt_list_foreach_expr(
				stmt->do_while_block.block, param, func))
				return false;
			break;

		case OFC_SEMA_STMT_CALL:
			if (stmt->call.args && !ofc_sema_dummy_arg_list_foreach_expr(
				stmt->call.args, param, func))
				return false;
			break;

		case OFC_SEMA_STMT_RETURN:
			if (stmt->alt_return && !ofc_sema_expr_foreach(
				stmt->alt_return, param, func))
				return false;
			break;

		case OFC_SEMA_STMT_ENTRY:
		case OFC_SEMA_STMT_CYCLE:
		case OFC_SEMA_STMT_EXIT:
			break;

		default:
			return false;
	}

	return true;
}

bool ofc_sema_stmt_list_foreach_expr(
	ofc_sema_stmt_list_t* list, void* param,
	bool (*func)(ofc_sema_expr_t* expr, void* param))
{
	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		if (!list->stmt[i]) continue;

		if (!ofc_sema_stmt_foreach_expr(
			list->stmt[i], param, func))
			return false;
	}

	return true;
}


bool ofc_sema_stmt_print(
	ofc_colstr_t* cs, unsigned indent,
	ofc_sema_label_map_t* label_map,
	const ofc_sema_stmt_t* stmt)
{
	if (!stmt) return false;

	switch (stmt->type)
	{
		case OFC_SEMA_STMT_ASSIGNMENT:
			return ofc_sema_stmt_assignment_print(cs, stmt);

		case OFC_SEMA_STMT_ASSIGN:
			return ofc_sema_stmt_assign_print(cs, stmt);

		case OFC_SEMA_STMT_IO_FORMAT:
			return ofc_sema_stmt_io_format_print(cs, stmt);

		case OFC_SEMA_STMT_IO_WRITE:
			return ofc_sema_stmt_write_print(cs, stmt);

		case OFC_SEMA_STMT_IO_READ:
			return ofc_sema_stmt_read_print(cs, stmt);

		case OFC_SEMA_STMT_IO_PRINT:
			return ofc_sema_stmt_print_print(cs, stmt);

		case OFC_SEMA_STMT_IO_REWIND:
		case OFC_SEMA_STMT_IO_END_FILE:
		case OFC_SEMA_STMT_IO_BACKSPACE:
			return ofc_sema_stmt_io_position_print(cs, stmt);

		case OFC_SEMA_STMT_IO_OPEN:
			return ofc_sema_stmt_io_open_print(cs, stmt);

		case OFC_SEMA_STMT_IO_CLOSE:
			return ofc_sema_stmt_io_close_print(cs, stmt);

		case OFC_SEMA_STMT_IO_INQUIRE:
			return ofc_sema_stmt_io_inquire_print(cs, stmt);

		case OFC_SEMA_STMT_CONTAINS:
			return ofc_colstr_keyword_atomic_writez(cs, "CONTAINS");

		case OFC_SEMA_STMT_CONTINUE:
			return ofc_colstr_keyword_atomic_writez(cs, "CONTINUE");

		case OFC_SEMA_STMT_IF_COMPUTED:
			return ofc_sema_stmt_if_comp_print(cs, stmt);

		case OFC_SEMA_STMT_IF_STATEMENT:
			return ofc_sema_stmt_if_print(cs, indent, stmt);

		case OFC_SEMA_STMT_IF_THEN:
			return ofc_sema_stmt_if_then_print(
				cs, indent, label_map, stmt);

		case OFC_SEMA_STMT_SELECT_CASE:
			return ofc_sema_stmt_select_case_print(
				cs, indent, label_map, stmt);

		case OFC_SEMA_STMT_STOP:
		case OFC_SEMA_STMT_PAUSE:
			return ofc_sema_stmt_stop_pause_print(cs, stmt);

		case OFC_SEMA_STMT_GO_TO:
			return ofc_sema_go_to_print(cs, stmt);

		case OFC_SEMA_STMT_GO_TO_COMPUTED:
			return ofc_sema_go_to_computed_print(cs, stmt);

		case OFC_SEMA_STMT_DO_LABEL:
			return ofc_sema_stmt_do_label_print(cs, stmt);

		case OFC_SEMA_STMT_DO_BLOCK:
			return ofc_sema_stmt_do_block_print(
				cs, indent, label_map, stmt);

		case OFC_SEMA_STMT_DO_WHILE:
			return ofc_sema_stmt_do_while_print(cs, stmt);

		case OFC_SEMA_STMT_DO_WHILE_BLOCK:
			return ofc_sema_stmt_do_while_block_print(
				cs, indent, label_map, stmt);

		case OFC_SEMA_STMT_CYCLE:
		case OFC_SEMA_STMT_EXIT:
			return ofc_sema_stmt_cycle_exit_print(cs, stmt);

		case OFC_SEMA_STMT_CALL:
			return ofc_sema_stmt_call_print(cs, stmt);

		case OFC_SEMA_STMT_RETURN:
			return ofc_sema_stmt_return_print(cs, stmt);

		case OFC_SEMA_STMT_ENTRY:
			return ofc_sema_stmt_entry_print(cs, stmt);

		default:
			break;
	}

	return false;
}

static const char* ofc_sema_stmt__name[] =
{
	"ASSIGNMENT",
	"ASSIGN",
	"IO_FORMAT",
	"IO_WRITE",
	"IO_READ",
	"IO_PRINT",
	"IO_REWIND",
	"IO_END_FILE",
	"IO_BACKSPACE",
	"IO_OPEN",
	"IO_CLOSE",
	"IO_INQUIRE",
	"CONTINUE",
	"IF_COMPUTED",
	"IF_STATEMENT",
	"IF_THEN",
	"SELECT_CASE",
	"STOP",
	"PAUSE",
	"GO_TO",
	"GO_TO_COMPUTED",
	"DO_LABEL",
	"DO_BLOCK",
	"DO_WHILE",
	"DO_WHILE_BLOCK",
	"CALL",
	"RETURN",
	"ENTRY",
	"CYCLE",
	"EXIT",
	"PUBLIC",
	"PRIVATE",

	NULL
};

static const char* ofc_sema_stmt__str_rep(
	const ofc_sema_stmt_t* stmt)
{
	if (!stmt || (stmt->type >= OFC_SEMA_STMT_COUNT))
		return NULL;

	return ofc_sema_stmt__name[stmt->type];
}

bool ofc_sema_stmt_list_print(
	ofc_colstr_t* cs, unsigned indent,
	ofc_sema_label_map_t* label_map,
	const ofc_sema_stmt_list_t* stmt_list)
{
	if (!cs || !stmt_list)
		return false;

	unsigned i;
	for (i = 0; i < stmt_list->count; i++)
	{
		if (stmt_list->stmt[i])
		{
			const ofc_sema_label_t* label
				= ofc_sema_label_map_find_stmt(
					label_map, stmt_list->stmt[i]);
			if (label)
			{
				unsigned label_num = label->number;
				if (!ofc_colstr_newline(cs, indent, &label_num))
					return false;
			}
			else
			{
				if (!ofc_colstr_newline(cs, indent, NULL))
					return false;
			}

			if (!ofc_sema_stmt_print(cs, indent,
				label_map, stmt_list->stmt[i]))
			{
				ofc_file_error(NULL, NULL, "Failed to print statement: %s",
					ofc_sema_stmt__str_rep(stmt_list->stmt[i]));
				return false;
			}
		}
	}
	return true;
}
