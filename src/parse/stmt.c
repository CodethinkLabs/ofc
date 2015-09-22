#include "parse.h"

unsigned parse_stmt_program(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_subroutine(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_function(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_block_data(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_implicit(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_call(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_decl(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_common(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_dimension(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_equivalence(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_assignment(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_continue(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_stop(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_pause(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_return(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_external(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_intrinsic(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_go_to(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_if(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_do(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_format(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_data(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_save(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_parameter(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_assign(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);

unsigned parse_stmt_io_open(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_io_rewind(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_io_backspace(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_io_read(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_io_write(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_io_end_file(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);



static void parse_stmt__cleanup(
	parse_stmt_t stmt)
{
	unsigned i;
	switch (stmt.type)
	{
		case PARSE_STMT_PROGRAM:
		case PARSE_STMT_SUBROUTINE:
		case PARSE_STMT_FUNCTION:
		case PARSE_STMT_BLOCK_DATA:
			parse_stmt_list_delete(stmt.program.body);
			parse_lhs_list_delete(stmt.program.args);
			parse_type_delete(stmt.program.type);
			break;
		case PARSE_STMT_ASSIGNMENT:
			parse_assign_delete(stmt.assignment);
			break;
		case PARSE_STMT_IMPLICIT:
			parse_implicit_list_delete(stmt.implicit);
			break;
		case PARSE_STMT_CALL:
			parse_call_arg_list_delete(stmt.call.args);
			break;
		case PARSE_STMT_DECL:
			parse_type_delete(stmt.decl.type);
			parse_decl_list_delete(stmt.decl.decl);
			break;
		case PARSE_STMT_COMMON:
			parse_common_group_list_delete(stmt.common);
			break;
		case PARSE_STMT_DIMENSION:
			parse_lhs_list_delete(stmt.dimension);
			break;
		case PARSE_STMT_EQUIVALENCE:
			parse_list_delete(
				stmt.equivalence.count,
				(void**)stmt.equivalence.group,
				(void*)parse_lhs_list_delete);
			break;
		case PARSE_STMT_STOP:
		case PARSE_STMT_PAUSE:
		case PARSE_STMT_RETURN:
			parse_expr_delete(stmt.stop_pause_return.value);
			break;
		case PARSE_STMT_EXTERNAL:
		case PARSE_STMT_INTRINSIC:
			parse_list_delete(
				stmt.external_intrinsic.count,
				(void**)stmt.external_intrinsic.name,
				free);
			break;
		case PARSE_STMT_GO_TO_ASSIGNED:
			free(stmt.go_to_assign.label);
			break;
		case PARSE_STMT_GO_TO_COMPUTED:
			free(stmt.go_to_comp.label);
			parse_expr_delete(stmt.go_to_comp.cond);
			break;
		case PARSE_STMT_IF_COMPUTED:
			parse_expr_delete(stmt.if_comp.cond);
			free(stmt.if_comp.label);
			break;
		case PARSE_STMT_IF_STATEMENT:
			parse_expr_delete(stmt.if_stmt.cond);
			parse_stmt_delete(stmt.if_stmt.stmt);
			break;
		case PARSE_STMT_IF_THEN:
			parse_expr_delete(stmt.if_then.cond);
			parse_stmt_list_delete(stmt.if_then.block_then);
			parse_stmt_list_delete(stmt.if_then.block_else);
			break;
		case PARSE_STMT_DO:
			parse_assign_delete(stmt.do_loop.init);
			parse_expr_delete(stmt.do_loop.last);
			parse_expr_delete(stmt.do_loop.step);
			break;
		case PARSE_STMT_IO_REWIND:
		case PARSE_STMT_IO_BACKSPACE:
		case PARSE_STMT_IO_READ:
		case PARSE_STMT_IO_WRITE:
		case PARSE_STMT_IO_END_FILE:
			parse_expr_delete(stmt.io.unit);
			parse_expr_delete(stmt.io.fmt);
			parse_expr_delete(stmt.io.rec);
			parse_expr_delete(stmt.io.end);
			parse_expr_delete(stmt.io.iostat);
			parse_expr_delete(stmt.io.err);
			parse_iolist_delete(stmt.io.args);
			break;
		case PARSE_STMT_IO_OPEN:
			parse_expr_delete(stmt.io_open.unit);
			parse_expr_delete(stmt.io_open.access);
			parse_expr_delete(stmt.io_open.blank);
			parse_expr_delete(stmt.io_open.err);
			parse_expr_delete(stmt.io_open.form);
			parse_expr_delete(stmt.io_open.iostat);
			parse_expr_delete(stmt.io_open.recl);
			parse_expr_delete(stmt.io_open.status);
			parse_expr_delete(stmt.io_open.fileopt);
			parse_expr_delete(stmt.io_open.action);
			break;
		case PARSE_STMT_FORMAT:
			parse_format_desc_list_delete(
				stmt.format.desc, stmt.format.desc_count);
			break;
		case PARSE_STMT_DATA:
			for (i = 0; i < stmt.data.init_count; i++)
				parse_expr_delete(stmt.data.init[i]);
			free(stmt.data.init);
			for (i = 0; i < stmt.data.name_count; i++)
				parse_lhs_delete(stmt.data.name[i]);
			free(stmt.data.name);
			break;
		case PARSE_STMT_SAVE:
			parse_save_list_delete(stmt.save.list);
			break;
		case PARSE_STMT_PARAMETER:
			parse_assign_list_delete(stmt.parameter.list);
			break;
		default:
			break;
	}
}

static parse_stmt_t* parse_stmt__alloc(
	parse_stmt_t stmt)
{
	parse_stmt_t* astmt
		= (parse_stmt_t*)malloc(
			sizeof(parse_stmt_t));
	if (!astmt) return NULL;

	*astmt = stmt;
	return astmt;
}



parse_stmt_t* parse_stmt(
	const sparse_t* src, const char* ptr,
	unsigned* len)
{
	if (ptr[0] == '\0')
		return NULL;

	parse_stmt_t stmt;
	stmt.type  = PARSE_STMT_EMPTY;

	/* TODO - Allow handling of label 0? */
	stmt.label = 0;
	sparse_label_find(src, ptr, &stmt.label);

	unsigned i = 0;
	switch (toupper(ptr[0]))
	{
		case 'A':
			if (i == 0) i = parse_stmt_assign(src, ptr, &stmt);
			break;

		case 'B':
			if (i == 0) i = parse_stmt_io_backspace(src, ptr, &stmt);
			if (i == 0) i = parse_stmt_block_data(src, ptr, &stmt);
			break;

		case 'C':
			if (i == 0) i = parse_stmt_continue(src, ptr, &stmt);
			if (i == 0) i = parse_stmt_call(src, ptr, &stmt);
			if (i == 0) i = parse_stmt_common(src, ptr, &stmt);
			break;

		case 'D':
			if (i == 0) i = parse_stmt_do(src, ptr, &stmt);
			if (i == 0) i = parse_stmt_data(src, ptr, &stmt);
			if (i == 0) i = parse_stmt_dimension(src, ptr, &stmt);
			break;

		case 'E':
			if (i == 0) i = parse_stmt_equivalence(src, ptr, &stmt);
			if (i == 0) i = parse_stmt_io_end_file(src, ptr, &stmt);
			if (i == 0) i = parse_stmt_external(src, ptr, &stmt);
			break;

		case 'F':
			if (i == 0) i = parse_stmt_format(src, ptr, &stmt);
			break;

		case 'G':
			if (i == 0) i = parse_stmt_go_to(src, ptr, &stmt);
			break;

		case 'I':
			if (i == 0) i = parse_stmt_implicit(src, ptr, &stmt);
			if (i == 0) i = parse_stmt_if(src, ptr, &stmt);
			if (i == 0) i = parse_stmt_intrinsic(src, ptr, &stmt);
			break;

		case 'O':
			if (i == 0) i = parse_stmt_io_open(src, ptr, &stmt);
			break;

		case 'P':
			if (i == 0) i = parse_stmt_parameter(src, ptr, &stmt);
			if (i == 0) i = parse_stmt_program(src, ptr, &stmt);
			if (i == 0) i = parse_stmt_pause(src, ptr, &stmt);
			break;

		case 'R':
			if (i == 0) i = parse_stmt_return(src, ptr, &stmt);
			if (i == 0) i = parse_stmt_io_read(src, ptr, &stmt);
			if (i == 0) i = parse_stmt_io_rewind(src, ptr, &stmt);
			break;

		case 'S':
			if (i == 0) i = parse_stmt_subroutine(src, ptr, &stmt);
			if (i == 0) i = parse_stmt_stop(src, ptr, &stmt);
			if (i == 0) i = parse_stmt_save(src, ptr, &stmt);
			break;

		case 'W':
			if (i == 0) i = parse_stmt_io_write(src, ptr, &stmt);
			break;

		default:
			break;
	}

	/* Drop incomplete statements, they may be an assignment or declaration. */
	if ((i > 0)
		&& !is_end_statement(&ptr[i], NULL))
	{
		parse_stmt__cleanup(stmt);
		i = 0;
	}

	if (i == 0) i = parse_stmt_function(src, ptr, &stmt);
	if (i == 0) i = parse_stmt_decl(src, ptr, &stmt);

	if (i == 0)
	{
		stmt.assignment = parse_assign(src, ptr, &i);
		if (stmt.assignment)
		{
			if (!stmt.assignment->init)
			{
				i = 0;
				parse_assign_delete(stmt.assignment);
			}
			else
			{
				stmt.type = PARSE_STMT_ASSIGNMENT;
			}
		}
	}

	unsigned l = 0;
	if (!is_end_statement(&ptr[i], &l))
	{
		if (i == 0)
			return NULL;

		sparse_error(src, &ptr[i],
			"Expected newline or semicolon after statement");
		parse_stmt__cleanup(stmt);
		return NULL;
	}
	i += l;

	parse_stmt_t* astmt
		= parse_stmt__alloc(stmt);
	if (!astmt)
	{
		parse_stmt__cleanup(stmt);
		return NULL;
	}

	if (len) *len = i;
	return astmt;
}

void parse_stmt_delete(
	parse_stmt_t* stmt)
{
	if (!stmt)
		return;

	parse_stmt__cleanup(*stmt);
	free(stmt);
}


parse_stmt_list_t* parse_stmt_list(
	const sparse_t* src, const char* ptr,
	unsigned* len)
{
	parse_stmt_list_t* list
		= (parse_stmt_list_t*)malloc(
			sizeof(parse_stmt_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->stmt = NULL;

	unsigned i = parse_list(src, ptr, '\0',
		&list->count, (void***)&list->stmt,
		(void*)parse_stmt,
		(void*)parse_stmt_delete);
	if (i == 0) return NULL;

	if (len) *len = i;
	return list;
}

void parse_stmt_list_delete(
	parse_stmt_list_t* list)
{
	if (!list)
		return;

	parse_list_delete(
		list->count, (void**)list->stmt,
		(void*)parse_stmt_delete);
	free(list);
}
