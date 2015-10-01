#include "parse.h"

unsigned parse_stmt_include(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_program(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_subroutine(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_function(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_block_data(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_implicit(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_call(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_entry(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_decl(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);

unsigned parse_stmt_common(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_namelist(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_dimension(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_virtual(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_equivalence(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);

unsigned parse_stmt_assignment(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_continue(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_stop(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_pause(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_return(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_go_to(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_if(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_do(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_format(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_data(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_save(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_parameter(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_assign(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_pointer(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);

unsigned parse_stmt_decl_attr_external(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_decl_attr_intrinsic(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_decl_attr_automatic(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_decl_attr_static(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_decl_attr_volatile(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);

unsigned parse_stmt_structure(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_union(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_map(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_record(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);

unsigned parse_stmt_io_open(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_io_inquire(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_io_rewind(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_io_backspace(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_io_read(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_io_write(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_io_end_file(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_io_close(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_io_print_type(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_io_encode(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_io_decode(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);
unsigned parse_stmt_io_accept(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt);



static void parse_stmt__cleanup(
	parse_stmt_t stmt)
{
	switch (stmt.type)
	{
		case PARSE_STMT_INCLUDE:
			parse_stmt_list_delete(stmt.include.include);
			sparse_delete(stmt.include.src);
			file_delete(stmt.include.file);
			break;
		case PARSE_STMT_PROGRAM:
		case PARSE_STMT_SUBROUTINE:
		case PARSE_STMT_FUNCTION:
		case PARSE_STMT_BLOCK_DATA:
			parse_stmt_list_delete(stmt.program.body);
			parse_call_arg_list_delete(stmt.program.args);
			parse_type_delete(stmt.program.type);
			break;
		case PARSE_STMT_ASSIGNMENT:
			parse_assign_delete(stmt.assignment);
			break;
		case PARSE_STMT_IMPLICIT:
			parse_implicit_list_delete(stmt.implicit);
			break;
		case PARSE_STMT_CALL:
		case PARSE_STMT_ENTRY:
			parse_call_arg_list_delete(stmt.call_entry.args);
			break;
		case PARSE_STMT_DECL:
			parse_type_delete(stmt.decl.type);
			parse_decl_list_delete(stmt.decl.decl);
			break;
		case PARSE_STMT_COMMON:
		case PARSE_STMT_NAMELIST:
			parse_common_group_list_delete(stmt.common_namelist);
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
		case PARSE_STMT_DECL_ATTR_EXTERNAL:
		case PARSE_STMT_DECL_ATTR_INTRINSIC:
		case PARSE_STMT_DECL_ATTR_AUTOMATIC:
		case PARSE_STMT_DECL_ATTR_STATIC:
		case PARSE_STMT_DECL_ATTR_VOLATILE:
			parse_list_delete(
				stmt.decl_attr.count,
				(void**)stmt.decl_attr.name,
				free);
			break;
		case PARSE_STMT_POINTER:
			parse_pointer_list_delete(
				stmt.pointer);
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
		case PARSE_STMT_DO_LABEL:
			parse_assign_delete(stmt.do_label.init);
			parse_expr_delete(stmt.do_label.last);
			parse_expr_delete(stmt.do_label.step);
			break;
		case PARSE_STMT_DO_BLOCK:
			parse_assign_delete(stmt.do_block.init);
			parse_expr_delete(stmt.do_block.last);
			parse_expr_delete(stmt.do_block.step);
			parse_stmt_list_delete(stmt.do_block.block);
			break;
		case PARSE_STMT_DO_WHILE:
			parse_expr_delete(stmt.do_while.cond);
			break;
		case PARSE_STMT_DO_WHILE_BLOCK:
			parse_expr_delete(stmt.do_while_block.cond);
			parse_stmt_list_delete(stmt.do_while_block.block);
			break;
		case PARSE_STMT_STRUCTURE:
		case PARSE_STMT_UNION:
		case PARSE_STMT_MAP:
			parse_stmt_list_delete(stmt.structure.block);
			break;
		case PARSE_STMT_RECORD:
			parse_record_list_delete(stmt.record);
			break;
		case PARSE_STMT_IO_REWIND:
		case PARSE_STMT_IO_BACKSPACE:
		case PARSE_STMT_IO_READ:
		case PARSE_STMT_IO_WRITE:
		case PARSE_STMT_IO_END_FILE:
		case PARSE_STMT_IO_CLOSE:
		case PARSE_STMT_IO_OPEN:
		case PARSE_STMT_IO_INQUIRE:
		case PARSE_STMT_IO_ENCODE:
		case PARSE_STMT_IO_DECODE:
			parse_call_arg_list_delete(stmt.io.params);
			parse_iolist_delete(stmt.io.iolist);
			break;
		case PARSE_STMT_IO_PRINT:
		case PARSE_STMT_IO_ACCEPT:
			parse_iolist_delete(stmt.io_print.iolist);
			break;
		case PARSE_STMT_FORMAT:
			parse_format_desc_list_delete(stmt.format);
			break;
		case PARSE_STMT_DATA:
			parse_data_list_delete(stmt.data);
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
	parse_debug_t* debug,
	unsigned* len)
{
	if (ptr[0] == '\0')
		return NULL;

	parse_stmt_t stmt;
	stmt.type  = PARSE_STMT_EMPTY;

	/* TODO - Allow handling of label 0? */
	stmt.label = 0;
	sparse_label_find(src, ptr, &stmt.label);

	unsigned dpos = parse_debug_position(debug);

	unsigned i = 0;

	if (i == 0) i = parse_stmt_function(src, ptr, debug, &stmt);
	if (i == 0) i = parse_stmt_decl(src, ptr, debug, &stmt);

	/* Drop incomplete statements. */
	if ((i > 0)
		&& !is_end_statement(&ptr[i], NULL))
	{
		parse_stmt__cleanup(stmt);
		i = 0;
		parse_debug_rewind(debug, dpos);
	}

	switch (toupper(ptr[0]))
	{
		case 'A':
			if (i == 0) i = parse_stmt_assign(src, ptr, debug, &stmt);
			if (i == 0) i = parse_stmt_decl_attr_automatic(src, ptr, debug, &stmt);
			if (i == 0) i = parse_stmt_io_accept(src, ptr, debug, &stmt);
			break;

		case 'B':
			if (i == 0) i = parse_stmt_io_backspace(src, ptr, debug, &stmt);
			if (i == 0) i = parse_stmt_block_data(src, ptr, debug, &stmt);
			break;

		case 'C':
			if (i == 0) i = parse_stmt_continue(src, ptr, debug, &stmt);
			if (i == 0) i = parse_stmt_call(src, ptr, debug, &stmt);
			if (i == 0) i = parse_stmt_common(src, ptr, debug, &stmt);
			if (i == 0) i = parse_stmt_io_close(src, ptr, debug, &stmt);
			break;

		case 'D':
			if (i == 0) i = parse_stmt_do(src, ptr, debug, &stmt);
			if (i == 0) i = parse_stmt_data(src, ptr, debug, &stmt);
			if (i == 0) i = parse_stmt_dimension(src, ptr, debug, &stmt);
			if (i == 0) i = parse_stmt_io_decode(src, ptr, debug, &stmt);
			break;

		case 'E':
			if (i == 0) i = parse_stmt_equivalence(src, ptr, debug, &stmt);
			if (i == 0) i = parse_stmt_io_end_file(src, ptr, debug, &stmt);
			if (i == 0) i = parse_stmt_decl_attr_external(src, ptr, debug, &stmt);
			if (i == 0) i = parse_stmt_entry(src, ptr, debug, &stmt);
			if (i == 0) i = parse_stmt_io_encode(src, ptr, debug, &stmt);
			break;

		case 'F':
			if (i == 0) i = parse_stmt_format(src, ptr, debug, &stmt);
			break;

		case 'G':
			if (i == 0) i = parse_stmt_go_to(src, ptr, debug, &stmt);
			break;

		case 'I':
			if (i == 0) i = parse_stmt_implicit(src, ptr, debug, &stmt);
			if (i == 0) i = parse_stmt_if(src, ptr, debug, &stmt);
			if (i == 0) i = parse_stmt_decl_attr_intrinsic(src, ptr, debug, &stmt);
			if (i == 0) i = parse_stmt_io_inquire(src, ptr, debug, &stmt);
			if (i == 0) i = parse_stmt_include(src, ptr, debug, &stmt);
			break;

		case 'M':
			if (i == 0) i = parse_stmt_map(src, ptr, debug, &stmt);
			break;

		case 'N':
			if (i == 0) i = parse_stmt_namelist(src, ptr, debug, &stmt);
			break;

		case 'O':
			if (i == 0) i = parse_stmt_io_open(src, ptr, debug, &stmt);
			break;

		case 'P':
			if (i == 0) i = parse_stmt_parameter(src, ptr, debug, &stmt);
			if (i == 0) i = parse_stmt_program(src, ptr, debug, &stmt);
			if (i == 0) i = parse_stmt_pause(src, ptr, debug, &stmt);
			if (i == 0) i = parse_stmt_io_print_type(src, ptr, debug, &stmt);
			if (i == 0) i = parse_stmt_pointer(src, ptr, debug, &stmt);
			break;

		case 'R':
			if (i == 0) i = parse_stmt_return(src, ptr, debug, &stmt);
			if (i == 0) i = parse_stmt_io_read(src, ptr, debug, &stmt);
			if (i == 0) i = parse_stmt_io_rewind(src, ptr, debug, &stmt);
			if (i == 0) i = parse_stmt_record(src, ptr, debug, &stmt);
			break;

		case 'S':
			if (i == 0) i = parse_stmt_subroutine(src, ptr, debug, &stmt);
			if (i == 0) i = parse_stmt_stop(src, ptr, debug, &stmt);
			if (i == 0) i = parse_stmt_save(src, ptr, debug, &stmt);
			if (i == 0) i = parse_stmt_decl_attr_static(src, ptr, debug, &stmt);
			if (i == 0) i = parse_stmt_structure(src, ptr, debug, &stmt);
			break;

		case 'T':
			if (i == 0) i = parse_stmt_io_print_type(src, ptr, debug, &stmt);
			break;

		case 'U':
			if (i == 0) i = parse_stmt_union(src, ptr, debug, &stmt);
			break;

		case 'V':
			if (i == 0) i = parse_stmt_virtual(src, ptr, debug, &stmt);
			if (i == 0) i = parse_stmt_decl_attr_volatile(src, ptr, debug, &stmt);
			break;

		case 'W':
			if (i == 0) i = parse_stmt_io_write(src, ptr, debug, &stmt);
			break;

		default:
			break;
	}

	/* Drop incomplete statements, they may be an assignment. */
	if ((i > 0)
		&& !is_end_statement(&ptr[i], NULL))
	{
		parse_stmt__cleanup(stmt);
		i = 0;
		parse_debug_rewind(debug, dpos);
	}

	if (i == 0)
	{
		stmt.assignment = parse_assign(src, ptr, debug, &i);
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
		{
			parse_debug_rewind(debug, dpos);
			return NULL;
		}

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



bool parse_stmt_include_print(
	string_t* tree_output, const parse_stmt_t* stmt);
bool parse_stmt_program_print(
	string_t* tree_output, const parse_stmt_t* stmt, unsigned indent);
bool parse_stmt_assign_print(
	string_t* tree_output, const parse_stmt_t* stmt);
bool parse_stmt_decl_print(
	string_t* tree_output, const parse_stmt_t* stmt);
bool parse_stmt_if_print(
	string_t* tree_output, const parse_stmt_t* stmt, unsigned indent);
bool parse_stmt_stop_pause_return_print(
	string_t* tree_output, const parse_stmt_t* stmt);
bool parse_stmt_format_print(
	string_t* tree_output, const parse_stmt_t* stmt);
bool parse_stmt_data_print(
	string_t* tree_output, const parse_stmt_t* stmt);
bool parse_stmt_common_namelist_print(
	string_t* tree_output, const parse_stmt_t* stmt);
bool parse_stmt_implicit_print(
	string_t* tree_output, const parse_stmt_t* stmt);
bool parse_stmt_save_print(
	string_t* tree_output, const parse_stmt_t* stmt);
bool parse_stmt_parameter_print(
	string_t* tree_output, const parse_stmt_t* stmt);
bool parse_stmt_continue_print(
	string_t* tree_output, const parse_stmt_t* stmt);
bool parse_stmt_dimension_print(
	string_t* tree_output, const parse_stmt_t* stmt);
bool parse_stmt_call_entry_print(
	string_t* tree_output, const parse_stmt_t* stmt);
bool parse_stmt_equivalence_print(
	string_t* tree_output, const parse_stmt_t* stmt);
bool parse_stmt_do_print(
	string_t* tree_output, const parse_stmt_t* stmt, unsigned indent);
bool parse_stmt_decl_attr_print(
	string_t* tree_output, const parse_stmt_t* stmt);
bool parse_stmt_pointer_print(
	string_t* tree_output, const parse_stmt_t* stmt);
bool parse_stmt_go_to_print(
	string_t* tree_output, const parse_stmt_t* stmt);
bool parse_stmt_structure_print(
	string_t* tree_output, const parse_stmt_t* stmt, unsigned indent);
bool parse_stmt_record_print(
	string_t* tree_output, const parse_stmt_t* stmt);
bool parse_stmt_io_print(
	string_t* tree_output, const parse_stmt_t* stmt);
bool parse_stmt_print_accept_print(
	string_t* tree_output, const parse_stmt_t* stmt);


bool parse_stmt_print(
	string_t* tree_output,
	const parse_stmt_t* stmt,
	unsigned indent)
{
	if (stmt->type == PARSE_STMT_EMPTY)
		return true;

	switch(stmt->type)
	{
		case PARSE_STMT_INCLUDE:
			if (!parse_stmt_include_print(tree_output, stmt))
				return false;
			break;

		case PARSE_STMT_PROGRAM:
		case PARSE_STMT_SUBROUTINE:
		case PARSE_STMT_FUNCTION:
		case PARSE_STMT_BLOCK_DATA:
			if (!parse_stmt_program_print(
				tree_output, stmt, indent))
				return false;
			break;
		case PARSE_STMT_IMPLICIT_NONE:
		case PARSE_STMT_IMPLICIT:
			if (!parse_stmt_implicit_print(tree_output, stmt))
				return false;
			break;
		case PARSE_STMT_CALL:
		case PARSE_STMT_ENTRY:
			if (!parse_stmt_call_entry_print(tree_output, stmt))
				return false;
			break;
		case PARSE_STMT_DECL:
			if (!parse_stmt_decl_print(tree_output, stmt))
				return false;
			break;
		case PARSE_STMT_DIMENSION:
			if (!parse_stmt_dimension_print(tree_output, stmt))
				return false;
			break;
		case PARSE_STMT_EQUIVALENCE:
			if (!parse_stmt_equivalence_print(tree_output, stmt))
				return false;
			break;
		case PARSE_STMT_COMMON:
		case PARSE_STMT_NAMELIST:
			if (!parse_stmt_common_namelist_print(tree_output, stmt))
				return false;
			break;
		case PARSE_STMT_ASSIGNMENT:
			if (!parse_assign_print(
				tree_output, stmt->assignment))
				return false;
			break;
		case PARSE_STMT_CONTINUE:
			if (!parse_stmt_continue_print(tree_output, stmt))
				return false;
			break;
		case PARSE_STMT_STOP:
		case PARSE_STMT_PAUSE:
		case PARSE_STMT_RETURN:
			if (!parse_stmt_stop_pause_return_print(tree_output, stmt))
				return false;
			break;
		case PARSE_STMT_DECL_ATTR_EXTERNAL:
		case PARSE_STMT_DECL_ATTR_INTRINSIC:
		case PARSE_STMT_DECL_ATTR_AUTOMATIC:
		case PARSE_STMT_DECL_ATTR_STATIC:
		case PARSE_STMT_DECL_ATTR_VOLATILE:
			if (!parse_stmt_decl_attr_print(tree_output, stmt))
				return false;
			break;
		case PARSE_STMT_POINTER:
			if (!parse_stmt_pointer_print(tree_output, stmt))
				return false;
			break;
		case PARSE_STMT_GO_TO:
		case PARSE_STMT_GO_TO_ASSIGNED:
		case PARSE_STMT_GO_TO_COMPUTED:
			if (!parse_stmt_go_to_print(tree_output, stmt))
				return false;
			break;
		case PARSE_STMT_IF_COMPUTED:
		case PARSE_STMT_IF_STATEMENT:
		case PARSE_STMT_IF_THEN:
			if (!parse_stmt_if_print(tree_output, stmt, indent))
				return false;
			break;
		case PARSE_STMT_DO_LABEL:
		case PARSE_STMT_DO_BLOCK:
		case PARSE_STMT_DO_WHILE:
		case PARSE_STMT_DO_WHILE_BLOCK:
			if (!parse_stmt_do_print(tree_output, stmt, indent))
				return false;
			break;
		case PARSE_STMT_STRUCTURE:
		case PARSE_STMT_UNION:
		case PARSE_STMT_MAP:
			if (!parse_stmt_structure_print(tree_output, stmt, indent))
				return false;
			break;
		case PARSE_STMT_RECORD:
			if (!parse_stmt_record_print(tree_output, stmt))
				return false;
			break;
		case PARSE_STMT_IO_OPEN:
		case PARSE_STMT_IO_INQUIRE:
		case PARSE_STMT_IO_REWIND:
		case PARSE_STMT_IO_BACKSPACE:
		case PARSE_STMT_IO_READ:
		case PARSE_STMT_IO_WRITE:
		case PARSE_STMT_IO_END_FILE:
		case PARSE_STMT_IO_CLOSE:
			parse_stmt_io_print(tree_output, stmt);
			break;
		case PARSE_STMT_IO_PRINT:
		case PARSE_STMT_IO_ACCEPT:
			parse_stmt_print_accept_print(tree_output, stmt);
			break;
		case PARSE_STMT_FORMAT:
			if (!parse_stmt_format_print(tree_output, stmt))
				return false;
			break;
		case PARSE_STMT_DATA:
			if (!parse_stmt_data_print(tree_output, stmt))
				return false;
			break;
		case PARSE_STMT_SAVE:
			if (!parse_stmt_save_print(tree_output, stmt))
				return false;
			break;
		case PARSE_STMT_PARAMETER:
			if (!parse_stmt_parameter_print(tree_output, stmt))
				return false;
			break;
		case PARSE_STMT_ASSIGN:
			if (!parse_stmt_assign_print(tree_output, stmt))
				return false;
			break;
		default:
			return false;
	}

	return true;
}



parse_stmt_list_t* parse_stmt_list(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len)
{
	parse_stmt_list_t* list
		= (parse_stmt_list_t*)malloc(
			sizeof(parse_stmt_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->stmt = NULL;

	unsigned i = parse_list(
		src, ptr, debug, '\0',
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



bool parse_stmt_list_print(
	string_t* tree_output,
	const parse_stmt_list_t* list,
	unsigned indent)
{
	if (!list)
		return false;

	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		if (!(list->stmt[i]->label > 0
			? string_printf(tree_output, "%5u ", list->stmt[i]->label)
			: string_printf(tree_output, "      ")))
			return false;

		unsigned j;
		for (j = 0; j < indent; j++)
		{
			if (!string_printf(tree_output, "  "))
				return false;
		}

		if (!parse_stmt_print(
			tree_output, list->stmt[i], indent))
			return false;

		if (!string_printf(tree_output, "\n"))
			return false;
	}

	return true;
}
