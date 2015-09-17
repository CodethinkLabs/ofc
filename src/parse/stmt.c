#include "parse.h"


unsigned parse_stmt(
	const sparse_t* src, const char* ptr,
	const unsigned* label,
	const parse_implicit_t* implicit,
	hashmap_t* decl,
	parse_stmt_t* stmt)
{
	unsigned i = 0;
	stmt->type  = PARSE_STMT_EMPTY;
	stmt->label = label;

	if (i == 0) i = parse_stmt_continue(src, ptr, stmt);
	if (i == 0) i = parse_stmt_stop_pause(src, ptr, stmt);
	if (i == 0) i = parse_stmt_go_to(src, ptr, stmt);
	if (i == 0) i = parse_stmt_if(src, ptr, implicit, decl, stmt);
	if (i == 0) i = parse_stmt_do(src, ptr, stmt);
	if (i == 0) i = parse_stmt_data(src, ptr, stmt);
	if (i == 0) i = parse_stmt_write(src, ptr, stmt);
	if (i == 0) i = parse_stmt_format(src, ptr, stmt);
	if (i == 0) i = parse_stmt_assign(src, ptr, stmt);

	if ((i > 0)
		&& !is_end_statement(ptr[i], NULL))
	{
		parse_stmt_cleanup(*stmt);
		i = 0;
	}

	/* Assignments can clash. */
	if (i == 0)
	{
		i = parse_stmt_assignment(
			src, ptr, implicit, decl, stmt);
	}

	unsigned len = 0;
	if (!is_end_statement(ptr[i], &len))
	{
		if (i == 0)
			return 0;

		sparse_error(src, &ptr[i],
			"Expected newline or semicolon after statement");
		parse_stmt_cleanup(*stmt);
		return 0;
	}
	i += len;

	return i;
}

void parse_stmt_cleanup(
	parse_stmt_t stmt)
{
	unsigned i;
	switch (stmt.type)
	{
		case PARSE_STMT_ASSIGNMENT:
			parse_lhs_cleanup(stmt.assignment.lhs);
			parse_expr_cleanup(stmt.assignment.rhs);
			break;
		case PARSE_STMT_STOP:
		case PARSE_STMT_PAUSE:
			if (stmt.stop_pause.has_code)
				parse_expr_cleanup(stmt.stop_pause.code);
			break;
		case PARSE_STMT_GO_TO_ASSIGNED:
		case PARSE_STMT_GO_TO_COMPUTED:
			free(stmt.go_to_comp.label);
			parse_expr_cleanup(stmt.go_to_comp.cond);
			break;
		case PARSE_STMT_IF_COMPUTED:
			free(stmt.if_comp.label);
			parse_expr_cleanup(stmt.if_comp.cond);
			break;
		case PARSE_STMT_IF_STATEMENT:
			parse_stmt_delete(stmt.if_stmt.stmt);
			parse_expr_cleanup(stmt.if_stmt.cond);
			break;
		case PARSE_STMT_DO:
			parse_lhs_cleanup(stmt.do_loop.iterator);
			parse_expr_cleanup(stmt.do_loop.init);
			parse_expr_cleanup(stmt.do_loop.last);
			parse_expr_cleanup(stmt.do_loop.step);
			break;
		case PARSE_STMT_WRITE:
			for (i = 0; i < stmt.write.elem_count; i++)
				parse_expr_cleanup(stmt.write.elem[i]);
			free(stmt.write.elem);
			parse_expr_cleanup(stmt.write.file);
			break;
		case PARSE_STMT_FORMAT:
			parse_format_desc_list_delete(
				stmt.format.desc, stmt.format.desc_count);
			break;
		case PARSE_STMT_DATA:
			for (i = 0; i < stmt.data.init_count; i++)
				parse_expr_cleanup(stmt.data.init[i]);
			free(stmt.data.init);
			for (i = 0; i < stmt.data.name_count; i++)
				parse_lhs_cleanup(stmt.data.name[i]);
			free(stmt.data.name);
			break;
		default:
			break;
	}
}


parse_stmt_t* parse_stmt_alloc(
	parse_stmt_t stmt)
{
	parse_stmt_t* astmt
		= (parse_stmt_t*)malloc(
			sizeof(parse_stmt_t));
	if (!astmt) return NULL;

	*astmt = stmt;
	return astmt;
}

void parse_stmt_delete(
	parse_stmt_t* stmt)
{
	if (!stmt)
		return;

	parse_stmt_cleanup(*stmt);
	free(stmt);
}
