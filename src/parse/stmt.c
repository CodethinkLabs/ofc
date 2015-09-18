#include "parse.h"

unsigned parse_stmt_implicit(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_decl(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_dimension(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_assignment(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_continue(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_stop_pause(
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
unsigned parse_stmt_write(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_rewind(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_format(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_data(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_assign(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);



static void parse_stmt__cleanup(
	parse_stmt_t stmt)
{
	unsigned i;
	switch (stmt.type)
	{
		case PARSE_STMT_ASSIGNMENT:
			parse_lhs_cleanup(stmt.assignment.lhs);
			parse_expr_delete(stmt.assignment.rhs);
			break;
		case PARSE_STMT_IMPLICIT:
			free(stmt.implicit);
			break;
		case PARSE_STMT_DECL:
			parse_type_delete(stmt.decl.type);
			for (i = 0; i < stmt.decl.count; i++)
			{
				parse_expr_delete(stmt.decl.entry[i].dimension);
				parse_expr_delete(stmt.decl.entry[i].init);
			}
			free(stmt.decl.entry);
			break;
		case PARSE_STMT_DIMENSION:
			free(stmt.dimension.name);
			for (i = 0; i < stmt.dimension.count; i++)
				parse_expr_delete(stmt.dimension.dimension[i]);
			free(stmt.dimension.dimension);
			break;
		case PARSE_STMT_STOP:
		case PARSE_STMT_PAUSE:
			parse_expr_delete(stmt.stop_pause.code);
			break;
		case PARSE_STMT_GO_TO_ASSIGNED:
		case PARSE_STMT_GO_TO_COMPUTED:
			free(stmt.go_to_comp.label);
			parse_expr_delete(stmt.go_to_comp.cond);
			break;
		case PARSE_STMT_IF_COMPUTED:
			free(stmt.if_comp.label);
			parse_expr_delete(stmt.if_comp.cond);
			break;
		case PARSE_STMT_IF_STATEMENT:
			parse_stmt_delete(stmt.if_stmt.stmt);
			parse_expr_delete(stmt.if_stmt.cond);
			break;
		case PARSE_STMT_DO:
			parse_lhs_cleanup(stmt.do_loop.iterator);
			parse_expr_delete(stmt.do_loop.init);
			parse_expr_delete(stmt.do_loop.last);
			parse_expr_delete(stmt.do_loop.step);
			break;
		case PARSE_STMT_WRITE:
			parse_expr_list_delete(stmt.write.elem);
			parse_expr_delete(stmt.write.file);
			break;
		case PARSE_STMT_FORMAT:
			parse_format_desc_list_delete(
				stmt.format.desc, stmt.format.desc_count);
			break;
		case PARSE_STMT_REWIND:
			parse_expr_delete(stmt.rewind.unit);
			parse_expr_delete(stmt.rewind.iostat);
			parse_expr_delete(stmt.rewind.err);
			break;
		case PARSE_STMT_DATA:
			for (i = 0; i < stmt.data.init_count; i++)
				parse_expr_delete(stmt.data.init[i]);
			free(stmt.data.init);
			for (i = 0; i < stmt.data.name_count; i++)
				parse_lhs_cleanup(stmt.data.name[i]);
			free(stmt.data.name);
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
	const unsigned* label,
	unsigned* len)
{
	parse_stmt_t stmt;
	stmt.type  = PARSE_STMT_EMPTY;
	stmt.label = label;

	unsigned i = 0;
	switch (toupper(ptr[0]))
	{
		case 'A':
			if (i == 0) i = parse_stmt_assign(src, ptr, &stmt);
			break;

		case 'C':
			if (i == 0) i = parse_stmt_continue(src, ptr, &stmt);
			break;

		case 'D':
			if (i == 0) i = parse_stmt_do(src, ptr, &stmt);
			if (i == 0) i = parse_stmt_data(src, ptr, &stmt);
			if (i == 0) i = parse_stmt_dimension(src, ptr, &stmt);
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
			break;

		case 'P':
			if (i == 0) i = parse_stmt_stop_pause(src, ptr, &stmt);
			break;

		case 'R':
			if (i == 0) i = parse_stmt_rewind(src, ptr, &stmt);
			break;

		case 'S':
			if (i == 0) i = parse_stmt_stop_pause(src, ptr, &stmt);
			break;

		case 'W':
			if (i == 0) i = parse_stmt_write(src, ptr, &stmt);
			break;

		default:
			break;
	}

	/* Drop incomplete statements, they may be an assignment or declaration. */
	if ((i > 0)
		&& !is_end_statement(ptr[i], NULL))
	{
		parse_stmt__cleanup(stmt);
		i = 0;
	}

	if (i == 0) i = parse_stmt_decl(src, ptr, &stmt);
	if (i == 0) i = parse_stmt_assignment(src, ptr, &stmt);

	unsigned l = 0;
	if (!is_end_statement(ptr[i], &l))
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
