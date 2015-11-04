#include <ofc/sema.h>


static ofc_sema_stmt_t* ofc_sema_stmt_simple(
	ofc_parse_stmt_e type)
{
	ofc_sema_stmt_e st;
	switch (type)
	{
		case OFC_PARSE_STMT_CONTINUE:
			st = OFC_SEMA_STMT_CONTINUE;
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

		case OFC_PARSE_STMT_IO_WRITE:
			s = ofc_sema_stmt_io_write(
				scope, stmt);
			break;

		case OFC_PARSE_STMT_IF_THEN:
		case OFC_PARSE_STMT_IF_STATEMENT:
		case OFC_PARSE_STMT_IF_COMPUTED:
			s = ofc_sema_stmt_if(scope, stmt);
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

		default:
			ofc_sema_scope_error(scope, stmt->src,
				"Unsuported statement");
			break;
	}

	return s;
}

bool ofc_sema_stmt_scoped(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	if (stmt->label != 0)
	{
		if (ofc_sema_label_map_find(
			scope->label, stmt->label))
		{
			ofc_sema_scope_error(scope, stmt->src,
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
		ofc_sema_scope_warning(scope, stmt->src,
			"Unlabelled CONTINUE statement has no effect");
		ofc_sema_stmt_delete(s);
		return true;
	}

	unsigned offset
		= ofc_sema_stmt_list_count(
			scope->stmt);

	if (!ofc_sema_stmt_list_add(
		scope->stmt, s))
	{
		ofc_sema_stmt_delete(s);
		return false;
	}

	if (stmt->label != 0)
	{
		if (!ofc_sema_label_map_add_stmt(scope, stmt,
			scope->label, stmt->label, offset))
		{
			/* This should never happen. */
			abort();
		}
	}

	return true;
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
		case OFC_SEMA_STMT_WRITE:
			ofc_sema_expr_delete(
				stmt->io_write.unit);
			ofc_sema_expr_delete(
				stmt->io_write.format_expr);
			ofc_sema_format_delete(
				stmt->io_write.format);
			ofc_sema_expr_delete(
				stmt->io_write.iostat);
			ofc_sema_expr_delete(
				stmt->io_write.rec);
			ofc_sema_expr_delete(
				stmt->io_write.err);
			break;
		case OFC_SEMA_STMT_IF_COMPUTED:
			ofc_sema_expr_delete(
				stmt->if_comp.cond);
			ofc_sema_expr_list_delete(
				stmt->if_comp.label);
			break;
		case OFC_SEMA_STMT_IF_THEN:
			ofc_sema_expr_delete(
				stmt->if_then.cond);
			ofc_sema_scope_delete(
				stmt->if_then.scope_then);
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
			ofc_sema_expr_delete(
				stmt->do_label.init);
			ofc_sema_expr_delete(
				stmt->do_label.last);
			ofc_sema_expr_delete(
				stmt->do_label.step);
			break;
		case OFC_SEMA_STMT_DO_BLOCK:
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

unsigned ofc_sema_stmt_list_count(
	const ofc_sema_stmt_list_t* list)
{
	return (list ? list->count : 0);
}
