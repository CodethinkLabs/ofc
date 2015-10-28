#include <ofc/parse.h>

static unsigned ofc_parse_stmt_go_to_unconditional(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned i = 0;

	unsigned len = ofc_parse_label(
		src, &ptr[i], debug, &stmt->go_to.label);
	if (len == 0)
		return 0;

	stmt->type = OFC_PARSE_STMT_GO_TO;
	return (i + len);
}

static unsigned ofc_parse_stmt_go_to_assigned(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = 0;

	unsigned len;
	stmt->go_to_assign.cond
		= ofc_parse_expr_integer_variable(
			src, &ptr[i], debug, &len);
	if (!stmt->go_to_assign.cond)
		return 0;
	i += len;

	if (ptr[i] == ',')
		i += 1;

	if (ptr[i++] != '(')
	{
		ofc_parse_expr_delete(
			stmt->go_to_assign.cond);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}

	stmt->go_to_assign.label
		= ofc_parse_expr_list(
			src, &ptr[i], debug, &len);
	if (!stmt->go_to_assign.label)
	{
		ofc_parse_expr_delete(
			stmt->go_to_assign.cond);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	stmt->type = OFC_PARSE_STMT_GO_TO_ASSIGNED;

	if (ptr[i++] != ')')
	{
		ofc_parse_expr_list_delete(
			stmt->go_to_assign.label);
		ofc_parse_expr_delete(
			stmt->go_to_assign.cond);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}

	return i;
}

static unsigned ofc_parse_stmt_go_to_computed(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned i = 0;

	if (ptr[i++] != '(')
		return 0;

	unsigned dpos = ofc_parse_debug_position(debug);

	ofc_parse_label_t label;
	unsigned len = ofc_parse_label(
		src, &ptr[i], debug, &label);
	if (len == 0)
		return 0;
	i += len;

	stmt->type = OFC_PARSE_STMT_GO_TO_COMPUTED;

	stmt->go_to_comp.label_count = 1;
	stmt->go_to_comp.label = (ofc_parse_label_t*)malloc(
		sizeof(ofc_parse_label_t) * stmt->go_to_comp.label_count);
	if (!stmt->go_to_comp.label)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	stmt->go_to_comp.label[0] = label;

	while (ptr[i] == ',')
	{
		unsigned j = (i + 1);
		len = ofc_parse_label(
			src, &ptr[j], debug, &label);
		if (len == 0) break;

		ofc_parse_label_t* nlabel = (ofc_parse_label_t*)realloc(stmt->go_to_comp.label,
			sizeof(ofc_parse_label_t) * (stmt->go_to_comp.label_count + 1));
		if (!nlabel)
		{
			free(stmt->go_to_comp.label);
			ofc_parse_debug_rewind(debug, dpos);
			return 0;
		}
		stmt->go_to_comp.label = nlabel;
		stmt->go_to_comp.label[stmt->go_to_comp.label_count++] = label;
		i = (j + len);
	}

	if (ptr[i++] != ')')
	{
		free(stmt->go_to_comp.label);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}

	if (ptr[i] == ',')
		i += 1;

	stmt->go_to_comp.cond = ofc_parse_expr(
		src, &ptr[i], debug, &len);
	if (!stmt->go_to_comp.cond)
	{
		free(stmt->go_to_comp.label);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	return i;
}

unsigned ofc_parse_stmt_go_to(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = ofc_parse_keyword(
		src, ptr, debug, OFC_PARSE_KEYWORD_GO_TO);
	if (i == 0) return 0;

	unsigned len = 0;
	if (len == 0) len = ofc_parse_stmt_go_to_assigned(src, &ptr[i], debug, stmt);
	if (len == 0) len = ofc_parse_stmt_go_to_computed(src, &ptr[i], debug, stmt);
	if (len == 0) len = ofc_parse_stmt_go_to_unconditional(src, &ptr[i], debug, stmt);

	if (len == 0)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	return i;
}



static bool ofc_parse_stmt_go_to_assigned_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
    return (ofc_colstr_atomic_writef(cs, "GO TO ")
		&& ofc_parse_expr_print(cs, stmt->go_to_assign.cond)
		&& ofc_colstr_atomic_writef(cs, ", (")
		&& ofc_parse_expr_list_print(cs, stmt->go_to_assign.label)
		&& ofc_colstr_atomic_writef(cs, ")"));
}

static bool ofc_parse_stmt_go_to_computed_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
    if (!ofc_colstr_atomic_writef(cs, "GO TO ("))
		return false;

	unsigned i;
	for (i = 0; i < stmt->go_to_comp.label_count; i++)
	{
		if ((i > 0) && !ofc_colstr_atomic_writef(cs, ", "))
			return false;

		if (!ofc_parse_label_print(cs,
			stmt->go_to_comp.label[i]))
			return false;
	}

	return (ofc_colstr_atomic_writef(cs, "), ")
		&& ofc_parse_expr_print(cs,
			stmt->go_to_comp.cond));
}

static bool ofc_parse_stmt_go_to_unconditional_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
	return (ofc_colstr_atomic_writef(cs, "GO TO ")
		&& ofc_parse_label_print(cs, stmt->go_to.label));
}

bool ofc_parse_stmt_go_to_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	switch (stmt->type)
	{
		case OFC_PARSE_STMT_GO_TO_ASSIGNED:
			return ofc_parse_stmt_go_to_assigned_print(cs, stmt);
		case OFC_PARSE_STMT_GO_TO_COMPUTED:
			return ofc_parse_stmt_go_to_computed_print(cs, stmt);
		case OFC_PARSE_STMT_GO_TO:
			return ofc_parse_stmt_go_to_unconditional_print(cs, stmt);
		default:
			break;
	}

	return false;
}
