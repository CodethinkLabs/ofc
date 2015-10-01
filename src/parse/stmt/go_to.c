#include "../parse.h"

static unsigned parse_stmt_go_to_unconditional(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned i = 0;

	unsigned len = parse_label(
		src, &ptr[i], debug, &stmt->go_to.label);
	if (len == 0)
		return 0;

	stmt->type = PARSE_STMT_GO_TO;
	return (i + len);
}

static unsigned parse_stmt_go_to_assigned(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned dpos = parse_debug_position(debug);

	unsigned i = 0;

	unsigned len = parse_label(
		src, &ptr[i], debug,
		&stmt->go_to_assign.cond);
	if (len == 0) return 0;
	i += len;

	if (ptr[i] == ',')
		i += 1;

	if (ptr[i++] != '(')
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}

	parse_label_t label;
	len = parse_label(
		src, &ptr[i], debug, &label);
	if (len == 0)
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	stmt->type = PARSE_STMT_GO_TO_ASSIGNED;

	stmt->go_to_assign.label_count = 1;
	stmt->go_to_assign.label = (parse_label_t*)malloc(
		sizeof(parse_label_t) * stmt->go_to_assign.label_count);
	if (!stmt->go_to_assign.label)
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	stmt->go_to_assign.label[0] = label;

	while (ptr[i] == ',')
	{
		unsigned j = (i + 1);
		len = parse_label(
			src, &ptr[j], debug, &label);
		if (len == 0) break;

		parse_label_t* nlabel = (parse_label_t*)realloc(stmt->go_to_assign.label,
			sizeof(parse_label_t) * (stmt->go_to_assign.label_count + 1));
		if (!nlabel)
		{
			free(stmt->go_to_assign.label);
			parse_debug_rewind(debug, dpos);
			return 0;
		}
		stmt->go_to_assign.label = nlabel;
		stmt->go_to_assign.label[stmt->go_to_assign.label_count++] = label;
		i = (j + len);
	}

	if (ptr[i++] != ')')
	{
		free(stmt->go_to_assign.label);
		parse_debug_rewind(debug, dpos);
		return 0;
	}

	return i;
}

static unsigned parse_stmt_go_to_computed(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned i = 0;

	if (ptr[i++] != '(')
		return 0;

	unsigned dpos = parse_debug_position(debug);

	parse_label_t label;
	unsigned len = parse_label(
		src, &ptr[i], debug, &label);
	if (len == 0)
		return 0;
	i += len;

	stmt->type = PARSE_STMT_GO_TO_COMPUTED;

	stmt->go_to_comp.label_count = 1;
	stmt->go_to_comp.label = (parse_label_t*)malloc(
		sizeof(parse_label_t) * stmt->go_to_comp.label_count);
	if (!stmt->go_to_comp.label)
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	stmt->go_to_comp.label[0] = label;

	while (ptr[i] == ',')
	{
		unsigned j = (i + 1);
		len = parse_label(
			src, &ptr[j], debug, &label);
		if (len == 0) break;

		parse_label_t* nlabel = (parse_label_t*)realloc(stmt->go_to_comp.label,
			sizeof(parse_label_t) * (stmt->go_to_comp.label_count + 1));
		if (!nlabel)
		{
			free(stmt->go_to_comp.label);
			parse_debug_rewind(debug, dpos);
			return 0;
		}
		stmt->go_to_comp.label = nlabel;
		stmt->go_to_comp.label[stmt->go_to_comp.label_count++] = label;
		i = (j + len);
	}

	if (ptr[i++] != ')')
	{
		free(stmt->go_to_comp.label);
		parse_debug_rewind(debug, dpos);
		return 0;
	}

	if (ptr[i] == ',')
		i += 1;

	stmt->go_to_comp.cond = parse_expr(
		src, &ptr[i], debug, &len);
	if (!stmt->go_to_comp.cond)
	{
		free(stmt->go_to_comp.label);
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	return i;
}

unsigned parse_stmt_go_to(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned dpos = parse_debug_position(debug);

	unsigned i = parse_keyword(
		src, ptr, debug, PARSE_KEYWORD_GO_TO);
	if (i == 0) return 0;

	unsigned len = 0;
	if (len == 0) len = parse_stmt_go_to_assigned(src, &ptr[i], debug, stmt);
	if (len == 0) len = parse_stmt_go_to_computed(src, &ptr[i], debug, stmt);
	if (len == 0) len = parse_stmt_go_to_unconditional(src, &ptr[i], debug, stmt);

	if (len == 0)
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	return i;
}



static bool parse_stmt_go_to_assigned_print(
	string_t* tree_output, const parse_stmt_t* stmt)
{
    if (!string_printf(tree_output, "GO TO ")
		|| !parse_label_print(tree_output, stmt->go_to_assign.cond)
		|| !string_printf(tree_output, ", ("))
		return false;

	unsigned i;
	for (i = 0; i < stmt->go_to_assign.label_count; i++)
	{
		if ((i > 0) && !string_printf(tree_output, ", "))
			return false;

		if (!parse_label_print(tree_output,
			stmt->go_to_assign.label[i]))
			return false;
	}

	return string_printf(tree_output, ")");
}

static bool parse_stmt_go_to_computed_print(
	string_t* tree_output, const parse_stmt_t* stmt)
{
    if (!string_printf(tree_output, "GO TO ("))
		return false;

	unsigned i;
	for (i = 0; i < stmt->go_to_comp.label_count; i++)
	{
		if ((i > 0) && !string_printf(tree_output, ", "))
			return false;

		if (!parse_label_print(tree_output,
			stmt->go_to_comp.label[i]))
			return false;
	}

	return (string_printf(tree_output, "), ")
		&& parse_expr_print(tree_output,
			stmt->go_to_comp.cond));
}

static bool parse_stmt_go_to_unconditional_print(
	string_t* tree_output, const parse_stmt_t* stmt)
{
	return (string_printf(tree_output, "GO TO ")
		&& parse_label_print(tree_output, stmt->go_to.label));
}

bool parse_stmt_go_to_print(
	string_t* tree_output, const parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	switch (stmt->type)
	{
		case PARSE_STMT_GO_TO_ASSIGNED:
			return parse_stmt_go_to_assigned_print(tree_output, stmt);
		case PARSE_STMT_GO_TO_COMPUTED:
			return parse_stmt_go_to_computed_print(tree_output, stmt);
		case PARSE_STMT_GO_TO:
			return parse_stmt_go_to_unconditional_print(tree_output, stmt);
		default:
			break;
	}

	return false;
}
