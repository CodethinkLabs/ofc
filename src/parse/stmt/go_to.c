#include "../parse.h"

static unsigned parse_stmt_go_to_unconditional(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = 0;

	unsigned len = parse_label(
		src, &ptr[i], &stmt->go_to.label);
	if (len == 0)
		return 0;

	stmt->type = PARSE_STMT_GO_TO;
	return (i + len);
}

static unsigned parse_stmt_go_to_assigned(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = 0;

	unsigned len = parse_label(
		src, &ptr[i], &stmt->go_to_assign.cond);
	if (len == 0) return 0;
	i += len;

	if (ptr[i] == ',')
		i += 1;

	if (ptr[i++] != '(')
		return 0;

	parse_label_t label;
	len = parse_label(
		src, &ptr[i], &label);
	if (len == 0) return 0;
	i += len;

	stmt->type = PARSE_STMT_GO_TO_ASSIGNED;

	stmt->go_to_assign.label_count = 1;
	stmt->go_to_assign.label = (parse_label_t*)malloc(
		sizeof(parse_label_t) * stmt->go_to_assign.label_count);
	if (!stmt->go_to_assign.label)
		return 0;
	stmt->go_to_assign.label[0] = label;

	while (ptr[i] == ',')
	{
		unsigned j = (i + 1);
		len = parse_label(
			src, &ptr[j], &label);
		if (len == 0) break;

		parse_label_t* nlabel = (parse_label_t*)realloc(stmt->go_to_assign.label,
			sizeof(parse_label_t) * (stmt->go_to_assign.label_count + 1));
		if (!nlabel)
		{
			free(stmt->go_to_assign.label);
			return 0;
		}
		stmt->go_to_assign.label = nlabel;
		stmt->go_to_assign.label[stmt->go_to_assign.label_count++] = label;
		i = (j + len);
	}

	if (ptr[i++] != ')')
	{
		free(stmt->go_to_assign.label);
		return 0;
	}

	return i;
}

static unsigned parse_stmt_go_to_computed(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = 0;

	if (ptr[i++] != '(')
		return 0;

	parse_label_t label;
	unsigned len = parse_label(
		src, &ptr[i], &label);
	if (len == 0)
		return 0;
	i += len;

	stmt->type = PARSE_STMT_GO_TO_COMPUTED;

	stmt->go_to_comp.label_count = 1;
	stmt->go_to_comp.label = (parse_label_t*)malloc(
		sizeof(parse_label_t) * stmt->go_to_comp.label_count);
	if (!stmt->go_to_comp.label)
		return 0;
	stmt->go_to_comp.label[0] = label;

	while (ptr[i] == ',')
	{
		unsigned j = (i + 1);
		len = parse_label(
			src, &ptr[j], &label);
		if (len == 0) break;

		parse_label_t* nlabel = (parse_label_t*)realloc(stmt->go_to_comp.label,
			sizeof(parse_label_t) * (stmt->go_to_comp.label_count + 1));
		if (!nlabel)
		{
			free(stmt->go_to_comp.label);
			return 0;
		}
		stmt->go_to_comp.label = nlabel;
		stmt->go_to_comp.label[stmt->go_to_comp.label_count++] = label;
		i = (j + len);
	}

	if (ptr[i++] != ')')
	{
		free(stmt->go_to_comp.label);
		return 0;
	}

	if (ptr[i] == ',')
		i += 1;

	stmt->go_to_comp.cond = parse_expr(
		src, &ptr[i], &len);
	if (!stmt->go_to_comp.cond)
	{
		free(stmt->go_to_comp.label);
		return 0;
	}
	i += len;

	return i;
}

unsigned parse_stmt_go_to(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, PARSE_KEYWORD_GO_TO);
	if (i == 0) return 0;

	unsigned len = 0;
	if (len == 0) len = parse_stmt_go_to_assigned(src, &ptr[i], stmt);
	if (len == 0) len = parse_stmt_go_to_computed(src, &ptr[i], stmt);
	if (len == 0) len = parse_stmt_go_to_unconditional(src, &ptr[i], stmt);

	return (len ? (i + len) : 0);
}



static bool parse_stmt_go_to_assigned_print(
	int fd, const parse_stmt_t* stmt)
{
    if (!dprintf_bool(fd, "GO TO ")
		|| !parse_label_print(fd, stmt->go_to_assign.cond)
		|| !dprintf_bool(fd, ", ("))
		return false;

	unsigned i;
	for (i = 0; i < stmt->go_to_assign.label_count; i++)
	{
		if ((i > 0) && !dprintf_bool(fd, ", "))
			return false;

		if (!parse_label_print(fd,
			stmt->go_to_assign.label[i]))
			return false;
	}

	return dprintf_bool(fd, ")");
}

static bool parse_stmt_go_to_computed_print(
	int fd, const parse_stmt_t* stmt)
{
    if (!dprintf_bool(fd, "GO TO ("))
		return false;

	unsigned i;
	for (i = 0; i < stmt->go_to_comp.label_count; i++)
	{
		if ((i > 0) && !dprintf_bool(fd, ", "))
			return false;

		if (!parse_label_print(fd,
			stmt->go_to_comp.label[i]))
			return false;
	}

	return (dprintf_bool(fd, "), ")
		&& parse_expr_print(fd,
			stmt->go_to_comp.cond));
}

static bool parse_stmt_go_to_unconditional_print(
	int fd, const parse_stmt_t* stmt)
{
	return (dprintf_bool(fd, "GO TO ")
		&& parse_label_print(fd, stmt->go_to.label));
}

bool parse_stmt_go_to_print(
	int fd, const parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	switch (stmt->type)
	{
		case PARSE_STMT_GO_TO_ASSIGNED:
			return parse_stmt_go_to_assigned_print(fd, stmt);
		case PARSE_STMT_GO_TO_COMPUTED:
			return parse_stmt_go_to_computed_print(fd, stmt);
		case PARSE_STMT_GO_TO:
			return parse_stmt_go_to_unconditional_print(fd, stmt);
		default:
			break;
	}

	return false;
}
