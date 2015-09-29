#include "parse.h"


parse_implicit_do_t* parse_implicit_do(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len)
{
	unsigned i = 0;

	if (ptr[i++] != '(')
		return 0;

	parse_implicit_do_t* id
		= (parse_implicit_do_t*)malloc(
			sizeof(parse_implicit_do_t));
	if (!id) return 0;

	id->dlist = NULL;
	id->init = NULL;
	id->limit = NULL;
	id->step = NULL;

	unsigned dpos = parse_debug_position(debug);

	unsigned l;
	id->dlist = parse_lhs(
		src, &ptr[i], debug, &l);
	if (!id->dlist)
	{
		free(id);
		return 0;
	}
	i += l;

	if (ptr[i++] != ',')
	{
		parse_implicit_do_delete(id);
		parse_debug_rewind(debug, dpos);
		return 0;
	}

	id->init = parse_assign_init(
		src, &ptr[i], debug, &l);
	if (!id->init)
	{
		parse_implicit_do_delete(id);
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += l;

	if (ptr[i++] != ',')
	{
		parse_implicit_do_delete(id);
		parse_debug_rewind(debug, dpos);
		return 0;
	}

	id->limit = parse_expr(
		src, &ptr[i], debug, &l);
	if (!id->limit)
	{
		parse_implicit_do_delete(id);
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += l;

	if (ptr[i] == ',')
	{
		i += 1;

		id->step = parse_expr(
			src, &ptr[i], debug, &l);
		if (!id->step)
		{
			parse_implicit_do_delete(id);
			parse_debug_rewind(debug, dpos);
			return 0;
		}
		i += l;
	}

	if (ptr[i++] != ')')
	{
		parse_implicit_do_delete(id);
		parse_debug_rewind(debug, dpos);
		return 0;
	}

	if (len) *len = i;
	return id;
}

parse_implicit_do_t* parse_implicit_do_copy(
	parse_implicit_do_t* id)
{
	if (!id)
		return NULL;

	parse_implicit_do_t* copy
		= (parse_implicit_do_t*)malloc(
			sizeof(parse_implicit_do_t));
	if (!copy) return NULL;

	copy->dlist = parse_lhs_copy(id->dlist);
	copy->init  = parse_assign_copy(id->init);
	copy->limit = parse_expr_copy(id->limit);
	copy->step  = parse_expr_copy(id->step);

	if (!copy->dlist
		|| !copy->init || !copy->limit
		|| (id->step && !copy->step))
	{
		parse_implicit_do_delete(copy);
		return NULL;
	}

	return copy;
}

void parse_implicit_do_delete(
	parse_implicit_do_t* id)
{
	if (!id)
		return;

	parse_lhs_delete(id->dlist);
	parse_assign_delete(id->init);
	parse_expr_delete(id->limit);
	parse_expr_delete(id->step);
	free(id);
}

bool parse_implicit_do_print(
	int fd, const parse_implicit_do_t* id)
{
	if (!dprintf_bool(fd, "(")
		|| !parse_lhs_print(fd, id->dlist)
		|| !dprintf_bool(fd, ", ")
		|| !parse_assign_print(fd, id->init)
		|| !dprintf_bool(fd, ", ")
		|| !parse_expr_print(fd, id->limit))
		return false;

	if (id->step)
	{
		if (!dprintf_bool(fd, ", ")
			|| !parse_expr_print(fd, id->step))
			return false;
	}

	return dprintf_bool(fd, ")");
}
