#include "parse.h"


parse_assign_t* parse_assign(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len)
{
	unsigned dpos = parse_debug_position(debug);

	unsigned i;
	parse_lhs_t* name
		= parse_lhs(src, ptr, debug, &i);
	if (!name) return NULL;

	parse_expr_t* init = NULL;
	if (ptr[i] == '=')
	{
		i += 1;

		unsigned l;
		init = parse_expr(
			src, &ptr[i], debug, &l);
		if (!init)
		{
			parse_lhs_delete(name);
			parse_debug_rewind(debug, dpos);
			return NULL;
		}
		i += l;
	}

	parse_assign_t* assign
		= (parse_assign_t*)malloc(
			sizeof(parse_assign_t));
	if (!assign)
	{
		parse_expr_delete(init);
		parse_lhs_delete(name);
		parse_debug_rewind(debug, dpos);
		return NULL;
	}

	assign->name = name;
	assign->init = init;

	if (len) *len = i;
	return assign;
}

/* TODO - Make this a flag to a static function for speed. */
parse_assign_t* parse_assign_init(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len)
{
	unsigned dpos = parse_debug_position(debug);

	unsigned i;
	parse_assign_t* assign
		= parse_assign(src, ptr, debug, &i);
	if (!assign) return NULL;

	if (!assign->init)
	{
		parse_assign_delete(assign);
		parse_debug_rewind(debug, dpos);
		return NULL;
	}

	if (len) *len = i;
	return assign;
}

parse_assign_t* parse_assign_copy(
	const parse_assign_t* assign)
{
	if (!assign)
		return NULL;

	parse_assign_t* copy
		= (parse_assign_t*)malloc(
			sizeof(parse_assign_t));
	if (!copy) return NULL;

	copy->name = parse_lhs_copy(assign->name);
	copy->init = parse_expr_copy(assign->init);

	if (!copy->name
		|| (assign->init && !copy->init))
	{
		parse_assign_delete(copy);
		return NULL;
	}

	return copy;
}

void parse_assign_delete(
	parse_assign_t* assign)
{
	if (!assign)
		return;

	parse_expr_delete(assign->init);
	parse_lhs_delete(assign->name);
	free(assign);
}

bool parse_assign_print(
	int fd, const parse_assign_t* assign)
{
	if (!assign)
		return false;

	if (!parse_lhs_print(
		fd, assign->name))
		return false;

	if (assign->init)
	{
		if (!dprintf_bool(fd, " = ")
			|| !parse_expr_print(fd, assign->init))
			return false;
	}

	return true;
}



parse_assign_list_t* parse_assign_list(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len)
{
	parse_assign_list_t* list
		= (parse_assign_list_t*)malloc(
			sizeof(parse_assign_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->assign = NULL;

	unsigned i = parse_list(
		src, ptr, debug, ',',
		&list->count, (void***)&list->assign,
		(void*)parse_assign,
		(void*)parse_assign_delete);
	if (i == 0) return NULL;

	if (len) *len = i;
	return list;
}

parse_assign_list_t* parse_assign_list_copy(
	const parse_assign_list_t* list)
{
	if (!list)
		return NULL;

	parse_assign_list_t* copy
		= (parse_assign_list_t*)malloc(
			sizeof(parse_assign_list_t));
	if (!copy) return NULL;

	copy->count = 0;
	copy->assign = NULL;

	if (!parse_list_copy(
		&copy->count, (void***)&copy->assign,
		list->count, (const void**)list->assign,
		(void*)parse_assign_copy,
		(void*)parse_assign_delete))
	{
		free(copy);
		return NULL;
	}

	return copy;
}

void parse_assign_list_delete(
	parse_assign_list_t* list)
{
	if (!list)
		return;

	parse_list_delete(
		list->count, (void**)list->assign,
		(void*)parse_assign_delete);

	free(list);
}

bool parse_assign_list_print(
	int fd, const parse_assign_list_t* list)
{
	if (!list)
		return false;

	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		if ((i > 0) && (!dprintf_bool(fd, ", ")))
			return false;

		if (!parse_assign_print(
			fd, list->assign[i]))
			return false;
	}

	return (i > 0);
}
