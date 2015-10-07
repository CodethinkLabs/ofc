#include "parse.h"


static parse_ioarg_t* parse_ioarg(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len)
{
	parse_ioarg_t* arg
		= (parse_ioarg_t*)malloc(
			sizeof(parse_ioarg_t));
	if (!arg) return NULL;

	unsigned dpos = parse_debug_position(debug);

	arg->is_implicit_do = false;
	arg->expr = parse_expr(
		src, ptr, debug, len);

	if (arg->expr)
		return arg;

	unsigned i = 0;
	if (ptr[i] != '(')
	{
		free(arg);
		parse_debug_rewind(debug, dpos);
		return NULL;
	}

	arg->is_implicit_do = true;
	unsigned l;
	arg->id = parse_implicit_do(
		src, &ptr[i], debug, &l);
	if (!arg->id)
	{
		free(arg);
		parse_debug_rewind(debug, dpos);
		return NULL;
	}
	i += l;

	if (len) *len = i;
	return arg;
}

static void parse_ioarg_delete(
	parse_ioarg_t* arg)
{
	if (!arg)
		return;

	if (arg->is_implicit_do)
		parse_implicit_do_delete(arg->id);
	else
		parse_expr_delete(arg->expr);

	free(arg);
}

bool parse_ioarg_print(
	colstr_t* cs, const parse_ioarg_t* arg)
{
	if (!arg)
		return false;

	if (arg->is_implicit_do)
		return parse_implicit_do_print(cs, arg->id);
	else
		return parse_expr_print(cs, arg->expr);
}


parse_iolist_t* parse_iolist(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len)
{
	parse_iolist_t* list
		= (parse_iolist_t*)malloc(
			sizeof(parse_iolist_t));
	if (!list) return NULL;

	list->count = 0;
	list->arg = NULL;

	unsigned i = parse_list(
		src, ptr, debug, ',',
		&list->count, (void***)&list->arg,
		(void*)parse_ioarg,
		(void*)parse_ioarg_delete);
	if (i == 0)
	{
		free(list);
		return NULL;
	}

	if (len) *len = i;
	return list;
}

void parse_iolist_delete(
	parse_iolist_t* list)
{
	if (!list)
		return;

	parse_list_delete(
		list->count, (void**)list->arg,
		(void*)parse_ioarg_delete);
	free(list);
}

bool parse_iolist_print(
	colstr_t* cs, const parse_iolist_t* list)
{
	return parse_list_print(
		cs, list->count, (const void**)list->arg,
		(void*)parse_ioarg_print);
}
