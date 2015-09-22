#include "parse.h"


parse_call_arg_t* parse_call_arg(
	const sparse_t* src, const char* ptr,
	unsigned* len)
{
	parse_call_arg_t* call_arg
		= (parse_call_arg_t*)malloc(
			sizeof(parse_call_arg_t));
	if (!call_arg) return NULL;

	unsigned i = 0;
	if ((ptr[i] == '*')
		|| (ptr[i] == '&'))
	{
		i += 1;

		unsigned l = parse_label(
			src, &ptr[i], &call_arg->label);
		if (l == 0)
		{
			free(call_arg);
			return NULL;
		}
		i += l;

		call_arg->is_return = true;
	}
	else
	{
		call_arg->expr = parse_expr(src, ptr, &i);
		if (!call_arg->expr)
		{
			free(call_arg);
			return NULL;
		}
		call_arg->is_return = false;
	}

	if (len) *len = i;
	return call_arg;
}

void parse_call_arg_delete(
	parse_call_arg_t* call_arg)
{
	if (!call_arg)
		return;

	if (!call_arg->is_return)
		parse_expr_delete(call_arg->expr);
	free(call_arg);
}



parse_call_arg_list_t* parse_call_arg_list(
	const sparse_t* src, const char* ptr,
	unsigned* len)
{
	parse_call_arg_list_t* list
		= (parse_call_arg_list_t*)malloc(
			sizeof(parse_call_arg_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->call_arg = NULL;

	unsigned i = parse_list(src, ptr, ',',
		&list->count, (void***)&list->call_arg,
		(void*)parse_call_arg,
		(void*)parse_call_arg_delete);
	if (i == 0)
	{
		free(list);
		return NULL;
	}

	if (len) *len = i;
	return list;
}

void parse_call_arg_list_delete(
	parse_call_arg_list_t* list)
{
	if (!list)
		return;

	parse_list_delete(
		list->count, (void**)list->call_arg,
		(void*)parse_call_arg_delete);
	free(list);
}
