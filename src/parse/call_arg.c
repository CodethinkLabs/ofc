#include "parse.h"


static parse_call_arg_t* parse__call_arg(
	const sparse_t* src, const char* ptr,
	bool named, unsigned* len)
{
	parse_call_arg_t* call_arg
		= (parse_call_arg_t*)malloc(
			sizeof(parse_call_arg_t));
	if (!call_arg) return NULL;

	unsigned i = 0;
	call_arg->name = STR_REF_EMPTY;
	if (named)
	{
		str_ref_t ident;
		unsigned l = parse_ident(
			src, &ptr[i], &ident);
		if ((l > 0) && (ptr[i + l] == '='))
		{
			call_arg->name = ident;
			i += (l + 1);
		}
	}

	bool was_asterisk = (ptr[i] == '*');
	if (was_asterisk
		|| (ptr[i] == '&'))
	{
		i += 1;

		unsigned l = parse_label(
			src, &ptr[i], &call_arg->label);
		if (l == 0)
		{
			if (!was_asterisk)
			{
				free(call_arg);
				return NULL;
			}

			call_arg->type = PARSE_CALL_ARG_ASTERISK;
		}
		else
		{
			i += l;
			call_arg->type = PARSE_CALL_ARG_RETURN;
		}
	}
	else
	{
		unsigned l;
		call_arg->expr = parse_expr(src, &ptr[i], &l);
		if (!call_arg->expr)
		{
			free(call_arg);
			return NULL;
		}
		i += l;
		call_arg->type = PARSE_CALL_ARG_EXPR;
	}

	if (len) *len = i;
	return call_arg;
}

parse_call_arg_t* parse_call_arg_named(
	const sparse_t* src, const char* ptr,
	unsigned* len)
{
	return parse__call_arg(src, ptr, true, len);
}

parse_call_arg_t* parse_call_arg(
	const sparse_t* src, const char* ptr,
	unsigned* len)
{
	return parse__call_arg(src, ptr, false, len);
}

void parse_call_arg_delete(
	parse_call_arg_t* call_arg)
{
	if (!call_arg)
		return;

	if (call_arg->type == PARSE_CALL_ARG_EXPR)
		parse_expr_delete(call_arg->expr);
	free(call_arg);
}



static parse_call_arg_list_t* parse_call_arg__list(
	const sparse_t* src, const char* ptr,
	bool named, unsigned* len)
{
	parse_call_arg_list_t* list
		= (parse_call_arg_list_t*)malloc(
			sizeof(parse_call_arg_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->call_arg = NULL;

	unsigned i = parse_list(src, ptr, ',',
		&list->count, (void***)&list->call_arg,
		(named ? (void*)parse_call_arg_named
			: (void*)parse_call_arg_named),
		(void*)parse_call_arg_delete);
	if (i == 0)
	{
		free(list);
		return NULL;
	}

	if (len) *len = i;
	return list;
}

parse_call_arg_list_t* parse_call_arg_list_named(
	const sparse_t* src, const char* ptr,
	unsigned* len)
{
	return parse_call_arg__list(
		src, ptr, true, len);
}

parse_call_arg_list_t* parse_call_arg_list(
	const sparse_t* src, const char* ptr,
	unsigned* len)
{
	return parse_call_arg__list(
		src, ptr, false, len);
}

parse_call_arg_list_t* parse_call_arg_list_wrap(
	parse_call_arg_t* arg)
{
	if (!arg)
		return NULL;

	parse_call_arg_list_t* list
		= (parse_call_arg_list_t*)malloc(
			sizeof(parse_call_arg_list_t));
	if (!list) return NULL;

	list->call_arg = (parse_call_arg_t**)malloc(
		sizeof(parse_call_arg_t*));
	if (!list->call_arg)
	{
		free(list);
		return NULL;
	}

	list->count = 1;
	list->call_arg[0] = arg;

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
