#include "parse.h"


static parse_call_arg_t* parse__call_arg(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	bool named, bool force, unsigned* len)
{
	parse_call_arg_t* call_arg
		= (parse_call_arg_t*)malloc(
			sizeof(parse_call_arg_t));
	if (!call_arg) return NULL;

	unsigned dpos = parse_debug_position(debug);

	unsigned i = 0;
	call_arg->name = STR_REF_EMPTY;
	if (named)
	{
		str_ref_t ident;
		unsigned l = parse_ident(
			src, &ptr[i], debug, &ident);
		if ((l > 0) && (ptr[i + l] == '='))
		{
			call_arg->name = ident;
			i += (l + 1);
		}
		else if (force)
		{
			free(call_arg);
			parse_debug_rewind(debug, dpos);
			return NULL;
		}
	}

	bool was_asterisk = (ptr[i] == '*');
	if (was_asterisk
		|| (ptr[i] == '&'))
	{
		i += 1;

		unsigned l = parse_label(
			src, &ptr[i], debug,
			&call_arg->label);
		if (l == 0)
		{
			if (!was_asterisk)
			{
				free(call_arg);
				parse_debug_rewind(debug, dpos);
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
		call_arg->expr = parse_expr(
			src, &ptr[i], debug, &l);
		if (!call_arg->expr)
		{
			free(call_arg);
			parse_debug_rewind(debug, dpos);
			return NULL;
		}
		i += l;
		call_arg->type = PARSE_CALL_ARG_EXPR;
	}

	if (len) *len = i;
	return call_arg;
}

parse_call_arg_t* parse_call_arg_force_named(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len)
{
	return parse__call_arg(
		src, ptr, debug,
		true, true, len);
}

parse_call_arg_t* parse_call_arg_named(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len)
{
	return parse__call_arg(
		src, ptr, debug,
		true, false, len);
}

parse_call_arg_t* parse_call_arg(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len)
{
	return parse__call_arg(
		src, ptr, debug,
		false, false, len);
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

bool parse_call_arg_print(
	string_t* tree_output, const parse_call_arg_t* call_arg)
{
	if (!call_arg)
		return false;

	if (!str_ref_empty(call_arg->name))
	{
		if (!str_ref_print(tree_output, call_arg->name)
			|| !string_printf(tree_output, "="))
			return false;
	}

	switch (call_arg->type)
	{
		case PARSE_CALL_ARG_RETURN:
		case PARSE_CALL_ARG_ASTERISK:
			if (!string_printf(tree_output, "*"))
				return false;
			break;
		default:
			break;
	}

	switch (call_arg->type)
	{
		case PARSE_CALL_ARG_RETURN:
			if (!parse_label_print(
				tree_output, call_arg->label))
				return false;
			break;
		case PARSE_CALL_ARG_EXPR:
			if (!parse_expr_print(
				tree_output, call_arg->expr))
				return false;
			break;
		default:
			break;
	}

	return true;
}



static parse_call_arg_list_t* parse_call_arg__list(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	bool named, bool force, unsigned* len)
{
	parse_call_arg_list_t* list
		= (parse_call_arg_list_t*)malloc(
			sizeof(parse_call_arg_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->call_arg = NULL;

	unsigned i = parse_list(src, ptr, debug, ',',
		&list->count, (void***)&list->call_arg,
		(named ? (force ? (void*)parse_call_arg_force_named
				: (void*)parse_call_arg_named)
			: (void*)parse_call_arg),
		(void*)parse_call_arg_delete);
	if (i == 0)
	{
		free(list);
		return NULL;
	}

	if (len) *len = i;
	return list;
}

parse_call_arg_list_t* parse_call_arg_list_force_named(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len)
{
	return parse_call_arg__list(
		src, ptr, debug, true, true, len);
}

parse_call_arg_list_t* parse_call_arg_list_named(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len)
{
	return parse_call_arg__list(
		src, ptr, debug, true, false, len);
}

parse_call_arg_list_t* parse_call_arg_list(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len)
{
	return parse_call_arg__list(
		src, ptr, debug, false, false, len);
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

bool parse_call_arg_list_print(
	string_t* tree_output, const parse_call_arg_list_t* list)
{
	return parse_list_print(
		tree_output, list->count, (const void**)list->call_arg,
		(void*)parse_call_arg_print);
}
