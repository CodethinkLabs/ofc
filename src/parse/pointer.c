#include "parse.h"


static parse_pointer_t* parse_pointer(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len)
{
	unsigned i = 0;
	if (ptr[i++] != '(')
		return NULL;

	parse_pointer_t* pointer
		= (parse_pointer_t*)malloc(
			sizeof(parse_pointer_t));
	if (!pointer) return NULL;

	unsigned dpos = parse_debug_position(debug);

	unsigned l = parse_name(
		src, &ptr[i], debug, &pointer->name);
	if (l == 0)
	{
		free(pointer);
		return NULL;
	}
	i += l;

	if (ptr[i++] != ',')
	{
		free(pointer);
		parse_debug_rewind(debug, dpos);
		return NULL;
	}

	l = parse_name(
		src, &ptr[i], debug, &pointer->target);
	if (l == 0)
	{
		free(pointer);
		parse_debug_rewind(debug, dpos);
		return NULL;
	}
	i += l;

	if (ptr[i++] != ')')
	{
		free(pointer);
		parse_debug_rewind(debug, dpos);
		return NULL;
	}

	if (len) *len = i;
	return pointer;
}

bool parse_pointer_print(
	string_t* tree_output, const parse_pointer_t* pointer)
{
	if (!pointer)
		return false;

	return (string_printf(tree_output, "(")
		&& str_ref_print(tree_output, pointer->name)
		&& string_printf(tree_output, ", ")
		&& str_ref_print(tree_output, pointer->target)
		&& string_printf(tree_output, ")"));
}


parse_pointer_list_t* parse_pointer_list(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len)
{
	parse_pointer_list_t* list
		= (parse_pointer_list_t*)malloc(
			sizeof(parse_pointer_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->pointer = NULL;

	unsigned i = parse_list(
		src, ptr, debug, ',',
		&list->count, (void***)&list->pointer,
		(void*)parse_pointer, free);
	if (i == 0)
	{
		free(list);
		return NULL;
	}

	if (len) *len = i;
	return list;
}

void parse_pointer_list_delete(
	parse_pointer_list_t* list)
{
	if (!list)
		return;

	parse_list_delete(
		list->count, (void**)list->pointer,
		free);
	free(list);
}

bool parse_pointer_list_print(
	string_t* tree_output, const parse_pointer_list_t* list)
{
	return parse_list_print(
		tree_output, list->count, (const void**)list->pointer,
		(void*)parse_pointer_print);
}
