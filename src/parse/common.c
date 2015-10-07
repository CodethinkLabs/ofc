#include "parse.h"


parse_common_group_t* parse_common_group(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len)
{
	unsigned dpos = parse_debug_position(debug);

	unsigned i = 0;

	str_ref_t group = STR_REF_EMPTY;
	if (ptr[i] == '/')
	{
		i += 1;

		unsigned l = parse_name(
			src, &ptr[i], debug, &group);
		i += l;

		if (ptr[i++] != '/')
		{
			parse_debug_rewind(debug, dpos);
			return NULL;
		}
	}

	parse_common_group_t* common
		= (parse_common_group_t*)malloc(
			sizeof(parse_common_group_t));
	if (!common)
	{
		parse_debug_rewind(debug, dpos);
		return NULL;
	}

	unsigned l;
	common->names
		= parse_lhs_list(
			src, &ptr[i], debug, &l);
	if (!common->names)
	{
		free(common);
		parse_debug_rewind(debug, dpos);
		return NULL;
	}
	i += l;

	common->group = group;

	if (len) *len = i;
	return common;
}

void parse_common_group_delete(
	parse_common_group_t* group)
{
	if (!group)
		return;

	parse_lhs_list_delete(group->names);
    free(group);
}

bool parse_common_group_print(
	colstr_t* cs, const parse_common_group_t* group)
{
	if (!group)
		return false;

	if (!str_ref_empty(group->group))
	{
		if (!colstr_atomic_writef(cs, "/")
			|| !str_ref_print(cs, group->group)
			|| !colstr_atomic_writef(cs, "/"))
			return false;
	}

	return parse_lhs_list_print(
		cs, group->names);
}


parse_common_group_list_t* parse_common_group_list(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len)
{
	parse_common_group_list_t* list
		= (parse_common_group_list_t*)malloc(
			sizeof(parse_common_group_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->group = NULL;

	unsigned i = parse_list_seperator_optional(
		src, ptr, debug, ',',
		&list->count, (void***)&list->group,
		(void*)parse_common_group,
		(void*)parse_common_group_delete);
	if (i == 0)
	{
		free(list);
		return NULL;
	}

	if (len) *len = i;
	return list;
}

void parse_common_group_list_delete(
	parse_common_group_list_t* list)
{
	if (!list)
		return;

	parse_list_delete(
		list->count, (void**)list->group,
		(void*)parse_common_group_delete);
	free(list);
}

bool parse_common_group_list_print(
	colstr_t* cs, const parse_common_group_list_t* list)
{
	if (!list)
		return false;

	return parse_list_print(cs,
		list->count, (const void**)list->group,
		(void*)parse_common_group_print);
}
