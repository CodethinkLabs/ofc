#include "parse.h"


parse_save_t* parse_save(
	const sparse_t* src, const char* ptr,
	unsigned* len)
{
	parse_save_t* save
		= (parse_save_t*)malloc(
			sizeof(parse_save_t));
	if (!save) return NULL;

	unsigned i = 0;
	if (ptr[i] == '/')
	{
		i += 1;

		unsigned l = parse_name(
			src, &ptr[i], &save->common);
		if (l == 0)
		{
			free(save);
			return NULL;
		}
		i += l;

		if (ptr[i++] != '/')
		{
			free(save);
			return NULL;
		}

		save->is_common = true;
	}
	else
	{
		save->lhs = parse_lhs(src, ptr, &i);
		if (!save->lhs)
		{
			free(save);
			return NULL;
		}
		save->is_common = false;
	}

	if (len) *len = i;
	return save;
}

void parse_save_delete(
	parse_save_t* save)
{
	if (!save)
		return;

	if (!save->is_common)
		parse_lhs_delete(save->lhs);
	free(save);
}

bool parse_save_print(
	int fd, const parse_save_t* save)
{
	if (!save)
		return false;

	if (save->is_common)
		return (dprintf_bool(fd, "/")
			&& str_ref_print(fd, save->common)
			&& dprintf_bool(fd, "/"));

	return parse_lhs_print(fd, save->lhs);
}



parse_save_list_t* parse_save_list(
	const sparse_t* src, const char* ptr,
	unsigned* len)
{
	parse_save_list_t* list
		= (parse_save_list_t*)malloc(
			sizeof(parse_save_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->save = NULL;

	unsigned i = parse_list(src, ptr, ',',
		&list->count, (void***)&list->save,
		(void*)parse_save,
		(void*)parse_save_delete);
	if (i == 0)
	{
		free(list);
		return NULL;
	}

	if (len) *len = i;
	return list;
}

void parse_save_list_delete(
	parse_save_list_t* list)
{
	if (!list)
		return;

	parse_list_delete(
		list->count, (void**)list->save,
		(void*)parse_save_delete);
	free(list);
}

bool parse_save_list_print(
	int fd, const parse_save_list_t* list)
{
	if (!list)
		return false;

	return parse_list_print(fd,
		list->count, (const void**)list->save,
		(void*)parse_save_print);
}
