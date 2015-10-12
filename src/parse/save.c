#include "parse.h"


ofc_parse_save_t* ofc_parse_save(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	ofc_parse_save_t* save
		= (ofc_parse_save_t*)malloc(
			sizeof(ofc_parse_save_t));
	if (!save) return NULL;

	unsigned i = 0;
	if (ptr[i] == '/')
	{
		i += 1;

		unsigned l = ofc_parse_name(
			src, &ptr[i], debug,
			&save->common);
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
		save->lhs = ofc_parse_lhs(
			src, ptr, debug, &i);
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

void ofc_parse_save_delete(
	ofc_parse_save_t* save)
{
	if (!save)
		return;

	if (!save->is_common)
		ofc_parse_lhs_delete(save->lhs);
	free(save);
}

bool ofc_parse_save_print(
	ofc_colstr_t* cs, const ofc_parse_save_t* save)
{
	if (!save)
		return false;

	if (save->is_common)
		return (ofc_colstr_atomic_writef(cs, "/")
			&& ofc_str_ref_print(cs, save->common)
			&& ofc_colstr_atomic_writef(cs, "/"));

	return ofc_parse_lhs_print(cs, save->lhs, false);
}



ofc_parse_save_list_t* ofc_parse_save_list(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	ofc_parse_save_list_t* list
		= (ofc_parse_save_list_t*)malloc(
			sizeof(ofc_parse_save_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->save = NULL;

	unsigned i = ofc_parse_list(
		src, ptr, debug, ',',
		&list->count, (void***)&list->save,
		(void*)ofc_parse_save,
		(void*)ofc_parse_save_delete);
	if (i == 0)
	{
		free(list);
		return NULL;
	}

	if (len) *len = i;
	return list;
}

void ofc_parse_save_list_delete(
	ofc_parse_save_list_t* list)
{
	if (!list)
		return;

	ofc_parse_list_delete(
		list->count, (void**)list->save,
		(void*)ofc_parse_save_delete);
	free(list);
}

bool ofc_parse_save_list_print(
	ofc_colstr_t* cs, const ofc_parse_save_list_t* list)
{
	if (!list)
		return false;

	return ofc_parse_list_print(cs,
		list->count, (const void**)list->save,
		(void*)ofc_parse_save_print);
}
