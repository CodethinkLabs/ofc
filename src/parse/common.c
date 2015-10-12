#include "parse.h"


ofc_parse_common_group_t* ofc_parse_common_group(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = 0;

	ofc_str_ref_t group = OFC_STR_REF_EMPTY;
	if (ptr[i] == '/')
	{
		i += 1;

		unsigned l = ofc_parse_name(
			src, &ptr[i], debug, &group);
		i += l;

		if (ptr[i++] != '/')
		{
			ofc_parse_debug_rewind(debug, dpos);
			return NULL;
		}
	}

	ofc_parse_common_group_t* common
		= (ofc_parse_common_group_t*)malloc(
			sizeof(ofc_parse_common_group_t));
	if (!common)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}

	unsigned l;
	common->names
		= ofc_parse_lhs_list(
			src, &ptr[i], debug, &l);
	if (!common->names)
	{
		free(common);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}
	i += l;

	common->group = group;

	if (len) *len = i;
	return common;
}

void ofc_parse_common_group_delete(
	ofc_parse_common_group_t* group)
{
	if (!group)
		return;

	ofc_parse_lhs_list_delete(group->names);
    free(group);
}

bool ofc_parse_common_group_print(
	ofc_colstr_t* cs, const ofc_parse_common_group_t* group)
{
	if (!group)
		return false;

	if (!ofc_colstr_atomic_writef(cs, "/"))
		return false;

	if (!ofc_str_ref_empty(group->group)
		&& !ofc_str_ref_print(cs, group->group))
		return false;

	if (!ofc_colstr_atomic_writef(cs, "/"))
		return false;

	return ofc_parse_lhs_list_print(
		cs, group->names, false);
}


ofc_parse_common_group_list_t* ofc_parse_common_group_list(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	ofc_parse_common_group_list_t* list
		= (ofc_parse_common_group_list_t*)malloc(
			sizeof(ofc_parse_common_group_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->group = NULL;

	unsigned i = ofc_parse_list_seperator_optional(
		src, ptr, debug, ',',
		&list->count, (void***)&list->group,
		(void*)ofc_parse_common_group,
		(void*)ofc_parse_common_group_delete);
	if (i == 0)
	{
		free(list);
		return NULL;
	}

	if (len) *len = i;
	return list;
}

void ofc_parse_common_group_list_delete(
	ofc_parse_common_group_list_t* list)
{
	if (!list)
		return;

	ofc_parse_list_delete(
		list->count, (void**)list->group,
		(void*)ofc_parse_common_group_delete);
	free(list);
}

bool ofc_parse_common_group_list_print(
	ofc_colstr_t* cs, const ofc_parse_common_group_list_t* list)
{
	if (!list)
		return false;

	return ofc_parse_list_print(cs,
		list->count, (const void**)list->group,
		(void*)ofc_parse_common_group_print);
}
