#include "parse.h"


static parse_record_t* parse_record(
	const sparse_t* src, const char* ptr,
	unsigned* len)
{
	unsigned i = 0;
	if (ptr[i++] != '/')
		return 0;

	parse_record_t* record
		= (parse_record_t*)malloc(
			sizeof(parse_record_t));
	if (!record) return NULL;

	unsigned l = parse_name(src, &ptr[i],
		&record->structure);
	if (l == 0)
	{
		free(record);
		return NULL;
	}
	i += l;

	if (ptr[i++] != '/')
	{
		free(record);
		return NULL;
	}

	l = parse_name(src, &ptr[i],
		&record->name);
	if (l == 0)
	{
		free(record);
		return NULL;
	}
	i += l;

	if (len) *len = i;
	return record;
}

static void parse_record_delete(
	parse_record_t* record)
{
	if (!record)
		return;

	free(record);
}



parse_record_list_t* parse_record_list(
	const sparse_t* src, const char* ptr,
	unsigned* len)
{
	parse_record_list_t* list
		= (parse_record_list_t*)malloc(
			sizeof(parse_record_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->record = NULL;

	unsigned i = parse_list(src, ptr, ',',
		&list->count, (void***)&list->record,
		(void*)parse_record,
		(void*)parse_record_delete);
	if (i == 0)
	{
		free(list);
		return NULL;
	}

	if (len) *len = i;
	return list;
}

void parse_record_list_delete(
	parse_record_list_t* list)
{
	if (!list)
		return;

	parse_list_delete(
		list->count, (void**)list->record,
		(void*)parse_record_delete);
	free(list);
}
