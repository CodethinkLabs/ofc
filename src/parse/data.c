#include "parse.h"


static parse_clist_entry_t* parse_clist_entry(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len)
{
	unsigned dpos = parse_debug_position(debug);

	parse_clist_entry_t* entry
		= (parse_clist_entry_t*)malloc(
			sizeof(parse_clist_entry_t));
	if (!entry) return NULL;

	unsigned i = parse_unsigned(
		src, ptr, debug, &entry->repeat);
	if ((i > 0) && (ptr[i] == '*'))
	{
		i += 1;
	}
	else
	{
		entry->repeat = 0;
		i = 0;
		parse_debug_rewind(debug, dpos);
	}

	unsigned l;
	entry->expr = parse_expr(
		src, &ptr[i], debug, &l);
	if (!entry->expr)
	{
		free(entry);
		parse_debug_rewind(debug, dpos);
		return NULL;
	}
	i += l;

	if (len) *len = i;
	return entry;
}

static void parse_clist_entry_delete(
	parse_clist_entry_t* entry)
{
	if (!entry)
		return;

	parse_expr_delete(entry->expr);
	free(entry);
}

static bool parse_clist_entry_print(
	colstr_t* cs, const parse_clist_entry_t* entry)
{
	if (!entry)
		return false;

	if ((entry->repeat > 1)
		&& !colstr_atomic_writef(cs, "%u*", entry->repeat))
		return false;

	return parse_expr_print(cs, entry->expr);
}



parse_clist_t* parse_clist(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len)
{
	unsigned dpos = parse_debug_position(debug);

	unsigned i = 0;
	if (ptr[i++] != '/')
		return NULL;

	parse_clist_t* list
		= (parse_clist_t*)malloc(
			sizeof(parse_clist_t));
	if (!list) return NULL;

	list->count = 0;
	list->entry = NULL;

	unsigned l = parse_list(
		src, &ptr[i], debug, ',',
		&list->count, (void***)&list->entry,
		(void*)parse_clist_entry,
		(void*)parse_clist_entry_delete);
	if (l == 0)
	{
		/* clist may not be empty. */
		free(list);
		return NULL;
	}
	i += l;

	if (ptr[i++] != '/')
	{
		parse_clist_delete(list);
		parse_debug_rewind(debug, dpos);
		return NULL;
	}

	if (len) *len = i;
	return list;
}

void parse_clist_delete(
	parse_clist_t* list)
{
	if (!list)
		return;

	parse_list_delete(
		list->count, (void**)list->entry,
		(void*)parse_clist_entry_delete);
	free(list);
}

bool parse_clist_print(
	colstr_t* cs, const parse_clist_t* list)
{
	if (!list)
		return false;

	return parse_list_print(cs,
		list->count, (const void**)list->entry,
		(void*)parse_clist_entry_print);
}



static parse_data_entry_t* parse_data_entry(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len)
{
	parse_data_entry_t* entry
		= (parse_data_entry_t*)malloc(
			sizeof(parse_data_entry_t));
	if (!entry) return NULL;

	unsigned dpos = parse_debug_position(debug);

	unsigned i;
	entry->nlist = parse_lhs_list(
		src, ptr, debug, &i);
	if (!entry->nlist)
	{
		free(entry);
		return NULL;
	}

	unsigned l;
	entry->clist = parse_clist(
		src, &ptr[i], debug, &l);
	if (!entry->clist)
	{
		parse_lhs_list_delete(entry->nlist);
		free(entry);
		parse_debug_rewind(debug, dpos);
		return NULL;
	}
	i += l;

	if (len) *len = i;
	return entry;
}

static void parse_data_entry_delete(
	parse_data_entry_t* entry)
{
	if (!entry)
		return;

	parse_lhs_list_delete(entry->nlist);
	parse_clist_delete(entry->clist);
	free(entry);
}

static bool parse_data_entry_print(
	colstr_t* cs, const parse_data_entry_t* entry)
{
	if (!entry)
		return false;

	return (parse_lhs_list_print(cs, entry->nlist, false)
		&& colstr_atomic_writef(cs, "/")
		&& parse_clist_print(cs, entry->clist)
		&& colstr_atomic_writef(cs, "/"));
}


parse_data_list_t* parse_data_list(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len)
{
	parse_data_list_t* list
		= (parse_data_list_t*)malloc(
			sizeof(parse_data_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->entry = NULL;

	unsigned i = parse_list_seperator_optional(
		src, ptr, debug, ',',
		&list->count, (void***)&list->entry,
		(void*)parse_data_entry,
		(void*)parse_data_entry_delete);
	if (i == 0)
	{
		/* data_list may not be empty. */
		free(list);
		return NULL;
	}

	if (len) *len = i;
	return list;
}

void parse_data_list_delete(
	parse_data_list_t* list)
{
	if (!list)
		return;

	parse_list_delete(
		list->count, (void**)list->entry,
		(void*)parse_data_entry_delete);
	free(list);
}

bool parse_data_list_print(
	colstr_t* cs, const parse_data_list_t* list)
{
	if (!list)
		return false;

	return parse_list_print(cs,
		list->count, (const void**)list->entry,
		(void*)parse_data_entry_print);
}
