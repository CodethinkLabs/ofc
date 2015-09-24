#include "parse.h"


parse_decl_t* parse_decl(
	const sparse_t* src, const char* ptr,
	unsigned* len)
{
	parse_decl_t* decl
		= (parse_decl_t*)malloc(
			sizeof(parse_decl_t));
	if (!decl) return NULL;

	unsigned i;
	decl->lhs = parse_lhs(src, ptr, &i);
	if (!decl->lhs)
	{
		free(decl);
		return NULL;
	}

	decl->len = NULL;
	if (ptr[i] == '*')
	{
		unsigned l;
		decl->len = parse_expr(
			src, &ptr[i + 1], &l);
		if (decl->len) i += (l + 1);
	}

	decl->init_expr  = NULL;
	decl->init_clist = NULL;
	if (ptr[i] == '=')
	{
		unsigned l;
		decl->init_expr = parse_expr(
			src, &ptr[i + 1], &l);
		if (decl->init_expr) i += (l + 1);
	}
	else if (ptr[i] == '/')
	{
		unsigned l;
		decl->init_clist = parse_clist(
			src, &ptr[i + 1], &l);
		if (decl->init_clist) i += (l + 1);
	}

	if (len) *len = i;
	return decl;
}

void parse_decl_delete(
	parse_decl_t* decl)
{
	if (!decl)
		return;

	parse_expr_delete(decl->init_expr);
	parse_clist_delete(decl->init_clist);

	parse_expr_delete(decl->len);
	parse_lhs_delete(decl->lhs);
	free(decl);
}



parse_decl_list_t* parse_decl_list(
	const sparse_t* src, const char* ptr,
	unsigned* len)
{
	parse_decl_list_t* list
		= (parse_decl_list_t*)malloc(
			sizeof(parse_decl_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->decl = NULL;

	unsigned i = parse_list(src, ptr, ',',
		&list->count, (void***)&list->decl,
		(void*)parse_decl,
		(void*)parse_decl_delete);
	if (i == 0)
	{
		free(list);
		return NULL;
	}

	if (len) *len = i;
	return list;
}

void parse_decl_list_delete(
	parse_decl_list_t* list)
{
	if (!list)
		return;

	parse_list_delete(
		list->count, (void**)list->decl,
		(void*)parse_decl_delete);
	free(list);
}
