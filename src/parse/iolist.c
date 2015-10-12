#include <ofc/parse.h>


static ofc_parse_ioarg_t* ofc_parse_ioarg(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	ofc_parse_ioarg_t* arg
		= (ofc_parse_ioarg_t*)malloc(
			sizeof(ofc_parse_ioarg_t));
	if (!arg) return NULL;

	unsigned dpos = ofc_parse_debug_position(debug);

	arg->is_implicit_do = false;
	arg->expr = ofc_parse_expr(
		src, ptr, debug, len);

	if (arg->expr)
		return arg;

	unsigned i = 0;
	if (ptr[i] != '(')
	{
		free(arg);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}

	arg->is_implicit_do = true;
	unsigned l;
	arg->id = ofc_parse_implicit_do(
		src, &ptr[i], debug, &l);
	if (!arg->id)
	{
		free(arg);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}
	i += l;

	if (len) *len = i;
	return arg;
}

static void ofc_parse_ioarg_delete(
	ofc_parse_ioarg_t* arg)
{
	if (!arg)
		return;

	if (arg->is_implicit_do)
		ofc_parse_implicit_do_delete(arg->id);
	else
		ofc_parse_expr_delete(arg->expr);

	free(arg);
}

bool ofc_parse_ioarg_print(
	ofc_colstr_t* cs, const ofc_parse_ioarg_t* arg)
{
	if (!arg)
		return false;

	if (arg->is_implicit_do)
		return ofc_parse_implicit_do_print(cs, arg->id);
	else
		return ofc_parse_expr_print(cs, arg->expr);
}


ofc_parse_iolist_t* ofc_parse_iolist(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	ofc_parse_iolist_t* list
		= (ofc_parse_iolist_t*)malloc(
			sizeof(ofc_parse_iolist_t));
	if (!list) return NULL;

	list->count = 0;
	list->arg = NULL;

	unsigned i = ofc_parse_list(
		src, ptr, debug, ',',
		&list->count, (void***)&list->arg,
		(void*)ofc_parse_ioarg,
		(void*)ofc_parse_ioarg_delete);
	if (i == 0)
	{
		free(list);
		return NULL;
	}

	if (len) *len = i;
	return list;
}

void ofc_parse_iolist_delete(
	ofc_parse_iolist_t* list)
{
	if (!list)
		return;

	ofc_parse_list_delete(
		list->count, (void**)list->arg,
		(void*)ofc_parse_ioarg_delete);
	free(list);
}

bool ofc_parse_iolist_print(
	ofc_colstr_t* cs, const ofc_parse_iolist_t* list)
{
	return ofc_parse_list_print(
		cs, list->count, (const void**)list->arg,
		(void*)ofc_parse_ioarg_print);
}
