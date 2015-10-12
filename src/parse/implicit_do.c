#include <ofc/parse.h>


ofc_parse_implicit_do_t* ofc_parse_implicit_do(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	unsigned i = 0;

	if (ptr[i++] != '(')
		return 0;

	ofc_parse_implicit_do_t* id
		= (ofc_parse_implicit_do_t*)malloc(
			sizeof(ofc_parse_implicit_do_t));
	if (!id) return 0;

	id->dlist = NULL;
	id->init = NULL;
	id->limit = NULL;
	id->step = NULL;

	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned l;
	id->dlist = ofc_parse_lhs(
		src, &ptr[i], debug, &l);
	if (!id->dlist)
	{
		free(id);
		return 0;
	}
	i += l;

	if (ptr[i++] != ',')
	{
		ofc_parse_implicit_do_delete(id);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}

	id->init = ofc_parse_assign_init(
		src, &ptr[i], debug, &l);
	if (!id->init)
	{
		ofc_parse_implicit_do_delete(id);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += l;

	if (ptr[i++] != ',')
	{
		ofc_parse_implicit_do_delete(id);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}

	id->limit = ofc_parse_expr(
		src, &ptr[i], debug, &l);
	if (!id->limit)
	{
		ofc_parse_implicit_do_delete(id);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += l;

	if (ptr[i] == ',')
	{
		i += 1;

		id->step = ofc_parse_expr(
			src, &ptr[i], debug, &l);
		if (!id->step)
		{
			ofc_parse_implicit_do_delete(id);
			ofc_parse_debug_rewind(debug, dpos);
			return 0;
		}
		i += l;
	}

	if (ptr[i++] != ')')
	{
		ofc_parse_implicit_do_delete(id);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}

	if (len) *len = i;
	return id;
}

ofc_parse_implicit_do_t* ofc_parse_implicit_do_copy(
	ofc_parse_implicit_do_t* id)
{
	if (!id)
		return NULL;

	ofc_parse_implicit_do_t* copy
		= (ofc_parse_implicit_do_t*)malloc(
			sizeof(ofc_parse_implicit_do_t));
	if (!copy) return NULL;

	copy->dlist = ofc_parse_lhs_copy(id->dlist);
	copy->init  = ofc_parse_assign_copy(id->init);
	copy->limit = ofc_parse_expr_copy(id->limit);
	copy->step  = ofc_parse_expr_copy(id->step);

	if (!copy->dlist
		|| !copy->init || !copy->limit
		|| (id->step && !copy->step))
	{
		ofc_parse_implicit_do_delete(copy);
		return NULL;
	}

	return copy;
}

void ofc_parse_implicit_do_delete(
	ofc_parse_implicit_do_t* id)
{
	if (!id)
		return;

	ofc_parse_lhs_delete(id->dlist);
	ofc_parse_assign_delete(id->init);
	ofc_parse_expr_delete(id->limit);
	ofc_parse_expr_delete(id->step);
	free(id);
}

bool ofc_parse_implicit_do_print(
	ofc_colstr_t* cs, const ofc_parse_implicit_do_t* id)
{
	if (!ofc_colstr_atomic_writef(cs, "(")
		|| !ofc_parse_lhs_print(cs, id->dlist, false)
		|| !ofc_colstr_atomic_writef(cs, ", ")
		|| !ofc_parse_assign_print(cs, id->init)
		|| !ofc_colstr_atomic_writef(cs, ", ")
		|| !ofc_parse_expr_print(cs, id->limit))
		return false;

	if (id->step)
	{
		if (!ofc_colstr_atomic_writef(cs, ", ")
			|| !ofc_parse_expr_print(cs, id->step))
			return false;
	}

	return ofc_colstr_atomic_writef(cs, ")");
}
