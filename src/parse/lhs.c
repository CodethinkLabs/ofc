#include "parse.h"


static void parse_lhs__cleanup(
	parse_lhs_t lhs)
{
	if (lhs.type != PARSE_LHS_VARIABLE)
		parse_lhs_delete(lhs.parent);

	switch (lhs.type)
	{
		case PARSE_LHS_ARRAY:
			parse_expr_delete(lhs.array.index);
			break;
		default:
			break;
	}
}

static parse_lhs_t* parse_lhs__alloc(
	parse_lhs_t lhs)
{
	parse_lhs_t* alhs
		= (parse_lhs_t*)malloc(
			sizeof(parse_lhs_t));
	if (!alhs) return NULL;

	*alhs = lhs;
	return alhs;
}

static bool parse_lhs__clone(
	parse_lhs_t* dst, const parse_lhs_t* src)
{
	if (!src || !dst)
		return NULL;

	parse_lhs_t clone = *src;

	if ((src->type != PARSE_LHS_VARIABLE)
		&& src->parent)
	{
		parse_lhs_t parent;
		if (!parse_lhs__clone(
			&parent, src->parent))
			return false;

		clone.parent
			= parse_lhs__alloc(parent);
		if (!clone.parent)
		{
			parse_lhs__cleanup(parent);
			return false;
		}
	}

	switch (src->type)
	{
		case PARSE_LHS_ARRAY:
			if (src->array.index)
				clone.array.index = parse_expr_copy(src->array.index);
		default:
			break;
	}

	*dst = clone;
	return true;
}



parse_lhs_t* parse_lhs(
	const sparse_t* src, const char* ptr,
	unsigned* len)
{
	parse_lhs_t lhs;
	lhs.type = PARSE_LHS_VARIABLE;

	unsigned i = parse_name(
		src, ptr, &lhs.variable);
	if (i == 0) return NULL;

	parse_lhs_t* alhs
		= parse_lhs__alloc(lhs);
	if (!alhs)
	{
		parse_lhs__cleanup(lhs);
		return NULL;
	}

	if (ptr[i] == '(')
	{
		/* TODO - Implement more complex array slices. */

		i += 1;

		lhs.type   = PARSE_LHS_ARRAY;
		lhs.parent = alhs;

		unsigned l = 0;
		lhs.array.index = parse_expr(
			src, &ptr[i], &l);
		if (!lhs.array.index)
		{
			parse_lhs__cleanup(lhs);
			return NULL;
		}
		i += l;

		if (ptr[i++] != ')')
		{
			parse_lhs__cleanup(lhs);
			return NULL;
		}

		parse_lhs_t* alhs
			= parse_lhs__alloc(lhs);
		if (!alhs)
		{
			parse_lhs__cleanup(lhs);
			return NULL;
		}
	}

	/* TODO - Implement struct LHS types. */

	if (len) *len = i;
	return alhs;
}

parse_lhs_t* parse_lhs_copy(
	parse_lhs_t* lhs)
{
	if (!lhs)
		return NULL;

	parse_lhs_t clone;
	if (!parse_lhs__clone(&clone, lhs))
		return NULL;

	parse_lhs_t* copy
		= parse_lhs__alloc(clone);
	if (!copy)
	{
		parse_lhs__cleanup(clone);
		return NULL;
	}

	return copy;
}

void parse_lhs_delete(
	parse_lhs_t* lhs)
{
	if (!lhs)
		return;

	parse_lhs__cleanup(*lhs);
	free(lhs);
}



bool parse_lhs_base_name(
	const parse_lhs_t lhs,
	str_ref_t* name)
{
	if (lhs.type == PARSE_LHS_VARIABLE)
	{
		if (name) *name = lhs.variable;
		return true;
	}

	if (!lhs.parent)
		return false;

	return parse_lhs_base_name(
		*lhs.parent, name);
}
