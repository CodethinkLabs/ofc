#include "parse.h"

unsigned parse_lhs(
	const sparse_t* src, const char* ptr,
	parse_lhs_t* lhs)
{
	unsigned i = parse_name(
		src, ptr, &lhs->variable);
	if (i == 0) return 0;

	lhs->type = PARSE_LHS_VARIABLE;

	if (ptr[i] == '(')
	{
		/* TODO - Implement more complex array slices. */

		i += 1;

		parse_lhs_t* plhs
			= parse_lhs_alloc(*lhs);
		if (!plhs) return 0;

		lhs->type   = PARSE_LHS_ARRAY;
		lhs->parent = plhs;
		lhs->array.index = NULL;

		parse_expr_t index;
		unsigned len = parse_expr(
			src, &ptr[i], &index);
		if (len == 0)
		{
			parse_lhs_cleanup(*lhs);
			return 0;
		}
		i += len;

		lhs->array.index
			= parse_expr_alloc(index);
		if (!lhs->array.index)
		{
			parse_expr_cleanup(index);
			parse_lhs_cleanup(*lhs);
			return 0;
		}

		if (ptr[i++] != ')')
		{
			parse_lhs_cleanup(*lhs);
			return 0;
		}
	}

	/* TODO - Implement struct LHS types. */

	return i;
}

void parse_lhs_cleanup(
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

bool parse_lhs_clone(
	parse_lhs_t* dst, const parse_lhs_t* src)
{
	if (!src || !dst)
		return NULL;

	parse_lhs_t clone = *src;

	if ((src->type != PARSE_LHS_VARIABLE)
		&& src->parent)
	{
		parse_lhs_t parent;
		if (!parse_lhs_clone(
			&parent, src->parent))
			return false;

		clone.parent
			= parse_lhs_alloc(parent);
		if (!clone.parent)
		{
			parse_lhs_cleanup(parent);
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


parse_lhs_t* parse_lhs_alloc(
	parse_lhs_t lhs)
{
	parse_lhs_t* alhs
		= (parse_lhs_t*)malloc(
			sizeof(parse_lhs_t));
	if (!alhs) return NULL;

	*alhs = lhs;
	return alhs;
}

void parse_lhs_delete(
	parse_lhs_t* lhs)
{
	if (!lhs)
		return;

	parse_lhs_cleanup(*lhs);
	free(lhs);
}
