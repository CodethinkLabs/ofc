#include "parse.h"


static void parse_lhs__cleanup(
	parse_lhs_t lhs)
{
	switch (lhs.type)
	{
		case PARSE_LHS_ARRAY:
		case PARSE_LHS_MEMBER_STRUCTURE:
		case PARSE_LHS_MEMBER_TYPE:
			parse_lhs_delete(lhs.parent);
			break;
		default:
			break;
	}

	switch (lhs.type)
	{
		case PARSE_LHS_ARRAY:
			parse_array_index_delete(lhs.array.index);
			break;
		case PARSE_LHS_IMPLICIT_DO:
			parse_implicit_do_delete(lhs.implicit_do);
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

	switch (src->type)
	{
		case PARSE_LHS_ARRAY:
		case PARSE_LHS_MEMBER_STRUCTURE:
		case PARSE_LHS_MEMBER_TYPE:
			if (src->parent)
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
			break;
		default:
			break;
	}

	switch (src->type)
	{
		case PARSE_LHS_ARRAY:
			if (src->array.index)
			{
				clone.array.index = parse_array_index_copy(
					src->array.index);
				if (!clone.array.index)
				{
					parse_lhs__cleanup(clone);
					return false;
				}
			}
			break;

		case PARSE_LHS_IMPLICIT_DO:
			if (src->implicit_do)
			{
				clone.implicit_do = parse_implicit_do_copy(
					src->implicit_do);
				if (!clone.implicit_do)
				{
					parse_lhs__cleanup(clone);
					return false;
				}
			}
			break;

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
	if (i == 0)
	{
		lhs.implicit_do = parse_implicit_do(src, ptr, &i);
		if (!lhs.implicit_do) return NULL;
		lhs.type = PARSE_LHS_IMPLICIT_DO;
	}

	parse_lhs_t* alhs
		= parse_lhs__alloc(lhs);
	if (!alhs)
	{
		parse_lhs__cleanup(lhs);
		return NULL;
	}

	if ((lhs.type == PARSE_LHS_VARIABLE)
		&& (ptr[i] == '('))
	{

		unsigned l = 0;
		parse_array_index_t* index
			= parse_array_index(
				src, &ptr[i], &l);
		if (!index
			&& (ptr[i + 1] == ')'))
			l = 2;

		if (l > 0)
		{
			i += l;

			lhs.type        = PARSE_LHS_ARRAY;
			lhs.parent      = alhs;
			lhs.array.index = index;

			parse_lhs_t* alhs
				= parse_lhs__alloc(lhs);
			if (!alhs)
			{
				parse_lhs__cleanup(lhs);
				return NULL;
			}
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



parse_lhs_list_t* parse_lhs_list(
	const sparse_t* src, const char* ptr,
	unsigned* len)
{
	parse_lhs_list_t* list
		= (parse_lhs_list_t*)malloc(
			sizeof(parse_lhs_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->lhs = NULL;

	unsigned i = parse_list(src, ptr, ',',
		&list->count, (void***)&list->lhs,
		(void*)parse_lhs,
		(void*)parse_lhs_delete);
	if (i == 0)
	{
		free(list);
		return NULL;
	}

	if (len) *len = i;
	return list;
}

parse_lhs_list_t* parse_lhs_list_bracketed(
	const sparse_t* src, const char* ptr,
	unsigned* len)
{
	unsigned i = 0;

	if (ptr[i++] != '(')
		return NULL;

	unsigned l;
	parse_lhs_list_t* list
		= parse_lhs_list(src, &ptr[i], &l);
	if (!list) return NULL;
	i += l;

	if (ptr[i++] != ')')
	{
		parse_lhs_list_delete(list);
		return NULL;
	}

	if (len) *len = i;
	return list;
}

void parse_lhs_list_delete(
	parse_lhs_list_t* list)
{
	if (!list)
		return;

	parse_list_delete(
		list->count, (void**)list->lhs,
		(void*)parse_lhs_delete);
	free(list);
}
