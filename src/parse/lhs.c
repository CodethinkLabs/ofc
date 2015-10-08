#include "parse.h"


static void parse_lhs__cleanup(
	parse_lhs_t lhs)
{
	switch (lhs.type)
	{
		case PARSE_LHS_ARRAY:
		case PARSE_LHS_STAR_LEN:
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
		case PARSE_LHS_STAR_LEN:
			parse_expr_delete(lhs.star_len.len);
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
		case PARSE_LHS_STAR_LEN:
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

		case PARSE_LHS_STAR_LEN:
			if (src->star_len.len)
			{
				clone.star_len.len = parse_expr_copy(
					src->star_len.len);
				if (!clone.star_len.len)
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



static parse_lhs_t* parse_lhs__array(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_lhs_t* parent, unsigned* len)
{
	if (!parent)
		return NULL;

	switch (parent->type)
	{
		case PARSE_LHS_IMPLICIT_DO:
			return NULL;
		default:
			break;
	}

	unsigned i = 0;
	if (ptr[i] != '(')
		return 0;

	unsigned dpos = parse_debug_position(debug);

	parse_lhs_t lhs;
	unsigned l = 0;
	lhs.array.index = parse_array_index(
		src, &ptr[i], debug, &l);
	if (!lhs.array.index)
	{
		if (ptr[i + 1] != ')')
			return NULL;
		l = 2;
	}
	i += l;

	lhs.type        = PARSE_LHS_ARRAY;
	lhs.parent      = parent;

	parse_lhs_t* alhs
		= parse_lhs__alloc(lhs);
	if (!alhs)
	{
		parse_lhs__cleanup(lhs);
		parse_debug_rewind(debug, dpos);
		return NULL;
	}

	if (len) *len = i;
	return alhs;
}

static parse_lhs_t* parse_lhs__star_len(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_lhs_t* parent, unsigned* len)
{
	if (!parent)
		return NULL;

	switch (parent->type)
	{
		case PARSE_LHS_IMPLICIT_DO:
			return NULL;
		default:
			break;
	}

	parse_lhs_t lhs;
	lhs.type   = PARSE_LHS_STAR_LEN;
	lhs.parent = parent;

	unsigned dpos = parse_debug_position(debug);

	unsigned i = parse_star_len(
		src, ptr, debug,
		&lhs.star_len.len,
		&lhs.star_len.var);
	if (i == 0) return 0;

	parse_lhs_t* alhs
		= parse_lhs__alloc(lhs);
	if (!alhs)
	{
		parse_lhs__cleanup(lhs);
		parse_debug_rewind(debug, dpos);
		return NULL;
	}

	if (len) *len = i;
	return alhs;
}


static parse_lhs_t* parse_lhs__member(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_lhs_t* parent, unsigned* len)
{
	if (!parent)
		return NULL;

	switch (parent->type)
	{
		case PARSE_LHS_IMPLICIT_DO:
			return NULL;
		default:
			break;
	}


	parse_lhs_t lhs;
	lhs.type = PARSE_LHS_MEMBER_TYPE;

	unsigned dpos = parse_debug_position(debug);

	unsigned i;
	if (ptr[0] == '.')
	{
		i = parse_operator(
			src, ptr, debug, NULL);
		if (i > 0)
		{
			parse_debug_rewind(debug, dpos);
			return NULL;
		}
		lhs.type = PARSE_LHS_MEMBER_STRUCTURE;
	}
	else if (ptr[0] != '%')
	{
		return NULL;
	}
	i = 1;

	unsigned l = parse_name(
		src, &ptr[i], debug, &lhs.member.name);
	if (l == 0) return NULL;
	i += l;

	lhs.parent = parent;

	parse_lhs_t* alhs
		= parse_lhs__alloc(lhs);
	if (!alhs)
	{
		parse_lhs__cleanup(lhs);
		parse_debug_rewind(debug, dpos);
		return NULL;
	}

	if (len) *len = i;
	return alhs;
}


static parse_lhs_t* parse__lhs(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	bool star_len,
	unsigned* len)
{
	parse_lhs_t lhs;

	unsigned dpos = parse_debug_position(debug);

	lhs.type = PARSE_LHS_VARIABLE;
	unsigned i = parse_name(
		src, ptr, debug, &lhs.variable);
	if (i == 0)
	{
		lhs.implicit_do = parse_implicit_do(
			src, ptr, debug, &i);
		if (!lhs.implicit_do) return NULL;
		lhs.type = PARSE_LHS_IMPLICIT_DO;
	}

	parse_lhs_t* alhs
		= parse_lhs__alloc(lhs);
	if (!alhs)
	{
		parse_lhs__cleanup(lhs);
		parse_debug_rewind(debug, dpos);
		return NULL;
	}

	while (true)
	{
		unsigned l;
		parse_lhs_t* child_lhs;

		child_lhs = parse_lhs__array(
				src, &ptr[i], debug, alhs, &l);
		if (child_lhs)
		{
			i += l;
			alhs = child_lhs;
			continue;
		}

		if (star_len)
		{
			child_lhs = parse_lhs__star_len(
					src, &ptr[i], debug, alhs, &l);
			if (child_lhs)
			{
				i += l;
				alhs = child_lhs;
				continue;
			}
		}

		child_lhs = parse_lhs__member(
				src, &ptr[i], debug, alhs, &l);
		if (child_lhs)
		{
			i += l;
			alhs = child_lhs;
			continue;
		}

		break;
	}

	if (len) *len = i;
	return alhs;
}

parse_lhs_t* parse_lhs_star_len(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len)
{
	return parse__lhs(
		src, ptr, debug, true ,len);
}

parse_lhs_t* parse_lhs(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len)
{
	return parse__lhs(
		src, ptr, debug, false ,len);
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

bool parse_lhs_print(
	colstr_t* cs, const parse_lhs_t* lhs,
	bool is_decl)
{
	switch (lhs->type)
	{
		case PARSE_LHS_VARIABLE:
			return str_ref_print(cs, lhs->variable);
		case PARSE_LHS_ARRAY:
			if (!parse_lhs_print(
				cs, lhs->parent, is_decl))
				return false;
			if (lhs->array.index)
				return parse_array_index_print(
					cs, lhs->array.index, is_decl);
			return colstr_atomic_writef(cs, "()");
		case PARSE_LHS_STAR_LEN:
			if (!parse_lhs_print(
				cs, lhs->parent, is_decl)
				|| !colstr_atomic_writef(cs, "*"))
				return false;
			if (lhs->star_len.var)
			{
				return colstr_atomic_writef(cs, "(*)");
			}
			else if (lhs->star_len.len)
			{
				bool bracketed = (lhs->star_len.len->type
					!= PARSE_EXPR_CONSTANT);

				if (bracketed && !colstr_atomic_writef(cs, "("))
					return false;

				if (!parse_expr_print(
					cs, lhs->star_len.len))
					return false;

				return (!bracketed || colstr_atomic_writef(cs, "("));
			}
			break;
		case PARSE_LHS_MEMBER_TYPE:
			return (parse_lhs_print(cs, lhs->parent, is_decl)
				&& colstr_atomic_writef(cs, "%%")
				&& str_ref_print(cs, lhs->member.name));
		case PARSE_LHS_MEMBER_STRUCTURE:
			return (parse_lhs_print(cs, lhs->parent, is_decl)
				&& colstr_atomic_writef(cs, ".")
				&& str_ref_print(cs, lhs->member.name));
		case PARSE_LHS_IMPLICIT_DO:
			return parse_implicit_do_print(
				cs, lhs->implicit_do);
		default:
			break;
	}

	return false;
}

static bool parse_lhs_print__decl(
	colstr_t* cs, const parse_lhs_t* lhs)
{
	return parse_lhs_print(cs, lhs, true);
}

static bool parse_lhs_print__not_decl(
	colstr_t* cs, const parse_lhs_t* lhs)
{
	return parse_lhs_print(cs, lhs, false);
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
	parse_debug_t* debug,
	unsigned* len)
{
	parse_lhs_list_t* list
		= (parse_lhs_list_t*)malloc(
			sizeof(parse_lhs_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->lhs = NULL;

	unsigned i = parse_list(
		src, ptr, debug, ',',
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
	parse_debug_t* debug,
	unsigned* len)
{
	unsigned i = 0;

	if (ptr[i++] != '(')
		return NULL;

	unsigned dpos = parse_debug_position(debug);

	unsigned l;
	parse_lhs_list_t* list
		= parse_lhs_list(src, &ptr[i], debug, &l);
	if (!list) return NULL;
	i += l;

	if (ptr[i++] != ')')
	{
		parse_lhs_list_delete(list);
		parse_debug_rewind(debug, dpos);
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

bool parse_lhs_list_print(
	colstr_t* cs, const parse_lhs_list_t* list,
	bool is_decl)
{
	return parse_list_print(cs,
		list->count, (const void**)list->lhs,
		(void*)(is_decl
			? parse_lhs_print__decl
			: parse_lhs_print__not_decl));
}

bool parse_lhs_list_bracketed_print(
	colstr_t* cs, const parse_lhs_list_t* list,
	bool is_decl)
{
	return (colstr_atomic_writef(cs, "(")
		&& parse_lhs_list_print(cs, list, is_decl)
		&& colstr_atomic_writef(cs, ")"));
}
