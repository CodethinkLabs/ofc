#include "parse.h"


static parse_array_range_t* parse_array__range(
	const sparse_t* src, const char* ptr,
	unsigned* len)
{
	unsigned i = 0;
	parse_expr_t* from = parse_expr(src, ptr, &i);
	parse_expr_t* to   = NULL;

	bool is_slice = (ptr[i] == ':');
	if (is_slice)
	{
		i += 1;

		unsigned l;
		to = parse_expr(src, &ptr[i], &l);
		if (to) i += l;
	}
	else if (!from)
	{
		return NULL;
	}

	parse_array_range_t* range
		= (parse_array_range_t*)malloc(
			sizeof(parse_array_range_t));
	if (!range)
	{
		parse_expr_delete(from);
		parse_expr_delete(to);
		return NULL;
	}

	range->is_slice = is_slice;
	range->from     = from;
	range->to       = to;

	if (len) *len = i;
	return range;
}

static void parse_array__range_delete(
	parse_array_range_t* range)
{
	if (!range)
		return;

	parse_expr_delete(range->to);
	parse_expr_delete(range->from);
	free(range);
}

static parse_array_range_t* parse_array__range_copy(
	const parse_array_range_t* range)
{
	if (!range)
		return NULL;

	parse_array_range_t* copy
		= (parse_array_range_t*)malloc(
			sizeof(parse_array_range_t));
	if (!copy) return NULL;

	copy->is_slice = range->is_slice;
	copy->from     = parse_expr_copy(range->from);
	copy->to       = parse_expr_copy(range->to);

	if ((range->from && !copy->from)
		|| (range->to && !copy->to))
	{
		parse_array__range_delete(copy);
		return NULL;
	}

	return copy;
}



parse_array_index_t* parse_array_index(
	const sparse_t* src, const char* ptr,
	unsigned* len)
{
	unsigned i = 0;
	if (ptr[i++] != '(')
		return NULL;

	parse_array_index_t* index
		= (parse_array_index_t*)malloc(
			sizeof(parse_array_index_t));
	if (!index) return NULL;

	index->count = 0;
	index->range = NULL;

	unsigned l = parse_list(src, &ptr[i], ',',
		&index->count, (void***)&index->range,
		(void*)parse_array__range,
		(void*)parse_array__range_delete);
	if (l == 0)
	{
		free(index);
		return NULL;
	}
	i += l;

	if (ptr[i++] != ')')
	{
		parse_list_delete(
			index->count, (void**)index->range,
			(void*)parse_array__range_delete);
		return NULL;
	}

	if (len) *len = i;
	return index;
}

parse_array_index_t* parse_array_index_copy(
	const parse_array_index_t* index)
{
	if (!index)
		return NULL;

	parse_array_index_t* copy
		= (parse_array_index_t*)malloc(
			sizeof(parse_array_index_t));
	if (!copy) return NULL;

	copy->count = 0;
	copy->range = NULL;

	if (!parse_list_copy(
		&copy->count, (void***)&copy->range,
		index->count, (const void**)index->range,
		(void*)parse_array__range_copy,
		(void*)parse_array__range_delete))
	{
		free(copy);
		return NULL;
	}

	return copy;
}

void parse_array_index_delete(
	parse_array_index_t* index)
{
	if (!index)
		return;

	parse_list_delete(
		index->count, (void**)index->range,
		(void*)parse_array__range_delete);
	free(index);
}
