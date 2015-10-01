#include "parse.h"


static parse_array_range_t* parse_array__range(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len)
{
	unsigned dpos = parse_debug_position(debug);

	unsigned i = 0;
	parse_expr_t* first  = parse_expr(src, ptr, debug, &i);
	parse_expr_t* last   = NULL;
	parse_expr_t* stride = NULL;

	bool is_slice = false;
	if (!first && (ptr[0] == '*'))
	{
		is_slice = true;
		i += 1;
	}

	if (ptr[i] == ':')
	{
		is_slice = true;
		i += 1;

		unsigned l;
		last = parse_expr(src, &ptr[i], debug, &l);
		if (last)
		{
			i += l;
		}
		else if (ptr[i] == '*')
		{
			i += 1;
		}

		if (ptr[i] == ':')
		{
			i += 1;

			stride = parse_expr(src, &ptr[i], debug, &l);
			if (stride)
			{
				i += l;
			}
			else if (ptr[i] == '*')
			{
				i += 1;
			}
		}
	}
	else if (!first && !is_slice)
	{
		parse_debug_rewind(debug, dpos);
		return NULL;
	}

	parse_array_range_t* range
		= (parse_array_range_t*)malloc(
			sizeof(parse_array_range_t));
	if (!range)
	{
		parse_expr_delete(first);
		parse_expr_delete(last);
		parse_expr_delete(stride);
		parse_debug_rewind(debug, dpos);
		return NULL;
	}

	range->is_slice = is_slice;
	range->first    = first;
	range->last     = last;
	range->stride   = stride;

	if (len) *len = i;
	return range;
}

static void parse_array__range_delete(
	parse_array_range_t* range)
{
	if (!range)
		return;

	parse_expr_delete(range->first);
	parse_expr_delete(range->last);
	parse_expr_delete(range->stride);
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
	copy->first    = parse_expr_copy(range->first);
	copy->last     = parse_expr_copy(range->last);
	copy->stride   = parse_expr_copy(range->stride);

	if ((range->first && !copy->first)
		|| (range->last && !copy->last)
		|| (range->stride && !copy->stride))
	{
		parse_array__range_delete(copy);
		return NULL;
	}

	return copy;
}

static bool parse_array__range_print(
	string_t* tree_output, const parse_array_range_t* range)
{
	if (!range)
		return false;

	if (range->first)
	{
		if (!parse_expr_print(
			tree_output, range->first))
			return false;
	}

	if (range->is_slice)
	{
		if (!string_printf(tree_output, ":"))
			return false;

		if (range->last)
		{
			if (!parse_expr_print(
				tree_output, range->last))
				return false;
		}

		if (range->stride)
		{
			if (!string_printf(tree_output, ":")
				|| !parse_expr_print(
					tree_output, range->stride))
				return false;
		}
	}
	else if (!range->first)
	{
		/* Invalid array index. */
		return false;
	}

	return true;
}



parse_array_index_t* parse_array_index(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
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

	unsigned dpos = parse_debug_position(debug);

	unsigned l = parse_list(
		src, &ptr[i], debug, ',',
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
		parse_debug_rewind(debug, dpos);
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

bool parse_array_index_print(
	string_t* tree_output, const parse_array_index_t* index)
{
	if (!index)
		return false;

	if (!string_printf(tree_output, "("))
		return false;

	unsigned i;
	for (i = 0; i < index->count; i++)
	{
		if ((i > 0) && !string_printf(tree_output, ", "))
			return false;

		if (!parse_array__range_print(
			tree_output, index->range[i]))
			return false;
	}

	if (!string_printf(tree_output, ")"))
		return false;

	return true;
}
