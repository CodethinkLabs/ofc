#include "parse.h"
#include <ctype.h>


bool parse_decl_create_implicit(
	const str_ref_t name,
	const parse_implicit_t* implicit,
	parse_decl_t* decl)
{
	if (str_ref_empty(name)
		|| !implicit)
		return false;

	unsigned index = (toupper(name.base[0]) - 'A');
	if (index > 26) return false;

	if (implicit->c[index].type == PARSE_TYPE_NONE)
		return false;

	if (!parse_type_clone(
		&decl->type, &implicit->c[index]))
		return false;

	decl->type_implicit = true;
	decl->name = name;
	decl->redecl = NULL;
	decl->has_init = false;
	decl->init = PARSE_EXPR_EMPTY;
	return true;
}

static unsigned parse__decl(
	const sparse_t* src, const char* ptr,
	const parse_type_t type,
	const hashmap_t* decl_map,
	parse_decl_t** decl)
{
	parse_decl_t d;
	unsigned i = parse_name(
		src, ptr, &d.name);
	if (i == 0) return 0;

	const parse_decl_t* existing = hashmap_find(
		decl_map, &d.name);
	d.redecl = existing;

	d.has_init = (ptr[i] == '=');
	if (d.has_init)
	{
		i += 1;

		unsigned len = parse_expr(
			src, &ptr[i], &d.init);
		if (len == 0) return 0;

		i += len;
	}
	else
	{
		d.init = PARSE_EXPR_EMPTY;
	}

	d.type_implicit = false;
	if (!parse_type_clone(
		&d.type, &type))
	{
		if (d.has_init)
			parse_expr_cleanup(d.init);
		return 0;
	}

	*decl = parse_decl_alloc(d);
	if (!*decl)
	{
		parse_decl_cleanup(d);
		return 0;
	}

	return i;
}

unsigned parse_decl(
	const sparse_t* src, const char* ptr,
	hashmap_t* decl_map)
{
	parse_type_t type;
	unsigned i = parse_type(
		src, ptr, &type);
	if (i == 0) return 0;

	parse_decl_t** decl = NULL;
	unsigned mc = 0;

	unsigned c;
	for (c = 0; true; c++)
	{
		unsigned j = i;
		if (c > 0)
		{
			if (ptr[j] != ',')
				break;
			j += 1;
		}

		if (c >= mc)
		{
			mc = (mc == 0 ? 16 : (mc << 1));
			parse_decl_t** ndecl
				= (parse_decl_t**)realloc(decl,
					(mc * sizeof(parse_decl_t*)));
			if (!ndecl)
			{
				unsigned e;
				for (e = 0; e < c; e++)
					parse_decl_delete(decl[c]);
				free(decl);
				return 0;
			}
			decl = ndecl;
		}

		unsigned len = parse__decl(
			src, &ptr[j], type, decl_map, &decl[c]);
		if (len == 0)
			break;

		i = (j + len);
	}

	parse_type_cleanup(type);

	if (c == 0)
		return 0;

	if ((ptr[i] == '\r')
		|| (ptr[i] == '\n')
		|| (ptr[i] == ';'))
	{
		i += 1;
	}
	else
	{
		unsigned e;
		for (e = 0; e < c; e++)
			parse_decl_delete(decl[e]);
		free(decl);
		return 0;
	}

	unsigned e;
	for (e = 0; e < c; e++)
	{
		if (!hashmap_add(decl_map, decl[e]))
		{
			/* This should only happen if we run out of memory. */
			abort();
		}
	}
	free(decl);

	return i;
}

void parse_decl_cleanup(
	parse_decl_t decl)
{
	if (!decl.type_implicit)
		parse_type_cleanup(decl.type);
	if (decl.has_init)
		parse_expr_cleanup(decl.init);
}


parse_decl_t* parse_decl_alloc(
	parse_decl_t decl)
{
	parse_decl_t* adecl
		= (parse_decl_t*)malloc(
			sizeof(parse_decl_t));
	if (adecl) *adecl = decl;
	return adecl;
}

void parse_decl_delete(
	parse_decl_t* decl)
{
	if (!decl)
		return;

	parse_decl_cleanup(*decl);
	free(decl);
}


uint8_t parse_decl_hash_ci(
	const str_ref_t* key)
{
	uint8_t h = 0;
	unsigned i;
	for (i = 0; i < key->size; i++)
		h += tolower(key->base[i]);
	return h;
}

uint8_t parse_decl_hash(
	const str_ref_t* key)
{
	uint8_t h = 0;
	unsigned i;
	for (i = 0; i < key->size; i++)
		h += key->base[i];
	return h;
}

const str_ref_t* parse_decl_key(
	const parse_decl_t* decl)
{
	return (decl ? &decl->name : NULL);
}

bool parse_decl_key_compare_ci(
	const str_ref_t* a, const str_ref_t* b)
{
	if (!a || !b)
		return false;
	return str_ref_equal_ci(*a, *b);
}

bool parse_decl_key_compare(
	const str_ref_t* a, const str_ref_t* b)
{
	if (!a || !b)
		return false;
	return str_ref_equal(*a, *b);
}
