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

	decl->type_implicit = true;
	decl->type = implicit->c[index];
	decl->name = name;
	decl->redecl = NULL;
	decl->has_init = false;
	decl->init = PARSE_EXPR_EMPTY;
	return true;
}

unsigned parse_decl(
	const sparse_t* src, const char* ptr,
	const hashmap_t* decl_map,
	parse_decl_t* decl)
{
	unsigned i = parse_type(
		src, ptr, &decl->type);
	if (i == 0) return 0;

	decl->type_implicit = false;

	unsigned len = parse_name(
		src, &ptr[i], &decl->name);
	if (len == 0) return 0;

	decl->name = str_ref(&ptr[i], len);
	i += len;

	decl->redecl = NULL;

	const parse_decl_t* existing = hashmap_find(
		decl_map, &decl->name);
	if (existing)
		decl->redecl = existing;

	decl->has_init = (ptr[i] == '=');
	if (decl->has_init)
	{
		i += 1;

		len = parse_expr(
			src, &ptr[i], &decl->init);
		if (len == 0) return 0;

		i += len;
	}
	else
	{
		decl->init = PARSE_EXPR_EMPTY;
	}

	if ((ptr[i] == '\r')
		|| (ptr[i] == '\n')
		|| (ptr[i] == ';'))
	{
		i += 1;
	}
	else
	{
		/* Not a declaration. */
		if (decl->has_init)
			parse_expr_cleanup(decl->init);
		return 0;
	}

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
