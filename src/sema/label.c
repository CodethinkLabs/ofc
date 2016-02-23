/* Copyright 2015 Codethink Ltd.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "ofc/sema.h"


ofc_sparse_ref_t ofc_sema_label_src(
	const ofc_sema_label_t* label)
{
	if (!label)
		return OFC_SPARSE_REF_EMPTY;

	switch (label->type)
	{
		case OFC_SEMA_LABEL_STMT:
		case OFC_SEMA_LABEL_END_BLOCK:
			if (label->stmt)
				return label->stmt->src;
			break;

		case OFC_SEMA_LABEL_END_SCOPE:
			if (label->scope)
				return label->scope->src;
			break;

		default:
			break;
	}

	return OFC_SPARSE_REF_EMPTY;
}


void ofc_sema_label__delete(
	ofc_sema_label_t* label)
{
	if (!label)
		return;

	free(label);
}

static ofc_sema_label_t* ofc_sema_label__stmt(
	unsigned number, const ofc_sema_stmt_t* stmt,
	ofc_sema_label_e type)
{
	ofc_sema_label_t* label
		= (ofc_sema_label_t*)malloc(
			sizeof(ofc_sema_label_t));
	if (!label) return NULL;

	label->type   = type;
	label->number = number;
	label->stmt   = stmt;
	label->used   = false;

	return label;
}

static ofc_sema_label_t* ofc_sema_label__scope(
	unsigned number, const ofc_sema_scope_t* scope)
{
	ofc_sema_label_t* label
		= (ofc_sema_label_t*)malloc(
			sizeof(ofc_sema_label_t));
	if (!label) return NULL;

	label->type   = OFC_SEMA_LABEL_END_SCOPE;
	label->number = number;
	label->scope  = scope;
	label->used   = false;

	return label;
}

static const unsigned* ofc_sema_label__number(
	const ofc_sema_label_t* label)
{
	return (label ? &label->number : NULL);
}

static const ofc_sema_stmt_t* ofc_sema_label__stmt_key(
	const ofc_sema_label_t* label)
{
	return (label ? label->stmt : NULL);
}

static const ofc_sema_scope_t* ofc_sema_label__scope_key(
	const ofc_sema_label_t* label)
{
	return (label ? label->scope : NULL);
}

static bool ofc_sema_label__compare(
	const unsigned* a, const unsigned* b)
{
	if (!a || !b)
		return false;
	return (*a == *b);
}

static bool ofc_sema_label__stmt_ptr_compare(
	const ofc_sema_stmt_t* a,
	const ofc_sema_stmt_t* b)
{
	if (!a || !b)
		return false;
	return (a == b);
}

static bool ofc_sema_label__scope_ptr_compare(
	const ofc_sema_scope_t* a,
	const ofc_sema_scope_t* b)
{
	if (!a || !b)
		return false;
	return (a == b);
}

static uint8_t ofc_sema_label__hash(const unsigned* label)
{
	if (!label)
		return 0;

	unsigned h = *label;
	h ^= (h >> (sizeof(h) * 4));
	h ^= (h >> (sizeof(h) * 2));

	return (h & 0xFF);
}

static uint8_t ofc_sema_label__ptr_hash(
	const void* ptr)
{
	uintptr_t p = (uintptr_t)ptr;
	uint8_t h = 0;

	unsigned i;
	for (i = 0; i < sizeof(p); i++)
		h += ((p >> (i * 8)) & 0xFF);
	return h;
}

ofc_sema_label_map_t* ofc_sema_label_map_create(void)
{
	ofc_sema_label_map_t* map
		= (ofc_sema_label_map_t*)malloc(
			sizeof(ofc_sema_label_map_t));
	if (!map) return NULL;

	map->count = 0;
	map->size  = 0;
	map->label = NULL;

	map->map = ofc_hashmap_create(
		(void*)ofc_sema_label__hash,
		(void*)ofc_sema_label__compare,
		(void*)ofc_sema_label__number,
		NULL);

	map->stmt = ofc_hashmap_create(
		(void*)ofc_sema_label__ptr_hash,
		(void*)ofc_sema_label__stmt_ptr_compare,
		(void*)ofc_sema_label__stmt_key,
		(void*)ofc_sema_label__delete);

	map->end_block = ofc_hashmap_create(
		(void*)ofc_sema_label__ptr_hash,
		(void*)ofc_sema_label__stmt_ptr_compare,
		(void*)ofc_sema_label__stmt_key,
		(void*)ofc_sema_label__delete);

	map->end_scope = ofc_hashmap_create(
		(void*)ofc_sema_label__ptr_hash,
		(void*)ofc_sema_label__scope_ptr_compare,
		(void*)ofc_sema_label__scope_key,
		(void*)ofc_sema_label__delete);

	if (!map->map
		|| !map->stmt
		|| !map->end_block
		|| !map->end_scope)
	{
		ofc_sema_label_map_delete(map);
		return NULL;
	}

	return map;
}

void ofc_sema_label_map_delete(
	ofc_sema_label_map_t* map)
{
	if (!map) return;

	ofc_hashmap_delete(map->end_block);
	ofc_hashmap_delete(map->end_scope);
	ofc_hashmap_delete(map->stmt);
	ofc_hashmap_delete(map->map);
	free(map->label);

	free(map);
}

static bool ofc_smea_label_map__add_stmt(
	ofc_sema_label_map_t* map, unsigned label,
	ofc_sema_label_t* l,
	const ofc_sparse_ref_t src)
{
	const ofc_sema_label_t* duplicate
		= ofc_hashmap_find(map->map, &label);
	if (duplicate)
	{
		ofc_sparse_ref_error(src,
			"Re-definition of label %d", label);
		return false;
	}

	if (label == 0)
	{
		ofc_sparse_ref_warning(src,
			"Label zero isn't supported in standard Fortran");
	}

	unsigned slot;
	if (map->count >= map->size)
	{
		unsigned nsize = map->count + 1;
		ofc_sema_label_t** nlabel
			= (ofc_sema_label_t**)realloc(map->label,
				(sizeof(ofc_sema_label_t*) * nsize));
		if (!nlabel) return false;
		map->label = nlabel;

		unsigned i;
		for (i = map->size; i < nsize; i++)
			map->label[i] = NULL;

		slot = map->size;
		map->size = nsize;
	}
	else
	{
		unsigned i;
		for (i = 0; i < map->size; i++)
		{
            if (map->label[i] == NULL)
			{
				slot = i;
				break;
			}
		}
		if (i == map->size)
			return false;
	}

	if (!ofc_hashmap_add(map->map, l))
		return false;

	map->label[slot] = l;
	map->count++;
	return true;
}

bool ofc_sema_label_map_add_stmt(
	ofc_sema_label_map_t* map, unsigned label,
	const ofc_sema_stmt_t* stmt)
{
	if (!map || !map->map || !map->stmt)
		return false;

	ofc_sema_label_t* l
		= ofc_sema_label__stmt(label, stmt,
			OFC_SEMA_LABEL_STMT);
	if (!l) return false;

	if (!ofc_smea_label_map__add_stmt(map, label, l, stmt->src))
		return false;

	if (!ofc_hashmap_add(
		map->stmt, l))
	{
		ofc_sema_label__delete(l);
		return false;
	}

	return true;
}

bool ofc_sema_label_map_add_end_block(
	ofc_sema_label_map_t* map, unsigned label,
	const ofc_sema_stmt_t* stmt)
{
	if (!map || !map->map || !map->end_block)
		return false;

	ofc_sema_label_t* l
		= ofc_sema_label__stmt(label, stmt,
			OFC_SEMA_LABEL_END_BLOCK);
	if (!l) return false;

	if (!ofc_smea_label_map__add_stmt(map, label, l, stmt->src))
		return false;

	if (!ofc_hashmap_add(
		map->end_block, l))
	{
		ofc_sema_label__delete(l);
		return false;
	}

	return true;
}

bool ofc_sema_label_map_add_end_scope(
	ofc_sema_label_map_t* map, unsigned label,
	const ofc_sema_scope_t* scope)
{
	if (!map || !map->map || !map->end_scope)
		return false;

	ofc_sema_label_t* l
		= ofc_sema_label__scope(label, scope);
	if (!l) return false;

	if (!ofc_smea_label_map__add_stmt(map, label, l, scope->src))
		return false;

	if (!ofc_hashmap_add(
		map->end_scope, l))
	{
		ofc_sema_label__delete(l);
		return false;
	}

	return true;
}

const ofc_sema_label_t* ofc_sema_label_map_find(
	const ofc_sema_label_map_t* map, unsigned label)
{
	if (!map)
		return NULL;
	return ofc_hashmap_find(
		map->map, &label);
}

ofc_sema_label_t* ofc_sema_label_map_find_modify(
	ofc_sema_label_map_t* map, unsigned label)
{
	if (!map) return NULL;
	return ofc_hashmap_find_modify(
		map->map, &label);
}

const ofc_sema_label_t* ofc_sema_label_map_find_stmt(
	const ofc_sema_label_map_t* map,
	const ofc_sema_stmt_t*      stmt)
{
	if (!map)
		return NULL;
	return ofc_hashmap_find(
		map->stmt, stmt);
}

const ofc_sema_label_t* ofc_sema_label_map_find_end_block(
	const ofc_sema_label_map_t* map,
	const ofc_sema_stmt_t*      stmt)
{
	if (!map)
		return NULL;
	return ofc_hashmap_find(
		map->end_block, stmt);
}

const ofc_sema_label_t* ofc_sema_label_map_find_end_scope(
	const ofc_sema_label_map_t* map,
	const ofc_sema_scope_t*     scope)
{
	if (!map)
		return NULL;
	return ofc_hashmap_find(
		map->end_scope, scope);
}

void ofc_sema_label_map_remove(
	ofc_sema_label_map_t* map, ofc_sema_label_t* label)
{
    if (!map || !label)
		return;

	switch (label->type)
	{
		case OFC_SEMA_LABEL_STMT:
			ofc_hashmap_remove(map->stmt, label);
			break;
		case OFC_SEMA_LABEL_END_BLOCK:
			ofc_hashmap_remove(map->end_block, label);
			break;
		case OFC_SEMA_LABEL_END_SCOPE:
			ofc_hashmap_remove(map->end_scope, label);
			break;

		default:
			break;
	}

	ofc_hashmap_remove(map->map, label);

	unsigned i;
	for (i = 0; i < map->size; i++)
	{
		if (map->label[i]
			&& (map->label[i]->number == label->number))
		{
			map->label[i] = NULL;
			map->count--;
			break;
		}
	}

	ofc_sema_label__delete(label);
}
