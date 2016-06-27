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

#include <stdlib.h>
#include <string.h>

#include "ofc/hashmap.h"

typedef struct ofc_hashmap__entry_s ofc_hashmap__entry_t;

struct ofc_hashmap__entry_s
{
	void             *item;
	ofc_hashmap__entry_t *next;
};

struct ofc_hashmap_s
{
	ofc_hashmap_hash_f        hash;
	ofc_hashmap_key_compare_f key_compare;
	ofc_hashmap_item_key_f    item_key;
	ofc_hashmap_item_delete_f item_delete;

	ofc_hashmap__entry_t* base[256];
};


static uint8_t ofc_hashmap__hash(const char* key)
{
	uint8_t h = 0;
	unsigned i;
	for (i = 0; key[i] != '\0'; i++)
		h += key[i];
	return h;
}

static bool ofc_hashmap__key_compare(const char* a, const char* b)
{
	return (strcmp(a, b) == 0);
}

static const char* ofc_hashmap__item_key(const char* item)
{
	return item;
}


ofc_hashmap_t* ofc_hashmap_create(
	ofc_hashmap_hash_f        hash,
	ofc_hashmap_key_compare_f key_compare,
	ofc_hashmap_item_key_f    item_key,
	ofc_hashmap_item_delete_f item_delete)
{
	ofc_hashmap_t* map
		= (ofc_hashmap_t*)malloc(
			sizeof(ofc_hashmap_t));
	if (!map) return NULL;

	map->hash        = (hash ? hash
		: (ofc_hashmap_hash_f)ofc_hashmap__hash);
	map->key_compare = (key_compare ? key_compare
		: (ofc_hashmap_key_compare_f)ofc_hashmap__key_compare);
	map->item_key    = (item_key ? item_key
		: (ofc_hashmap_item_key_f)ofc_hashmap__item_key);
	map->item_delete = item_delete;

	unsigned i;
	for (i = 0; i < 256; i++)
		map->base[i] = NULL;

	return map;
}

static void ofc_hashmap__entry_delete(
	ofc_hashmap__entry_t* entry,
	ofc_hashmap_item_delete_f item_delete)
{
	if (!entry)
		return;

	ofc_hashmap__entry_delete(
		entry->next, item_delete);

	if (item_delete)
		item_delete(entry->item);
	free(entry);
}

void ofc_hashmap_delete(ofc_hashmap_t* map)
{
	if (!map)
		return;

	unsigned i;
	for (i = 0; i < 256; i++)
		ofc_hashmap__entry_delete(
			map->base[i], map->item_delete);
	free(map);
}



bool ofc_hashmap_add(ofc_hashmap_t* map, void* item)
{
	if (!map || !item
		|| !map->item_key
		|| !map->hash)
		return false;

	const void* key = map->item_key(item);
	if (!key) return false;

	uint8_t hash = map->hash(key);

	ofc_hashmap__entry_t* entry
		= (ofc_hashmap__entry_t*)malloc(
			sizeof(ofc_hashmap__entry_t));
	if (!entry) return false;

	entry->item = item;
	entry->next = map->base[hash];
	map->base[hash] = entry;

	return true;
}

void ofc_hashmap_remove(
	ofc_hashmap_t* map, const void* item)
{
	if (!map || !item
		|| !map->item_key
		|| !map->hash)
		return;

	const void* key = map->item_key(item);
	if (!key) return;

	uint8_t hash = map->hash(key);

	ofc_hashmap__entry_t* prev;
	ofc_hashmap__entry_t* node;
	for (prev = NULL, node = map->base[hash];
		node && (node->item != item);
		prev = node, node = node->next);
	if (!node) return;

	if (!prev)
		map->base[hash] = node->next;
	else
		prev->next = node->next;

	free(node);
}


void* ofc_hashmap_find_modify(ofc_hashmap_t* map, const void* key)
{
	if (!map || !key
		|| !map->item_key)
		return NULL;

	uint8_t hash = map->hash(key);

	ofc_hashmap__entry_t* entry;
	for (entry = map->base[hash]; entry; entry = entry->next)
	{
		const void* ikey = map->item_key(entry->item);

		if (key == ikey)
			return entry->item;

		if (map->key_compare
			&& map->key_compare(key, ikey))
			return entry->item;
	}

	return NULL;
}

const void* ofc_hashmap_find(const ofc_hashmap_t* map, const void* key)
{
	return (const void*)ofc_hashmap_find_modify(
		(ofc_hashmap_t*)map, key);
}


bool ofc_hashmap_foreach(
	ofc_hashmap_t* map, void* param,
	bool (*func)(void* item, void* param))
{
	if (!map || !func)
		return false;

	unsigned i;
	for (i = 0; i < 256; i++)
	{
		ofc_hashmap__entry_t* entry;
		for (entry = map->base[i]; entry; entry = entry->next)
		{
			if (!func(entry->item, param))
				return false;
		}
	}

	return true;
}
