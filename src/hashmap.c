#include "hashmap.h"
#include <stdlib.h>
#include <string.h>

typedef struct hashmap__entry_s hashmap__entry_t;

struct hashmap__entry_s
{
	void             *item;
	hashmap__entry_t *next;
};

struct hashmap_s
{
	hashmap_hash_f        hash;
	hashmap_key_compare_f key_compare;
	hashmap_item_key_f    item_key;
	hashmap_item_delete_f item_delete;

	hashmap__entry_t* base[256];
};


static uint8_t hashmap__hash(const char* key)
{
	uint8_t h = 0;
	unsigned i;
	for (i = 0; key[i] != '\0'; i++)
		h += key[i];
	return h;
}

static bool hashmap__key_compare(const char* a, const char* b)
{
	return (strcmp(a, b) == 0);
}

static const char* hashmap__item_key(const char* item)
{
	return item;
}


hashmap_t* hashmap_create(
	hashmap_hash_f        hash,
	hashmap_key_compare_f key_compare,
	hashmap_item_key_f    item_key,
	hashmap_item_delete_f item_delete)
{
	hashmap_t* map
		= (hashmap_t*)malloc(
			sizeof(hashmap_t));
	if (!map) return NULL;

	map->hash        = (hash ? hash : (void*)hashmap__hash);
	map->key_compare = (key_compare ? key_compare : (void*)hashmap__key_compare);
	map->item_key    = (item_key ? item_key : (void*)hashmap__item_key);
	map->item_delete = item_delete;

	unsigned i;
	for (i = 0; i < 256; i++)
		map->base[i] = NULL;

	return map;
}

static void hashmap__entry_delete(
	hashmap__entry_t* entry,
	hashmap_item_delete_f item_delete)
{
	if (!entry)
		return;

	hashmap__entry_delete(
		entry->next, item_delete);

	if (item_delete)
		item_delete(entry->item);
	free(entry);
}

void hashmap_delete(hashmap_t* map)
{
	if (!map)
		return;

	unsigned i;
	for (i = 0; i < 256; i++)	
		hashmap__entry_delete(
			map->base[i], map->item_delete);
	free(map);
}



bool hashmap_add(hashmap_t* map, void* item)
{
	if (!map || !item
		|| !map->item_key
		|| !map->hash)
		return false;

	const void* key = map->item_key(item);
	if (!key) return false;

	if (hashmap_find(map, key))
		return false;

	uint8_t hash = map->hash(key);

	hashmap__entry_t* entry
		= (hashmap__entry_t*)malloc(
			sizeof(hashmap__entry_t));
	if (!entry) return false;

	entry->item = item;
	entry->next = map->base[hash];
	map->base[hash] = entry;

	return true;
}

const void* hashmap_find(const hashmap_t* map, const void* key)
{
	if (!map || !key
		|| !map->item_key)
		return NULL;

	uint8_t hash = map->hash(key);	

	hashmap__entry_t* entry;
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
