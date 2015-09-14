#ifndef __hashmap_h__
#define __hashmap_h__

#include <stdbool.h>
#include <stdint.h>

typedef uint8_t     (*hashmap_hash_f       )(const void* key);
typedef bool        (*hashmap_key_compare_f)(const void* a, const void* b);
typedef const void* (*hashmap_item_key_f   )(const void* item);
typedef void        (*hashmap_item_delete_f)(void* item);

typedef struct hashmap_s hashmap_t;

hashmap_t* hashmap_create(
	hashmap_hash_f        hash,
	hashmap_key_compare_f key_compare,
	hashmap_item_key_f    item_key,
	hashmap_item_delete_f item_delete);
void hashmap_delete(hashmap_t* map);

bool hashmap_add(hashmap_t* map, void* item);

const void* hashmap_find(const hashmap_t* map, const void* key);

#endif
