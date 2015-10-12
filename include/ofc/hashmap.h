#ifndef __ofc_hashmap_h__
#define __ofc_hashmap_h__

#include <stdbool.h>
#include <stdint.h>

typedef uint8_t     (*ofc_hashmap_hash_f       )(const void* key);
typedef bool        (*ofc_hashmap_key_compare_f)(const void* a, const void* b);
typedef const void* (*ofc_hashmap_item_key_f   )(const void* item);
typedef void        (*ofc_hashmap_item_delete_f)(void* item);

typedef struct ofc_hashmap_s ofc_hashmap_t;

ofc_hashmap_t* ofc_hashmap_create(
	ofc_hashmap_hash_f        hash,
	ofc_hashmap_key_compare_f key_compare,
	ofc_hashmap_item_key_f    item_key,
	ofc_hashmap_item_delete_f item_delete);
void ofc_hashmap_delete(ofc_hashmap_t* map);

bool ofc_hashmap_add(ofc_hashmap_t* map, void* item);

const void* ofc_hashmap_find(const ofc_hashmap_t* map, const void* key);

/* Don't modify the key in this function. */
void* ofc_hashmap_find_modify(ofc_hashmap_t* map, const void* key);

#endif
