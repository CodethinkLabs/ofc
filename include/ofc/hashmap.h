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
void ofc_hashmap_remove(ofc_hashmap_t* map, const void* item);

const void* ofc_hashmap_find(const ofc_hashmap_t* map, const void* key);

/* Don't modify the key in this function. */
void* ofc_hashmap_find_modify(ofc_hashmap_t* map, const void* key);

bool ofc_hashmap_foreach(
	ofc_hashmap_t* map, void* param,
	bool (*func)(void* item, void* param));

#endif
