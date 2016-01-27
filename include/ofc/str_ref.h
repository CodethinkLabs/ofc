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

#ifndef __ofc_str_ref_h__
#define __ofc_str_ref_h__

#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <ofc/colstr.h>

typedef struct
{
	const char* base;
	unsigned    size;
} ofc_str_ref_t;

#define OFC_STR_REF_EMPTY (ofc_str_ref_t){ .base = NULL, .size = 0 }

static inline ofc_str_ref_t ofc_str_ref(const char* base, unsigned size)
	{ return (ofc_str_ref_t){ base, size }; }
static inline ofc_str_ref_t ofc_str_ref_from_strz(const char* strz)
	{ return (ofc_str_ref_t){ strz, strlen(strz) }; }

bool    ofc_str_ref_empty(const ofc_str_ref_t ref);
uint8_t ofc_str_ref_hash(const ofc_str_ref_t ref);
uint8_t ofc_str_ref_hash_ci(const ofc_str_ref_t ref);
bool    ofc_str_ref_equal(const ofc_str_ref_t a, const ofc_str_ref_t b);
bool    ofc_str_ref_equal_ci(const ofc_str_ref_t a, const ofc_str_ref_t b);
bool    ofc_str_ref_equal_strz(const ofc_str_ref_t a, const char* b);
bool    ofc_str_ref_equal_strz_ci(const ofc_str_ref_t a, const char* b);
bool    ofc_str_ref_print(ofc_colstr_t* cs, const ofc_str_ref_t str_ref);

ofc_str_ref_t ofc_str_ref_bridge(ofc_str_ref_t start, ofc_str_ref_t end);

static inline uint8_t ofc_str_ref_ptr_hash(const ofc_str_ref_t* ref)
	{ return (ref ? ofc_str_ref_hash(*ref) : 0); }
static inline uint8_t ofc_str_ref_ptr_hash_ci(const ofc_str_ref_t* ref)
	{ return (ref ? ofc_str_ref_hash_ci(*ref) : 0); }
static inline bool ofc_str_ref_ptr_equal(const ofc_str_ref_t* a, const ofc_str_ref_t* b)
	{ if (!a || !b) return false; return ofc_str_ref_equal(*a, *b); }
static inline bool ofc_str_ref_ptr_equal_ci(const ofc_str_ref_t* a, const ofc_str_ref_t* b)
	{ if (!a || !b) return false; return ofc_str_ref_equal_ci(*a, *b); }

#endif
