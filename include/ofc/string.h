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

#ifndef __ofc_string_h__
#define __ofc_string_h__

#include <stdbool.h>

typedef struct
{
	char*    base;
	unsigned size;
} ofc_string_t;

ofc_string_t* ofc_string_create(const char* base, unsigned size);
ofc_string_t* ofc_string_copy(const ofc_string_t* src);
void          ofc_string_delete(ofc_string_t* string);

bool ofc_string_empty(const ofc_string_t* string);

/* May return truncated string if string contains nul characters. */
const char* ofc_string_strz(const ofc_string_t* string);

unsigned ofc_string_length(const ofc_string_t* string);
bool     ofc_string_equal(const ofc_string_t a, const ofc_string_t b);

#endif
