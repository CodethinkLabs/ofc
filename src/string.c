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

#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ofc/string.h"

ofc_string_t* ofc_string_create(const char* base, unsigned size)
{
	ofc_string_t* string
		= (ofc_string_t*)malloc(
			sizeof(ofc_string_t));
	if (!string)
		return NULL;

	string->base = (size == 0 ? NULL : (char*)malloc(size + 1));
	string->size = (string->base ? size : 0);
	if (string->base)
	{
		if (base)
		{
			memcpy(string->base, base, size);
			string->base[size] = '\0';
		}
		else
		{
			memset(string->base, '\0', (size + 1));
		}
	}

	return string;
}

ofc_string_t* ofc_string_copy(const ofc_string_t* src)
{
	return ofc_string_create(
		src->base, src->size);
}

void ofc_string_delete(ofc_string_t* string)
{
	if (!string)
		return;

	free(string->base);
	free(string);
}


bool ofc_string_empty(const ofc_string_t* string)
{
	return (!string->base || (string->size == 0));
}


const char* ofc_string_strz(const ofc_string_t* string)
{
	if (!string)
		return NULL;
	return string->base;
}

unsigned ofc_string_length(const ofc_string_t* string)
{
	return (string->base ? string->size : 0);
}

bool ofc_string_equal(const ofc_string_t a, const ofc_string_t b)
{
	if (a.size != b.size)
		return false;

	if (a.base == b.base)
		return true;

	return (memcmp(a.base, b.base, a.size) == 0);
}
