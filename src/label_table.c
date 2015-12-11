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

#include <stdint.h>
#include <stdlib.h>

#include "ofc/label_table.h"

typedef struct label_s label_t;

struct label_s
{
	unsigned offset;
	unsigned number;

	label_t* next;
};

struct ofc_label_table_s
{
	label_t* base[256];
};


ofc_label_table_t* ofc_label_table_create(void)
{
	ofc_label_table_t* table
		= (ofc_label_table_t*)malloc(
			sizeof(ofc_label_table_t));
	if (!table) return NULL;

	unsigned i;
	for (i = 0; i < 256; i++)
		table->base[i] = NULL;
	return table;
}

static void label__delete(label_t* label)
{
	if (!label)
		return;
	label__delete(label->next);
	free(label);
}

void ofc_label_table_delete(ofc_label_table_t* table)
{
	if (!table)
		return;

	unsigned i;
	for (i = 0; i < 256; i++)
		label__delete(table->base[i]);
	free(table);
}


static uint8_t ofc_label_table__offset_hash(unsigned offset)
{
	uint32_t h = (offset & 0xFFFFFFFFU);
	h = (h & 0x0000FFFF) ^ (h >> 16U);
	h = (h & 0x000000FF) ^ (h >>  8U);
	return h;
}

bool ofc_label_table_add(
	ofc_label_table_t* table, unsigned offset, unsigned number)
{
	if (!table)
		return false;

	/* Don't allow duplicate labels at the same position. */
	if (ofc_label_table_find(table, offset, NULL))
		return false;

	uint8_t hash = ofc_label_table__offset_hash(offset);

	label_t* label = (label_t*)malloc(sizeof(label_t));
	if (!label) return false;

	label->offset = offset;
	label->number = number;
	label->next   = table->base[hash];

	table->base[hash] = label;

	return true;
}

bool ofc_label_table_find(
	const ofc_label_table_t* table, unsigned offset, unsigned* number)
{
	if (!table)
		return false;

	uint8_t hash = ofc_label_table__offset_hash(offset);

	label_t* label;
	for (label = table->base[hash]; label; label = label->next)
	{
		if (label->offset == offset)
		{
			if (number) *number = label->number;
			return true;
		}
	}

	return false;
}
