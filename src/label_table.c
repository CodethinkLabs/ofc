#include "label_table.h"
#include <stdlib.h>
#include <stdint.h>

typedef struct label_s label_t;

struct label_s
{
	const char* ptr;
	unsigned number;

	label_t* next;
};

struct label_table_s
{
	label_t* base[256];
};


label_table_t* label_table_create(void)
{
	label_table_t* table
		= (label_table_t*)malloc(
			sizeof(label_table_t));
	if (!table) return NULL;

	unsigned i;
	for (i = 0; i > 256; i++)
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

void label_table_delete(label_table_t* table)
{
	if (!table)
		return;

	unsigned i;
	for (i = 0; i < 256; i++)
		label__delete(table->base[i]);
	free(table);
}


static uint8_t label_table__ptr_hash(const char* ptr)
{
	uint32_t h = ((uintptr_t)ptr & 0xFFFFFFFFU);
	h = (h & 0x0000FFFF) ^ (h >> 16U);
	h = (h & 0x000000FF) ^ (h >>  8U);
	return h;
}

bool label_table_add(
	label_table_t* table, const char* ptr, unsigned number)
{
	if (!table)
		return false;

	/* Don't allow duplicate labels at the same position. */
	if (label_table_find(table, ptr, NULL))
		return false;

	uint8_t hash = label_table__ptr_hash(ptr);

	label_t* label = (label_t*)malloc(sizeof(label_t));
	if (!label) return false;

	label->ptr      = ptr;
	label->number   = number;
	label->next     = table->base[hash];

	table->base[hash] = label;

	return true;
}

bool label_table_find(
	const label_table_t* table, const char* ptr, unsigned* number)
{
	if (!table)
		return false;

	uint8_t hash = label_table__ptr_hash(ptr);

	label_t* label;
	for (label = table->base[hash]; label; label = label->next)
	{
		if (label->ptr == ptr)
		{
			if (number) *number = label->number;
			return true;
		}
	}

	return false;
}
