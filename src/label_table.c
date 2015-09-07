#include "label_table.h"
#include <stdlib.h>
#include <stdint.h>

typedef struct label_s label_t;

struct label_s
{
	unsigned position;
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


static uint8_t label_table__position_hash(unsigned position)
{
	position = ((position >> 16) ^ (position & 0xFFFF));
	position = ((position >>  8) ^ (position & 0x00FF));
	return position;
}

bool label_table_add(
	label_table_t* table, unsigned position, unsigned number)
{
	if (!table)
		return false;

	/* Don't allow duplicate labels in the same position. */
	if (label_table_find(table, position, NULL))
		return false;

	uint8_t hash = label_table__position_hash(position);

	label_t* label = (label_t*)malloc(sizeof(label_t));
	if (!label) return false;

	label->position = position;
	label->number   = number;
	label->next     = table->base[hash];

	table->base[hash] = label;

	return true;
}

bool label_table_find(
	const label_table_t* table, unsigned position, unsigned* number)
{
	if (!table)
		return false;

	uint8_t hash = label_table__position_hash(position);

	label_t* label;
	for (label = table->base[hash]; label; label = label->next)
	{
		if (label->position == position)
		{
			if (number) *number = label->number;
			return true;
		}
	}

	return false;
}
