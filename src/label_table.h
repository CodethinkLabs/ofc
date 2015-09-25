#ifndef __label_table_h__
#define __label_table_h__

#include <stdbool.h>

typedef struct label_table_s label_table_t;

label_table_t* label_table_create(void);
void           label_table_delete(label_table_t* table);

bool label_table_add(
	label_table_t* table, unsigned offset, unsigned number);
bool label_table_find(
	const label_table_t* table, unsigned offset, unsigned* number);

#endif
