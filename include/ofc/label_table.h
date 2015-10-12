#ifndef __ofc_label_table_h__
#define __ofc_label_table_h__

#include <stdbool.h>

typedef struct ofc_label_table_s ofc_label_table_t;

ofc_label_table_t* ofc_label_table_create(void);
void               ofc_label_table_delete(ofc_label_table_t* table);

bool ofc_label_table_add(
	ofc_label_table_t* table, unsigned offset, unsigned number);
bool ofc_label_table_find(
	const ofc_label_table_t* table, unsigned offset, unsigned* number);

#endif
