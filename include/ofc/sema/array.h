#ifndef __ofc_sema_array_h__
#define __ofc_sema_array_h__

typedef struct
{
	ofc_sema_expr_t* base;
	unsigned         count;
	unsigned         stride;
} ofc_sema_array_slice_t;

typedef struct
{
	unsigned               dimensions;
	ofc_sema_array_slice_t slice[0];
} ofc_sema_array_t;

ofc_sema_array_t* ofc_sema_array(
	const ofc_sema_scope_t*        scope,
	const ofc_sema_array_t*        array,
	const ofc_parse_array_index_t* index);

ofc_sema_array_t* ofc_sema_array_copy(
	const ofc_sema_array_t* array);

void ofc_sema_array_delete(ofc_sema_array_t* array);

uint8_t ofc_sema_array_hash(
	const ofc_sema_array_t* array);
bool ofc_sema_array_compare(
	const ofc_sema_array_t* a,
	const ofc_sema_array_t* b);

unsigned ofc_sema_array_total(const ofc_sema_array_t* array);


typedef struct
{
	unsigned         dimensions;
	ofc_sema_expr_t* index[0];
} ofc_sema_array_index_t;

ofc_sema_array_index_t* ofc_sema_array_index(
	const ofc_sema_scope_t*        scope,
	const ofc_sema_array_t*        array,
	const ofc_parse_array_index_t* index);
void ofc_sema_array_index_delete(
	ofc_sema_array_index_t* index);

bool ofc_sema_array_index_compare(
	const ofc_sema_array_index_t* a,
	const ofc_sema_array_index_t* b);

#endif
