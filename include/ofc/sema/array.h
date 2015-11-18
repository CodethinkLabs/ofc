#ifndef __ofc_sema_array_h__
#define __ofc_sema_array_h__


typedef struct
{
	int      base;
	unsigned count;
} ofc_sema_array_dims_t;

typedef struct
{
	unsigned              dimensions;
	ofc_sema_array_dims_t segment[0];
} ofc_sema_array_t;

ofc_sema_array_t* ofc_sema_array(
	ofc_sema_scope_t*              scope,
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

bool ofc_sema_array_index_print(ofc_colstr_t* cs,
	const ofc_sema_array_index_t* index);

ofc_sema_array_index_t* ofc_sema_array_index(
	ofc_sema_scope_t*              scope,
	const ofc_sema_array_t*        array,
	const ofc_parse_array_index_t* index);
void ofc_sema_array_index_delete(
	ofc_sema_array_index_t* index);

bool ofc_sema_array_index_offset(
	const ofc_sema_scope_t*       scope,
	const ofc_sema_decl_t*        decl,
	const ofc_sema_array_index_t* index,
	unsigned* offset);

bool ofc_sema_array_index_compare(
	const ofc_sema_array_index_t* a,
	const ofc_sema_array_index_t* b);


typedef struct
{
	ofc_sema_expr_t* index;

	int              base;
	unsigned         count;
	unsigned         stride;
} ofc_sema_array_segment_t;

typedef struct
{
	unsigned                 dimensions;
	ofc_sema_array_segment_t segment[0];
} ofc_sema_array_slice_t;

ofc_sema_array_slice_t* ofc_sema_array_slice(
	ofc_sema_scope_t*              scope,
	const ofc_sema_array_t*        array,
	const ofc_parse_array_index_t* index);
void ofc_sema_array_slice_delete(
	ofc_sema_array_slice_t* slice);

bool ofc_sema_array_slice_compare(
	const ofc_sema_array_slice_t* a,
	const ofc_sema_array_slice_t* b);

ofc_sema_array_t* ofc_sema_array_slice_dims(
	const ofc_sema_array_slice_t* slice);

#endif
