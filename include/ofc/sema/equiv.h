#ifndef __ofc_sema_equiv_h__
#define __ofc_sema_equiv_h__

typedef struct
{
	unsigned         count;
	ofc_sema_lhs_t** lhs;
	ofc_hashmap_t*   map;

	unsigned refcnt;
} ofc_sema_equiv_t;

bool ofc_sema_equiv(
	ofc_sema_lhs_t* a,
	ofc_sema_lhs_t* b);

void ofc_sema_equiv_delete(
	ofc_sema_equiv_t* equiv);

#endif
