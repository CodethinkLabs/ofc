#ifndef __ofc_sema_structure_h__
#define __ofc_sema_structure_h__

typedef struct
{
	bool is_vax;
	bool is_union;

	struct
	{
		unsigned                count;
		const ofc_sema_type_t** type;
		ofc_str_ref_t*          name;
	} member;

	bool locked;
} ofc_sema_structure_t;

ofc_sema_structure_t* ofc_sema_structure_create(bool is_vax);
ofc_sema_structure_t* ofc_sema_structure_create_union(void);

bool ofc_sema_structure_append(
	ofc_sema_structure_t*  structure,
	const ofc_sema_type_t* type, ofc_str_ref_t name);

void ofc_sema_structure_delete(ofc_sema_structure_t* structure);

uint8_t ofc_sema_structure_hash(
	const ofc_sema_structure_t* structure);

bool ofc_sema_structure_compare(
	const ofc_sema_structure_t* a,
	const ofc_sema_structure_t* b);

unsigned ofc_sema_structure_size(const ofc_sema_structure_t* structure);

#endif
