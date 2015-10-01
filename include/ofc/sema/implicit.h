#ifndef __ofc_sema_implicit_h__
#define __ofc_sema_implicit_h__

typedef struct
{
	const ofc_sema_type_t* type[26];
} ofc_sema_implicit_t;

ofc_sema_implicit_t*   ofc_sema_implicit_create(void);
bool                   ofc_sema_implicit_none(ofc_sema_implicit_t* implicit);
bool                   ofc_sema_implicit_set(ofc_sema_implicit_t* implicit, const ofc_sema_type_t* type, char c);
const ofc_sema_type_t* ofc_sema_implicit_get(const ofc_sema_implicit_t* implicit, char c);
void                   ofc_sema_implicit_delete(ofc_sema_implicit_t* implicit);

#endif
