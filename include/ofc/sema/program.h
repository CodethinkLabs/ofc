#ifndef __ofc_sema_program_h__
#define __ofc_sema_program_h__

#include <ofc/sema/implicit.h>


typedef struct
{
	ofc_str_ref_t name;

	ofc_sema_implicit_t* implicit;

	/* decl_list_map */
	/* type_list_map */
	/* label_list_map */
	/* namelist_list_map */

	/* stmt_list */
} ofc_sema_program_t;

ofc_sema_program_t* ofc_sema_program(
	const ofc_parse_stmt_t* stmt);
void ofc_sema_program_delete(
	ofc_sema_program_t* program);

#endif
