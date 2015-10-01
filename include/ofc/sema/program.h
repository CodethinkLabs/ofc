#ifndef __ofc_sema_program_h__
#define __ofc_sema_program_h__

typedef struct
{
	ofc_str_ref_t name;
} ofc_sema_program_t;

ofc_sema_program_t* ofc_sema_program(
	const ofc_parse_stmt_t* stmt);
void ofc_sema_program_delete(
	ofc_sema_program_t* program);

#endif
