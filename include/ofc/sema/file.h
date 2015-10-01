#ifndef __ofc_sema_file_h__
#define __ofc_sema_file_h__

typedef struct
{
	ofc_sema_program_t* program;
} ofc_sema_file_t;

ofc_sema_file_t* ofc_sema_file(const ofc_parse_stmt_list_t* tree);
void ofc_sema_file_delete(ofc_sema_file_t* file);

#endif
