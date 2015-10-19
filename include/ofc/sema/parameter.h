#ifndef __ofc_sema_parameter_h__
#define __ofc_sema_parameter_h__

typedef struct
{
	ofc_str_ref_t       name;
	ofc_sema_typeval_t* typeval;
} ofc_sema_parameter_t;

bool ofc_sema_parameter(
	const ofc_parse_stmt_t* stmt,
	ofc_hashmap_t* map);
bool ofc_sema_parameter_decl(
	const ofc_parse_stmt_t* stmt,
	ofc_hashmap_t* map);
void ofc_sema_parameter_delete(
	ofc_sema_parameter_t* parameter);

bool ofc_sema_parameter_int32(
	const ofc_sema_parameter_t* parameter,
	int32_t* value);

#endif
