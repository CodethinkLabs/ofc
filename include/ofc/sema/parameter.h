#ifndef __ofc_sema_parameter_h__
#define __ofc_sema_parameter_h__

typedef struct
{
	ofc_str_ref_t       name;
	ofc_sema_typeval_t* typeval;
} ofc_sema_parameter_t;

ofc_hashmap_t* ofc_sema_parameter_map_create(
	bool case_sensitive);

bool ofc_sema_parameter(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
bool ofc_sema_parameter_decl(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
void ofc_sema_parameter_delete(
	ofc_sema_parameter_t* parameter);

const ofc_str_ref_t* ofc_sema_parameter_name(
	const ofc_sema_parameter_t* parameter);

const ofc_sema_type_t* ofc_sema_parameter_type(
	const ofc_sema_parameter_t* parameter);

const ofc_sema_typeval_t* ofc_sema_parameter_get(
	const ofc_sema_parameter_t* parameter);

bool ofc_sema_parameter_int32(
	const ofc_sema_parameter_t* parameter,
	int32_t* value);

#endif
