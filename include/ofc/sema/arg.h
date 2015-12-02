#ifndef __ofc_sema_arg_h__
#define __ofc_sema_arg_h__

typedef struct
{
	bool          alt_return;
	ofc_str_ref_t name;
} ofc_sema_arg_t;

typedef struct
{
	unsigned        count;
	ofc_sema_arg_t* arg;
} ofc_sema_arg_list_t;


ofc_sema_arg_list_t* ofc_sema_arg_list(
	const ofc_sema_scope_t* scope,
	const ofc_parse_call_arg_list_t* plist);
ofc_sema_arg_list_t* ofc_sema_arg_list_stmt_func(
	const ofc_sema_scope_t* scope,
	const ofc_parse_array_index_t* index);
void ofc_sema_arg_list_delete(
	ofc_sema_arg_list_t* list);

#endif
