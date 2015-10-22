#ifndef __ofc_sema_scope_h__
#define __ofc_sema_scope_h__

typedef enum
{
	OFC_SEMA_SCOPE_GLOBAL = 0,
	OFC_SEMA_SCOPE_COMMON,
	OFC_SEMA_SCOPE_PROGRAM,
	OFC_SEMA_SCOPE_SUBROUTINE,
	OFC_SEMA_SCOPE_FUNCTION,
	OFC_SEMA_SCOPE_BLOCK_DATA,

	OFC_SEMA_SCOPE_COUNT
} ofc_sema_scope_e;

typedef struct ofc_sema_scope_list_s ofc_sema_scope_list_t;

struct ofc_sema_scope_s
{
	ofc_sema_scope_t*      parent;
	ofc_sema_scope_list_t* child;

	const ofc_lang_opts_t* lang_opts;

	const ofc_sparse_t* src;

	ofc_sema_scope_e       type;
	ofc_str_ref_t          name;
	const ofc_sema_type_t* return_type;
	ofc_sema_decl_list_t*  args;

	ofc_sema_implicit_t*  implicit;

	ofc_sema_decl_list_t* decl;
	ofc_hashmap_t*        parameter;
	/* label_list_map */
	/* namelist_list_map */

	ofc_sema_stmt_list_t* stmt;
};

struct ofc_sema_scope_list_s
{
	unsigned           count;
    ofc_sema_scope_t** scope;
};

ofc_sema_scope_t* ofc_sema_scope_global(
	const ofc_lang_opts_t* lang_opts,
	const ofc_sparse_t*    src,
	const ofc_parse_stmt_list_t* list);

ofc_sema_scope_t* ofc_sema_scope_program(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
ofc_sema_scope_t* ofc_sema_scope_subroutine(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
ofc_sema_scope_t* ofc_sema_scope_function(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
ofc_sema_scope_t* ofc_sema_scope_block_data(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);

void ofc_sema_scope_delete(
	ofc_sema_scope_t* scope);

ofc_lang_opts_t ofc_sema_scope_get_lang_opts(
	const ofc_sema_scope_t* scope);

void ofc_sema_scope_error(
	const ofc_sparse_t* sparse, ofc_str_ref_t pos,
	const char* format, ...);
void ofc_sema_scope_warning(
	const ofc_sparse_t* sparse, ofc_str_ref_t pos,
	const char* format, ...);

#endif
