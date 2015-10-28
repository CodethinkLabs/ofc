#ifndef __ofc_sema_stmt_h__
#define __ofc_sema_stmt_h__

typedef enum
{
	OFC_SEMA_STMT_ASSIGNMENT = 0,
	OFC_SEMA_STMT_WRITE,
	OFC_SEMA_STMT_CONTINUE,
	OFC_SEMA_STMT_IF_COMPUTED,
	OFC_SEMA_STMT_IF_STATEMENT,
	OFC_SEMA_STMT_IF_THEN,
	OFC_SEMA_STMT_STOP,
	OFC_SEMA_STMT_PAUSE,
	OFC_SEMA_STMT_GO_TO,
} ofc_sema_stmt_e;

struct ofc_sema_stmt_s
{
	ofc_sema_stmt_e type;

	union
	{
		struct
		{
			const ofc_sema_decl_t* dest;
			ofc_sema_expr_t*       expr;
		} assignment;

		struct
		{
			ofc_sema_expr_t*       cond;
			ofc_sema_expr_list_t*  label;
		} if_comp;

		struct
		{
			ofc_sema_expr_t* cond;
			ofc_sema_stmt_t* stmt;
		} if_stmt;

		struct
		{
			ofc_sema_expr_t* cond;
			ofc_sema_scope_t* scope_then;
			ofc_sema_scope_t* scope_else;
		} if_then;

		struct
		{
			ofc_sema_expr_t* unit;
			bool             stdout;

			/* TODO - Namelist. */
			ofc_sema_expr_t*   format_expr;
			ofc_sema_format_t* format;
			bool               format_ldio;

			ofc_sema_expr_t* iostat;
			ofc_sema_expr_t* rec;
			ofc_sema_expr_t* err;
		} io_write;

		struct
		{
			ofc_sema_expr_t* str;
		} stop_pause;

		struct
		{
			ofc_sema_expr_t* label;
		} go_to;
	};
};


ofc_sema_stmt_t* ofc_sema_stmt(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
bool ofc_sema_stmt_scoped(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);

ofc_sema_stmt_t* ofc_sema_stmt_alloc(
	ofc_sema_stmt_t stmt);
void ofc_sema_stmt_delete(
	ofc_sema_stmt_t* stmt);

ofc_sema_stmt_t* ofc_sema_stmt_assignment(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
ofc_sema_stmt_t* ofc_sema_stmt_io_write(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
ofc_sema_stmt_t* ofc_sema_stmt_if(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
ofc_sema_stmt_t* ofc_sema_stmt_stop_pause(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
ofc_sema_stmt_t* ofc_sema_stmt_go_to(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);


struct ofc_sema_stmt_list_s
{
	unsigned          count;
	ofc_sema_stmt_t** stmt;
};


ofc_sema_stmt_list_t* ofc_sema_stmt_list_create(void);

void ofc_sema_stmt_list_delete(
	ofc_sema_stmt_list_t* list);

bool ofc_sema_stmt_list_add(
	ofc_sema_stmt_list_t* list,
	ofc_sema_stmt_t* stmt);

unsigned ofc_sema_stmt_list_count(
	const ofc_sema_stmt_list_t* list);

#endif
