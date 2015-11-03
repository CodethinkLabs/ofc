#ifndef __ofc_sema_expr_h__
#define __ofc_sema_expr_h__

typedef enum
{
	OFC_SEMA_EXPR_CONSTANT = 0,
	OFC_SEMA_EXPR_LHS,
	OFC_SEMA_EXPR_CAST,
	OFC_SEMA_EXPR_INTRINSIC,

	OFC_SEMA_EXPR_POWER,
	OFC_SEMA_EXPR_MULTIPLY,
	OFC_SEMA_EXPR_CONCAT,
	OFC_SEMA_EXPR_DIVIDE,
	OFC_SEMA_EXPR_ADD,
	OFC_SEMA_EXPR_SUBTRACT,
	OFC_SEMA_EXPR_NEGATE,
	OFC_SEMA_EXPR_EQ,
	OFC_SEMA_EXPR_NE,
	OFC_SEMA_EXPR_LT,
	OFC_SEMA_EXPR_LE,
	OFC_SEMA_EXPR_GT,
	OFC_SEMA_EXPR_GE,
	OFC_SEMA_EXPR_NOT,
	OFC_SEMA_EXPR_AND,
	OFC_SEMA_EXPR_OR,
	OFC_SEMA_EXPR_EQV,
	OFC_SEMA_EXPR_NEQV,

	OFC_SEMA_EXPR_COUNT
} ofc_sema_expr_e;

struct ofc_sema_expr_s
{
	ofc_sema_expr_e type;

	ofc_str_ref_t src;

	union
	{
		ofc_sema_typeval_t* constant;

		ofc_sema_lhs_t* lhs;

		struct
		{
			const ofc_sema_type_t* type;
			ofc_sema_expr_t*       expr;
		} cast;

		struct
		{
			ofc_sema_expr_t* a;
			ofc_sema_expr_t* b;
		};
	};
};

struct ofc_sema_expr_list_s
{
	unsigned           count;
	ofc_sema_expr_t** expr;
};

ofc_sema_expr_t* ofc_sema_expr(
	const ofc_sema_scope_t* scope,
	const ofc_parse_expr_t* expr);
ofc_sema_expr_t* ofc_sema_expr_cast(
	const ofc_sema_scope_t* scope,
	ofc_sema_expr_t* expr,
	const ofc_sema_type_t* type);
void ofc_sema_expr_delete(
	ofc_sema_expr_t* expr);

bool ofc_sema_expr_compare(
	const ofc_sema_expr_t* a,
	const ofc_sema_expr_t* b);

uint8_t ofc_sema_expr_hash(
	const ofc_sema_expr_t* expr);

const ofc_sema_type_t* ofc_sema_expr_type(
	const ofc_sema_expr_t* expr);

ofc_sema_typeval_t* ofc_sema_expr_resolve(
	const ofc_sema_scope_t* scope,
	const ofc_sema_expr_t* expr);

const ofc_sema_typeval_t* ofc_sema_expr_constant(
	const ofc_sema_expr_t* expr);
bool ofc_sema_expr_is_constant(
	const ofc_sema_expr_t* expr);

bool ofc_sema_expr_validate_uint(
	const ofc_sema_expr_t* expr);

bool ofc_sema_expr_resolve_uint(
	const ofc_sema_expr_t* expr,
	unsigned* value);

ofc_sema_expr_list_t* ofc_sema_expr_list_create(void);
void ofc_sema_expr_list_delete(
	ofc_sema_expr_list_t* list);
bool ofc_sema_expr_list_add(
	ofc_sema_expr_list_t* list,
	ofc_sema_expr_t* expr);
unsigned ofc_sema_expr_list_count(
	const ofc_sema_expr_list_t* list);

#endif
