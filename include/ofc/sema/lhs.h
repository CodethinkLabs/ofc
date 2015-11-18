#ifndef __ofc_sema_lhs_h__
#define __ofc_sema_lhs_h__

typedef enum
{
	OFC_SEMA_LHS_DECL,
	OFC_SEMA_LHS_ARRAY_INDEX,
	OFC_SEMA_LHS_ARRAY_SLICE,
	OFC_SEMA_LHS_STRUCTURE_MEMBER,
} ofc_sema_lhs_e;

struct ofc_sema_lhs_s
{
	ofc_sema_lhs_e type;

	union
	{
		ofc_sema_decl_t* decl;

		struct
		{
			ofc_sema_lhs_t* parent;

			union
			{
				ofc_sema_array_t*       slice;
				ofc_sema_array_index_t* index;
				ofc_str_ref_t           member;
			};
		};
	};

	const ofc_sema_type_t* data_type;

	unsigned refcnt;
};

const ofc_sema_type_t* ofc_sema_lhs_decl_type(
	ofc_sema_scope_t* scope,
	const ofc_sema_type_t* type,
	ofc_parse_lhs_t* lhs);

ofc_sema_lhs_t* ofc_sema_lhs(
	ofc_sema_scope_t* scope,
	const ofc_parse_lhs_t* lhs);
ofc_sema_lhs_t* ofc_sema_lhs_expr(
	ofc_sema_scope_t* scope,
	const ofc_parse_lhs_t* lhs);
bool ofc_sema_lhs_reference(
	ofc_sema_lhs_t* lhs);
void ofc_sema_lhs_delete(
	ofc_sema_lhs_t* lhs);

bool ofc_sema_lhs_init(
	const ofc_sema_scope_t* scope,
	ofc_sema_lhs_t* lhs,
	const ofc_sema_expr_t* init);

bool ofc_sema_lhs_compare(
	const ofc_sema_lhs_t* a,
	const ofc_sema_lhs_t* b);

ofc_sema_decl_t* ofc_sema_lhs_decl(
	ofc_sema_lhs_t* lhs);
const ofc_sema_type_t* ofc_sema_lhs_type(
	const ofc_sema_lhs_t* lhs);

bool ofc_sema_lhs_equiv(
	ofc_sema_lhs_t* a,
	ofc_sema_lhs_t* b);

#endif
