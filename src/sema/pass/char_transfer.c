/* Copyright 2016 Codethink Ltd.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "ofc/sema.h"


static bool ofc_sema_pass_char_transfer__expr(
	ofc_sema_expr_t* expr, void* param)
{
	(void)param;

	if (!expr)
		return false;

	if ((expr->type != OFC_SEMA_EXPR_CAST)
		|| !ofc_sema_expr_type_is_character(expr->cast.expr)
		|| ofc_sema_expr_type_is_character(expr))
		return true;

	const ofc_sema_type_t* type
		= ofc_sema_expr_type(expr->cast.expr);
	if (!type) return NULL;

	const ofc_sema_type_t* ctype
		= ofc_sema_expr_type(expr);
	if (!ctype) return false;

	ofc_sema_expr_t* mold = NULL;
	switch (ctype->type)
	{
		case OFC_SEMA_TYPE_BYTE:
			/* No safe way to create a BYTE mold. */
			break;

		case OFC_SEMA_TYPE_LOGICAL:
		{
			ofc_sema_typeval_t* tv
				= ofc_sema_typeval_create_logical(
					true, ctype->kind, OFC_SPARSE_REF_EMPTY);
			if (tv)
			{
				mold = ofc_sema_expr_typeval(tv);
				if (!mold) ofc_sema_typeval_delete(tv);
			}
			break;
		}

		case OFC_SEMA_TYPE_INTEGER:
			mold = ofc_sema_expr_integer(
				1, OFC_SEMA_KIND_DEFAULT);
			if (ctype->kind != OFC_SEMA_KIND_DEFAULT)
			{
				/* Try to use intrinsic casts to generate mold if possible. */
				ofc_sema_expr_t* nmold
					= ofc_sema_expr_cast_intrinsic(
						mold, ctype);
				if (!nmold) ofc_sema_expr_delete(mold);
				mold = nmold;

				if (!mold)
				{
					mold = ofc_sema_expr_integer(
						1, ctype->kind);
				}
			}
			break;

		case OFC_SEMA_TYPE_REAL:
		{
			ofc_sema_typeval_t* tv
				= ofc_sema_typeval_create_real(
					1.0, ctype->kind, OFC_SPARSE_REF_EMPTY);
			if (tv)
			{
				mold = ofc_sema_expr_typeval(tv);
				if (!mold) ofc_sema_typeval_delete(tv);
			}
			break;
		}

		case OFC_SEMA_TYPE_COMPLEX:
		{
			ofc_sema_typeval_t* tv
				= ofc_sema_typeval_create_complex(
					1.0, 0.0, ctype->kind, OFC_SPARSE_REF_EMPTY);
			if (tv)
			{
				mold = ofc_sema_expr_typeval(tv);
				if (!mold) ofc_sema_typeval_delete(tv);
			}
			break;
		}

		default:
			break;
	}

	if (!mold)
	{
		ofc_sparse_ref_warning(expr->src,
			"No way to convert this string cast to a TRANSFER"
			", as there's no safe MOLD for destination type");
		return true;
	}

	/* TODO - INTRINSIC - Find a neater way to resolve intrinsics. */
	const ofc_sema_intrinsic_t* intrinsic
		= ofc_sema_intrinsic(ofc_str_ref_from_strz("TRANSFER"), false);
	if (!intrinsic)
	{
		ofc_sema_expr_delete(mold);
		return false;
	}

	unsigned csize, size;
	if (!ofc_sema_type_size(ctype, &csize)
		|| !ofc_sema_type_size(type, &size))
		return false;

	/* Pad string to size of transfer type. */
	ofc_sema_expr_t* cast = expr->cast.expr;
	if (csize > size)
	{
		const ofc_sema_type_t* pad_type
			= ofc_sema_character_pad_to_type(type, ctype);
		if (!pad_type) return NULL;

		cast = ofc_sema_expr_cast(expr->cast.expr, pad_type);
		if (!cast) return NULL;
	}

	ofc_sema_expr_list_t* args
		= ofc_sema_expr_list_create();
	if (!args)
	{
		if (cast != expr->cast.expr)
		{
			cast->cast.expr = NULL;
			ofc_sema_expr_delete(cast);
		}
		ofc_sema_expr_delete(mold);
		return false;
	}

	if (!ofc_sema_expr_list_add(args, cast))
	{
		if (cast != expr->cast.expr)
		{
			cast->cast.expr = NULL;
			ofc_sema_expr_delete(cast);
		}

		ofc_sema_expr_list_delete(args);
		ofc_sema_expr_delete(mold);
		return false;
	}

	if (!ofc_sema_expr_list_add(args, mold))
	{
		/* We don't want to delete expr->cast.expr */
		args->expr[0] = NULL;

		if (cast != expr->cast.expr)
		{
			cast->cast.expr = NULL;
			ofc_sema_expr_delete(cast);
		}

		ofc_sema_expr_list_delete(args);
		ofc_sema_expr_delete(mold);
		return false;
	}

	/* TODO - PASS - Find a neater way to replace expressions. */
	ofc_sema_typeval_delete(expr->constant);

	expr->type = OFC_SEMA_EXPR_INTRINSIC;
	expr->constant = ofc_sema_intrinsic_constant(
		intrinsic, expr->args);

	expr->label = NULL;
	expr->brackets = false;
	expr->repeat = 0;

	expr->is_alt_return = false;
	expr->is_label = false;
	expr->is_format = false;

	expr->intrinsic = intrinsic;
	expr->args = args;

	return true;
}

static bool ofc_sema_pass_char_transfer__scope(
	ofc_sema_scope_t* scope, void* param)
{
	(void)param;

	if (!scope)
		return false;

	return ofc_sema_scope_foreach_expr(
		scope, NULL, ofc_sema_pass_char_transfer__expr);
}

bool ofc_sema_pass_char_transfer(
	ofc_sema_scope_t* scope)
{
	if (!scope)
		return false;

	return ofc_sema_scope_foreach_scope(
		scope, NULL, ofc_sema_pass_char_transfer__scope);
}
