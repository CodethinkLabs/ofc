/* Copyright 2015 Codethink Ltd.
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
#include "ofc/global_opts.h"

extern ofc_global_opts_t global_opts;


bool ofc_sema_stmt_equivalence(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_EQUIVALENCE))
		return false;

	if (stmt->label != 0)
	{
		ofc_sparse_ref_warning(stmt->src,
			"EQUIVALENCE statements can't be labelled, ignoring.");
	}

	unsigned g;
	for (g = 0; g < stmt->equivalence.count; g++)
	{
		const ofc_parse_lhs_list_t* list
			= stmt->equivalence.group[g];
		if (!list || (list->count == 0))
			continue;

		ofc_sema_lhs_t* base
			= ofc_sema_lhs(scope, list->lhs[0]);
		if (!base)
		{
			ofc_sparse_ref_error(list->lhs[0]->src,
				"Invalid EQUIVALENCE element");
			return false;
		}

		/* TODO - Don't mark EQUIVALENCE as used? */
		if (!ofc_sema_lhs_mark_used(
			base, true, true))
		{
			ofc_sema_lhs_delete(base);
			return false;
		}

		if (list->count < 2)
		{
			ofc_sparse_ref_warning(base->src,
				"EQUIVALENCE groups should contain more than 1 entry.");
			ofc_sema_lhs_delete(base);
			continue;
		}

		ofc_sema_equiv_t* equiv
			= ofc_sema_equiv_create();
		if (!equiv)
		{
			ofc_sema_lhs_delete(base);
			return false;
		}

		if (!ofc_sema_equiv_add(equiv, base))
		{
			ofc_sema_equiv_delete(equiv);
			ofc_sema_lhs_delete(base);
			return false;
		}

		unsigned i;
		for (i = 1; i < list->count; i++)
		{
			ofc_sema_lhs_t* elhs
				= ofc_sema_lhs(scope, list->lhs[i]);
			if (!elhs)
			{
				ofc_sparse_ref_error(list->lhs[i]->src,
					"Invalid EQUIVALENCE element");
				ofc_sema_equiv_delete(equiv);
				return false;
			}

			/* TODO - Don't mark EQUIVALENCE as used? */
			if (!ofc_sema_lhs_mark_used(
				elhs, true, true))
			{
				ofc_sema_lhs_delete(elhs);
				ofc_sema_equiv_delete(equiv);
				return false;
			}

			if (!ofc_sema_type_compatible(
				ofc_sema_lhs_type(base),
				ofc_sema_lhs_type(elhs)))
			{
				if (!global_opts.no_warn_equiv_type)
				{
					ofc_sparse_ref_warning(elhs->src,
						"EQUIVALENCE types don't match.");
				}
			}

			if (!ofc_sema_equiv_add(equiv, elhs))
			{
				/* TODO - Better error messages for this in 'sema/equiv.c'. */
				ofc_sparse_ref_warning(base->src,
					"EQUIVALENCE statement causes collision.");

				ofc_sema_lhs_delete(elhs);
				ofc_sema_equiv_delete(equiv);
				return false;
			}
		}

		if (!ofc_sema_scope_equiv_add(scope, equiv))
		{
			ofc_sema_equiv_delete(equiv);
			return false;
		}
	}

	return true;
}
