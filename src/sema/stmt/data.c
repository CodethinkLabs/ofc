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

static bool ofc_sema_stmt__data(
	ofc_sema_scope_t* scope,
	const ofc_parse_lhs_list_t*  nlist,
	const ofc_parse_expr_list_t* clist)
{
	ofc_sema_lhs_list_t* lhs_list
		= ofc_sema_input_list(scope, nlist, NULL);
	ofc_sema_expr_list_t* expr_list
		= ofc_sema_expr_list_clist(scope, clist);

	unsigned lhs_count;
	unsigned expr_count;
	if (!ofc_sema_lhs_list_elem_count(lhs_list, &lhs_count)
		|| !ofc_sema_expr_list_elem_count(expr_list, &expr_count))
	{
		ofc_sema_expr_list_delete(expr_list);
		ofc_sema_lhs_list_delete(lhs_list);
		return false;
	}

	if (expr_count < lhs_count)
	{
		ofc_sparse_ref_warning(nlist->lhs[0]->src,
			"Not enough initializer elements in DATA statement");
	}
	else if (expr_count > lhs_count)
	{
		ofc_sparse_ref_warning(clist->expr[0]->src,
			"Too many initializer elements in DATA statement, ignoring excess");
	}

	bool success = ofc_sema_lhs_list_init(
		lhs_list, expr_list);
	ofc_sema_expr_list_delete(expr_list);
	ofc_sema_lhs_list_delete(lhs_list);

	/* TODO - Better errors if initialization fails. */
	return success;
}


bool ofc_sema_stmt_data(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_DATA)
		|| !stmt->data)
		return false;

	unsigned j;
	for (j = 0; j < stmt->data->count; j++)
	{
		const ofc_parse_data_entry_t* entry
			= stmt->data->entry[j];
		if (!entry) continue;

		if (!ofc_sema_stmt__data(scope,
			entry->nlist, entry->clist))
			return false;
	}

	return true;
}
