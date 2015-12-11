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

#include "ofc/parse.h"


unsigned ofc_parse_star_len(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_expr_t** count, bool* is_variable)
{
	unsigned i = 0;

	if (ptr[i++] != '*')
		return 0;

	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned l;
	ofc_parse_expr_t* expr;
	if (ptr[i] == '(')
	{
		i += 1;

		expr = ofc_parse_expr(
			src, &ptr[i], debug, &l);
		if (!expr)
		{
			if (ptr[i] != '*')
				return 0;
			l = 1;
		}
		i += l;

		if (ptr[i++] != ')')
		{
			ofc_parse_expr_delete(expr);
			ofc_parse_debug_rewind(debug, dpos);
			return 0;
		}
	}
	else
	{
		expr = ofc_parse_expr_integer(
			src, &ptr[i], debug, &l);
		if (!expr) return 0;
		i += l;
		*count = expr;
		*is_variable = false;
	}

	*count = expr;
	*is_variable = (expr == NULL);
	return i;
}
