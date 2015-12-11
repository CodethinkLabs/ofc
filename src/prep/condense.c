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

#include <stdlib.h>

#include "ofc/fctype.h"
#include "ofc/prep.h"


ofc_sparse_t* ofc_prep_condense(ofc_sparse_t* unformat)
{
	const char* src = ofc_sparse_strz(unformat);
	if (!src) return NULL;

	ofc_sparse_t* condense
		= ofc_sparse_create_child(unformat);
	if (!condense) return NULL;

	unsigned i = 0;
	while (src[i] != '\0')
	{
		/* Skip whitespace. */
		for (; (src[i] != '\0') && ofc_is_hspace(src[i]); i++);

		if (src[i] == '\0')
			break;

		/* Parse non-whitespace. */
		const char* base = &src[i];
		unsigned size;
		for(size = 0; (src[i] != '\0') && !ofc_is_hspace(src[i]); size++, i++);

		/* Append non-whitespace to condense sparse. */
		if (!ofc_sparse_append_strn(condense, base, size))
		{
			ofc_sparse_delete(condense);
			return NULL;
		}
	}

	ofc_sparse_lock(condense);
	return condense;
}
