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

#include "ofc/fctype.h"

bool ofc_is_vspace(char c)
{
	return ((c == '\r') || (c == '\n'));
}

bool ofc_is_hspace(char c)
{
	switch (c)
	{
		case ' ':
		case '\t':
		case '\f':
		case '\v':
			return true;
		default:
			break;
	}
	return false;
}

bool ofc_is_ident(char c)
{
	return (isalnum(c)
		|| (c == '_')
		|| (c == '$'));
}

bool ofc_is_end_statement(const char* c, unsigned* len)
{
	if (ofc_is_vspace(c[0]) || (c[0] == ';'))
	{
		if (len) *len = 1;
		return true;
	}

	if (len) *len = 0;
	return (c[0] == '\0');
}
