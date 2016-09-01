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

#include <string.h>

#include "ofc/parse.h"

static const char* ofc_parse_operator__name[] =
{
		"**",
		"*",
		"//",
		"/",
		"+",
		"-",
		".EQ.",
		".NE.",
		".LT.",
		".LE.",
		".GT.",
		".GE.",
		".NOT.",
		".AND.",
		".OR.",
		".XOR.",
		".EQV.",
		".NEQV.",
};

const char* ofc_parse_operator_str_rep(
	const ofc_parse_operator_e operator)
{
	if (operator >= OFC_PARSE_OPERATOR_COUNT)
		return NULL;

	return ofc_parse_operator__name[operator];
}


unsigned ofc_parse_operator(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_operator_e* operator)
{
	ofc_parse_operator_e op;
	unsigned size = 1;
	switch (ptr[0])
	{
		case '*':
			if (ptr[1] == '*')
			{
				op = OFC_PARSE_OPERATOR_POWER;
				size = 2;
			}
			else
			{
				op = OFC_PARSE_OPERATOR_MULTIPLY;
			}
			break;

		case '/':
			if (ptr[1] == '/')
			{
				op = OFC_PARSE_OPERATOR_CONCAT;
				size = 2;
			}
			else if (ptr[1] == '=')
			{
				op = OFC_PARSE_OPERATOR_NE;
				size = 2;
			}
			else
			{
				/* Divide counts as a mult op. */
				op = OFC_PARSE_OPERATOR_DIVIDE;
			}
			break;

		case '+':
			op = OFC_PARSE_OPERATOR_ADD;
			break;

		case '-':
			op = OFC_PARSE_OPERATOR_SUBTRACT;
			break;

		case '=':
			if (ptr[1] != '=')
				return 0;
			op = OFC_PARSE_OPERATOR_EQ;
			size = 2;
			break;

		case '>':
			op = OFC_PARSE_OPERATOR_GT;
			if (ptr[1] == '=')
			{
				size = 2;
				op = OFC_PARSE_OPERATOR_GE;
			}
			break;

		case '<':
			op = OFC_PARSE_OPERATOR_LT;
			if (ptr[1] == '=')
			{
				size = 2;
				op = OFC_PARSE_OPERATOR_LE;
			}
			break;

		case '.':
			if (strncasecmp(ptr, ".EQ.", 4) == 0)
			{
				op = OFC_PARSE_OPERATOR_EQ;
				size = 4;
			}
			else if (strncasecmp(ptr, ".NE.", 4) == 0)
			{
				op = OFC_PARSE_OPERATOR_NE;
				size = 4;
			}
			else if (strncasecmp(ptr, ".LT.", 4) == 0)
			{
				op = OFC_PARSE_OPERATOR_LT;
				size = 4;
			}
			else if (strncasecmp(ptr, ".LE.", 4) == 0)
			{
				op = OFC_PARSE_OPERATOR_LE;
				size = 4;
			}
			else if (strncasecmp(ptr, ".GT.", 4) == 0)
			{
				op = OFC_PARSE_OPERATOR_GT;
				size = 4;
			}
			else if (strncasecmp(ptr, ".GE.", 4) == 0)
			{
				op = OFC_PARSE_OPERATOR_GE;
				size = 4;
			}
			else if (strncasecmp(ptr, ".NOT.", 5) == 0)
			{
				op = OFC_PARSE_OPERATOR_NOT;
				size = 5;
			}
			else if (strncasecmp(ptr, ".AND.", 5) == 0)
			{
				op = OFC_PARSE_OPERATOR_AND;
				size = 5;
			}
			else if (strncasecmp(ptr, ".OR.", 4) == 0)
			{
				op = OFC_PARSE_OPERATOR_OR;
				size = 4;
			}
			else if (strncasecmp(ptr, ".XOR.", 5) == 0)
			{
				op = OFC_PARSE_OPERATOR_XOR;
				size = 5;
			}
			else if (strncasecmp(ptr, ".EQV.", 5) == 0)
			{
				op = OFC_PARSE_OPERATOR_EQV;
				size = 5;
			}
			else if (strncasecmp(ptr, ".NEQV.", 6) == 0)
			{
				op = OFC_PARSE_OPERATOR_NEQV;
				size = 6;
			}
			else
			{
				/* TODO - Handle Fortran90 defined operators. */
				return 0;
			}
			break;

		default:
			return 0;
	}

	if ((size > 1) && !ofc_sparse_sequential(
		src, ptr, size))
	{
		ofc_parse_debug_warning(debug,
			ofc_sparse_ref(src, ptr, size),
			"Operators shouldn't contain whitespace");
	}

	if (operator) *operator = op;
	return size;
}

bool ofc_parse_operator_unary(
	ofc_parse_operator_e operator)
{
	switch (operator)
	{
		case OFC_PARSE_OPERATOR_ADD:
		case OFC_PARSE_OPERATOR_SUBTRACT:
		case OFC_PARSE_OPERATOR_NOT:
			return true;
		default:
			break;

	}

	return false;
}

bool ofc_parse_operator_binary(
	ofc_parse_operator_e operator)
{
	switch (operator)
	{
		case OFC_PARSE_OPERATOR_POWER:
		case OFC_PARSE_OPERATOR_MULTIPLY:
		case OFC_PARSE_OPERATOR_DIVIDE:
		case OFC_PARSE_OPERATOR_ADD:
		case OFC_PARSE_OPERATOR_SUBTRACT:
		case OFC_PARSE_OPERATOR_CONCAT:
		case OFC_PARSE_OPERATOR_EQ:
		case OFC_PARSE_OPERATOR_NE:
		case OFC_PARSE_OPERATOR_LT:
		case OFC_PARSE_OPERATOR_LE:
		case OFC_PARSE_OPERATOR_GT:
		case OFC_PARSE_OPERATOR_GE:
		case OFC_PARSE_OPERATOR_AND:
		case OFC_PARSE_OPERATOR_OR:
		case OFC_PARSE_OPERATOR_XOR:
		case OFC_PARSE_OPERATOR_EQV:
		case OFC_PARSE_OPERATOR_NEQV:
			return true;
		default:
			break;
	}

	return false;
}

unsigned ofc_parse_operator_precedence_binary(
	ofc_parse_operator_e operator)
{
	switch (operator)
	{
		case OFC_PARSE_OPERATOR_POWER:
			return 2;
		case OFC_PARSE_OPERATOR_MULTIPLY:
		case OFC_PARSE_OPERATOR_DIVIDE:
			return 3;
		case OFC_PARSE_OPERATOR_ADD:
		case OFC_PARSE_OPERATOR_SUBTRACT:
			return 4;
		case OFC_PARSE_OPERATOR_CONCAT:
			return 5;
		case OFC_PARSE_OPERATOR_EQ:
		case OFC_PARSE_OPERATOR_NE:
		case OFC_PARSE_OPERATOR_LT:
		case OFC_PARSE_OPERATOR_LE:
		case OFC_PARSE_OPERATOR_GT:
		case OFC_PARSE_OPERATOR_GE:
			return 6;
		case OFC_PARSE_OPERATOR_AND:
			return 8;
		case OFC_PARSE_OPERATOR_OR:
			return 9;
		case OFC_PARSE_OPERATOR_XOR:
		case OFC_PARSE_OPERATOR_EQV:
		case OFC_PARSE_OPERATOR_NEQV:
		default:
			break;

	}

	return 10;
}

unsigned ofc_parse_operator_precedence_unary(
	ofc_parse_operator_e operator)
{
	switch (operator)
	{
		case OFC_PARSE_OPERATOR_ADD:
		case OFC_PARSE_OPERATOR_SUBTRACT:
			return 0;
		case OFC_PARSE_OPERATOR_NOT:
			return 7;
		default:
			break;

	}

	return 10;
}

bool ofc_parse_operator_print(
	ofc_colstr_t* cs, const ofc_parse_operator_e operator)
{
	if (operator >= OFC_PARSE_OPERATOR_COUNT)
		return false;

	return ofc_colstr_keyword_atomic_writez(cs,
		ofc_parse_operator_str_rep(operator));
}
