#include "parse.h"
#include <string.h>


unsigned parse_operator(
	const sparse_t* src, const char* ptr,
	parse_operator_e* operator)
{
	parse_operator_e op;
	unsigned size = 1;
	switch (ptr[0])
	{
		case '*':
			if (ptr[1] == '*')
			{
				op = PARSE_OPERATOR_POWER;
				size = 2;
			}
			else
			{
				op = PARSE_OPERATOR_MULTIPLY;
			}
			break;

		case '/':
			if (ptr[1] == '/')
			{
				op = PARSE_OPERATOR_CONCAT;
				size = 2;
			}
			else if (ptr[1] == '=')
			{
				op = PARSE_OPERATOR_NE;
				size = 2;
			}
			else
			{
				/* Divide counts as a mult op. */
				op = PARSE_OPERATOR_DIVIDE;
			}
			break;

		case '+':
			op = PARSE_OPERATOR_ADD;
			break;

		case '-':
			op = PARSE_OPERATOR_SUBTRACT;
			break;

		case '=':
			if (ptr[1] != '=')
				return 0;
			op = PARSE_OPERATOR_EQ;
			size = 2;
			break;

		case '>':
			op = PARSE_OPERATOR_GT;
			if (ptr[1] == '=')
			{
				size = 2;
				op = PARSE_OPERATOR_GE;
			}
			break;

		case '<':
			op = PARSE_OPERATOR_LT;
			if (ptr[1] == '=')
			{
				size = 2;
				op = PARSE_OPERATOR_LE;
			}
			break;

		case '.':
			if (strncmp(ptr, ".EQ.", 4) == 0)
			{
				op = PARSE_OPERATOR_EQ;
				size = 4;
			}
			else if (strncmp(ptr, ".NE.", 4) == 0)
			{
				op = PARSE_OPERATOR_NE;
				size = 4;
			}
			else if (strncmp(ptr, ".LT.", 4) == 0)
			{
				op = PARSE_OPERATOR_LT;
				size = 4;
			}
			else if (strncmp(ptr, ".LE.", 4) == 0)
			{
				op = PARSE_OPERATOR_LE;
				size = 4;
			}
			else if (strncmp(ptr, ".GT.", 4) == 0)
			{
				op = PARSE_OPERATOR_GT;
				size = 4;
			}
			else if (strncmp(ptr, ".GE.", 4) == 0)
			{
				op = PARSE_OPERATOR_GE;
				size = 4;
			}
			else if (strncmp(ptr, ".NOT.", 5) == 0)
			{
				op = PARSE_OPERATOR_NOT;
				size = 5;
			}
			else if (strncmp(ptr, ".AND.", 5) == 0)
			{
				op = PARSE_OPERATOR_AND;
				size = 5;
			}
			else if (strncmp(ptr, ".OR.", 4) == 0)
			{
				op = PARSE_OPERATOR_OR;
				size = 4;
			}
			else if (strncmp(ptr, ".EQV.", 5) == 0)
			{
				op = PARSE_OPERATOR_EQV;
				size = 5;
			}
			else if (strncmp(ptr, ".NEQV.", 6) == 0)
			{
				op = PARSE_OPERATOR_NEQV;
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

	if ((size > 1) && !sparse_sequential(
		src, ptr, size))
	{
		sparse_warning(src, ptr,
			"Operators shouldn't contain whitespace");
	}

	if (operator) *operator = op;
	return size;
}

bool parse_operator_unary(
	parse_operator_e operator)
{
	switch (operator)
	{
		case PARSE_OPERATOR_ADD:
		case PARSE_OPERATOR_SUBTRACT:
		case PARSE_OPERATOR_NOT:
			return true;
		default:
			break;

	}

	return false;
}

bool parse_operator_binary(
	parse_operator_e operator)
{
	switch (operator)
	{
		case PARSE_OPERATOR_POWER:
		case PARSE_OPERATOR_MULTIPLY:
		case PARSE_OPERATOR_DIVIDE:
		case PARSE_OPERATOR_ADD:
		case PARSE_OPERATOR_SUBTRACT:
		case PARSE_OPERATOR_CONCAT:
		case PARSE_OPERATOR_EQ:
		case PARSE_OPERATOR_NE:
		case PARSE_OPERATOR_LT:
		case PARSE_OPERATOR_LE:
		case PARSE_OPERATOR_GT:
		case PARSE_OPERATOR_GE:
		case PARSE_OPERATOR_AND:
		case PARSE_OPERATOR_OR:
		case PARSE_OPERATOR_EQV:
		case PARSE_OPERATOR_NEQV:
			return true;
		default:
			break;
	}

	return false;
}

unsigned parse_operator_precedence(
	parse_operator_e operator)
{
	switch (operator)
	{
		case PARSE_OPERATOR_POWER:
			return 2;
		case PARSE_OPERATOR_MULTIPLY:
		case PARSE_OPERATOR_DIVIDE:
			return 3;
		case PARSE_OPERATOR_ADD:
		case PARSE_OPERATOR_SUBTRACT:
			return 4;
		case PARSE_OPERATOR_CONCAT:
			return 5;
		case PARSE_OPERATOR_EQ:
		case PARSE_OPERATOR_NE:
		case PARSE_OPERATOR_LT:
		case PARSE_OPERATOR_LE:
		case PARSE_OPERATOR_GT:
		case PARSE_OPERATOR_GE:
			return 6;
		case PARSE_OPERATOR_NOT:
			return 7;
		case PARSE_OPERATOR_AND:
			return 8;
		case PARSE_OPERATOR_OR:
			return 9;
		case PARSE_OPERATOR_EQV:
		case PARSE_OPERATOR_NEQV:
		default:
			break;

	}

	return 10;
}
