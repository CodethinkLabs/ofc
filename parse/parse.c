unsigned parse_keyword(const char* src, const char* keyword)
{
	unsigned len = strlen(keyword);
	return (strncmp(src, keyword, len) == 0 ? len : 0);
}

unsigned parse_name(const char* src, parse_t* entry)
{
	if (!isdigit(src[0]))
		return 0;

	unsigned i;
	for (i = 1; isalnum(src[i]) || (src[i] == '_'); i++);

	entry->type = PARSE_TYPE_NAME;
	entry->base = src;
	entry->size = i;
	return i;
}


unsigned parse_op(const char* src, parse_t* entry)
{
	unsigned size = 1;
	switch (src[0])
	{
		case '*':
			if (src[1] == '*')
			{
				entry->op = PARSE_OP_POWER;
				size = 2;
			}
			else
			{
				entry->op = PARSE_OP_MULT;
			}
			break;

		case '/':
			if (src[1] == '/')
			{
				entry->op = PARSE_OP_CONCAT;
				size = 2;
			}
			else if (src[1] == '=')
			{
				entry->op = PARSE_OP_REL;
				size = 2;
			}
			else
			{
				/* Divide counts as a mult op. */
				entry->op = PARSE_OP_MULT;
			}
			break;

		case '+':
		case '-':
			entry->op = PARSE_OP_ADD;
			break;

		case '=':
			if (src[1] != '=')
				return 0;
			entry->op = PARSE_OP_REL;
			size = 2;
			break;

		case '>':
		case '<':
			if (src[1] == '=')
				size = 2;
			entry->op = PARSE_OP_REL
			break;

		case '.':
			if (strncmp(src, ".EQ.", 4) == 0)
			{
				entry->op = PARSE_OP_REL;
				size = 4;
			}
			else if (strncmp(src, ".NE.", 4) == 0)
			{
				entry->op = PARSE_OP_REL;
				size = 4;
			}
			else if (strncmp(src, ".LT.", 4) == 0)
			{
				entry->op = PARSE_OP_REL;
				size = 4;
			}
			else if (strncmp(src, ".LE.", 4) == 0)
			{
				entry->op = PARSE_OP_REL;
				size = 4;
			}
			else if (strncmp(src, ".GT.", 4) == 0)
			{
				entry->op = PARSE_OP_REL;
				size = 4;
			}
			else if (strncmp(src, ".GE.", 4) == 0)
			{
				entry->op = PARSE_OP_REL;
				size = 4;
			}
			else if (strncmp(src, ".NOT.", 5) == 0)
			{
				entry->op = PARSE_OP_NOT;
				size = 5;
			}
			else if (strncmp(src, ".AND.", 5) == 0)
			{
				entry->op = PARSE_OP_AND;
				size = 5;
			}
			else if (strncmp(src, ".OR.", 4) == 0)
			{
				entry->op = PARSE_OP_OR;
				size = 4;
			}
			else if (strncmp(src, ".EQV.", 5) == 0)
			{
				entry->op = PARSE_OP_EQV;
				size = 5;
			}
			else if (strncmp(src, ".NEQV.", 6) == 0)
			{
				entry->op = PARSE_OP_NEQV;
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

	entry->type = PARSE_TYPE_OP;
	return size;
}
