unsigned parse_program(const char* src, parse_t* entry)
{
	unsigned i = parse_keyword(src, "PROGRAM");
	if (i == 0) return 0;

	parse_t name;
	unsigned len = parse_name(&src[i], &name);
	if (len == 0)
	{
		fprintf(stderr, "Error: Expected name in PROGRAM statement.\n");
		return 0;
	}
	i += len;

	if (src[i++] != '\n')
	{
		fprintf(stderr, "Error: Expected newline after PROGRAM statement.\n");
		return 0;
	}

	/* TODO - Parse specification-part */

	/* TODO - Parse execution-part */

	/* TODO - Parse inernal-subprogram-part */

	len = parse_keyword(&src[i], "ENDPROGRAM");
	if (len != 0)
	{
		parse_t endname;
		unsigned nlen = parse_name(&src[i + len], &endname);
		if ((nlen > 0) && ((name.name.size != endname.name.size)
			|| (strncmp(name.name.base, endname.name.base, nlen) != 0)))
		{
			fprintf(stderr, "Warning: END PROGRAM name doesn't match PROGRAM name.\n");
		}
		len += nlen;
	}
	else
	{
		len = parse_keyword(src[i], "END");
		if (len == 0)
		{
			fprintf(stderr, "Error: Expected end of program.\n");
			return 0;
		}
	}
	i += len;

	if (src[i] != '\0')
	{
		fprintf(stderr, "Error: Expected end of input after main program.\n");
		return 0;
	}

	return i;
}
