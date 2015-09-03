#include "line.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>


struct line_s
{
	rope_t* rope;

	bool     has_label;
	unsigned label;

	char* strz;
};


line_t* line_create(unsigned* label, rope_t* rope)
{
	line_t* line = (line_t*)malloc(sizeof(line_t));
	if (!line) return NULL;

	line->rope = rope;

	line->has_label = (label != NULL);
	line->label     = (label ? *label : 0);

	line->strz = NULL;

	return line;
}

void line_delete(line_t* line)
{
	if (!line)
		return;

	rope_delete(line->rope);
	free(line);
}


bool line_append_rope(line_t* line, const rope_t* rope)
{
	if (!line)
		return false;

	if (!line->rope)
	{
		line->rope = rope_create();
		if (!line->rope)
			return false;
	}

	return rope_append_rope(line->rope, rope);
}


const char* line_strz(const line_t* line, bool label)
{
	if (!line) return NULL;

	if (!line->has_label || !label)
		return rope_strz(line->rope);

	if (!line->strz)
	{
		const char* codez = rope_strz(line->rope);
		if (!codez) return NULL;

		unsigned code_len = strlen(codez);

		unsigned label_len = (line->label == 0 ? 1 : 0);
		unsigned i;
		for (i = line->label; i > 0; i /= 10, label_len++);

		char* strz = (char*)malloc(label_len + 1 + code_len + 1);
		if (!strz) return NULL;

		/* TODO - Error check this. */
		sprintf(strz, "%u %s", line->label, codez);

		/* Cast to non-const because we're caching and
           the behaviour is side-effect free. */
		((line_t*)line)->strz = strz;
	}

	return line->strz;
}


bool line_position(
	const line_t* line, unsigned offset,
	const char** file, unsigned *row, unsigned* col)
{
	if (!line) return false;
	return rope_position(
		line->rope, offset, file, row, col);
}
