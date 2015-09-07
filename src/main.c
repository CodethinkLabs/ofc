#include <stdlib.h>
#include <stdio.h>

#include "file.h"
#include "preprocess.h"


int main(int argc, const char* argv[])
{
	if (argc < 2)
	{
		fprintf(stderr, "Error: Expected source path\n");
		return EXIT_FAILURE;
	}
	else if (argc > 2)
	{
		fprintf(stderr, "Error: Only one source path expected\n");
		return EXIT_FAILURE;
	}

	const char* path = argv[1];

	lang_opts_t opts = LANG_OPTS_F77;

	file_t* file = file_create(path);
	if (!file)
	{
		fprintf(stderr, "Error: Failed read source file '%s'\n", path);
		return EXIT_FAILURE;
	}

	preprocess_t* context = preprocess(file, opts);
	if (!context)
	{
		fprintf(stderr, "Error: Failed preprocess source file '%s'\n", path);
		file_delete(file);
		return EXIT_FAILURE;
	}

	const rope_t* condense
		= preprocess_condense_rope(context);
	const char* strz = rope_strz(condense);
	printf("%s\n", strz);

	/* TODO - Parse. */

	preprocess_delete(context);

	return EXIT_SUCCESS;
}
