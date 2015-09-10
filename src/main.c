#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "file.h"
#include "preprocess.h"


const char *get_filename_ext(const char *file_name) {
	if (!file_name)
		return NULL;

	const char *dot = NULL;
	unsigned i;
	for (i = 0; file_name[i] != '\0'; i++)
	{
		if (file_name[i] == '/')
			dot = NULL;
		else if (file_name[i] == '.')
			dot = &file_name[i];
	}
  return (dot ? &dot[1] : NULL);
}

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

  const char* source_file_ext = get_filename_ext(path);

	lang_opts_t opts;

	if (source_file_ext)
	{
		if (strcasecmp(source_file_ext, "F90") == 0)
			opts = LANG_OPTS_F90;
		else
			opts = LANG_OPTS_F77;
	}

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

	const sparse_t* condense
		= preprocess_condense_sparse(context);
	const char* strz = sparse_strz(condense);
	printf("%s\n", strz);

	/* TODO - Parse. */

	preprocess_delete(context);

	return EXIT_SUCCESS;
}
