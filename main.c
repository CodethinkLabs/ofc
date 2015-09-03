#include <stdlib.h>
#include <stdio.h>

#include "file.h"


static bool print_line(const line_t* line, void* param)
{
	(void)param; /* Unused */

	const char* strz = line_strz(line, true);
	if (!strz) return false;

	printf("%s\n", strz);
	return true;
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

	lang_opts_t opts = LANG_OPTS_F77;

	file_t* file = file_create(path, opts);
	if (!file)
	{
		fprintf(stderr, "Error: Failed process source file '%s'\n", path);
		return EXIT_FAILURE;
	}

	bool success = file_foreach_line(
		file, NULL, print_line);

	/* TODO - Further process file. */

	file_delete(file);

	return (success ? EXIT_SUCCESS : EXIT_FAILURE);
}
