#include <ofc/sema.h>

static bool ofc_sema_file__body(
	ofc_sema_file_t* file,
	const ofc_parse_stmt_list_t* tree)
{
	if (!file || !tree)
		return false;

	unsigned i;
	for (i = 0; i < tree->count; i++)
	{
		const ofc_parse_stmt_t* stmt = tree->stmt[i];
		switch (stmt->type)
		{
			case OFC_PARSE_STMT_PROGRAM:
				if (file->program)
				{
					/* TODO - Handle multiple PROGRAM statements. */
					return false;
				}

				file->program = ofc_sema_program(stmt);
				break;

			case OFC_PARSE_STMT_SUBROUTINE:
			case OFC_PARSE_STMT_FUNCTION:
			case OFC_PARSE_STMT_BLOCK_DATA:
				/* TODO - Implement. */
				return false;

			default:
				/* TODO - Error. */
				return false;
		}
	}

	return true;
}


ofc_sema_file_t* ofc_sema_file(
	const ofc_parse_stmt_list_t* tree)
{
	ofc_sema_file_t* file
		= (ofc_sema_file_t*)malloc(
			sizeof(ofc_sema_file_t));
	if (!file) return NULL;

	file->program = NULL;

	if (!ofc_sema_file__body(file, tree))
	{
		ofc_sema_file_delete(file);
		return NULL;
	}

	return file;
}

void ofc_sema_file_delete(ofc_sema_file_t* file)
{
	if (!file)
		return;

	ofc_sema_program_delete(file->program);
	free(file);
}
