#include <ofc/sema.h>

ofc_sema_program_t* ofc_sema_program(
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt || (stmt->type != OFC_PARSE_STMT_PROGRAM))
		return false;

	if (stmt->program.type
		|| stmt->program.args)
	{
		/* Shouldn't pass the parse stage like this. */
		return false;
	}

	ofc_sema_program_t* program
		= (ofc_sema_program_t*)malloc(
			sizeof(ofc_sema_program_t));
	if (!program) return NULL;

	program->name = stmt->program.name;

	const ofc_parse_stmt_list_t* body
		= stmt->program.body;
	if (body)
	{
		unsigned i;
		for (i = 0; i < body->count; i++)
		{
			ofc_parse_stmt_t* body_stmt = body->stmt[i];

			switch (body_stmt->type)
			{
				case OFC_PARSE_STMT_DECL:
					/* TODO - Implement. */
					break;

				/* TODO - Handle statements. */

				default:
					/* TODO - Error. */
					return false;
			}
		}
	}

	return program;
}

void ofc_sema_program_delete(
	ofc_sema_program_t* program)
{
	if (!program)
		return;

	free(program);
}
