#include <ofc/sema.h>


ofc_sema_typeval_t* ofc_sema_typeval_literal(
	const ofc_parse_literal_t* literal,
	const ofc_type_t* type)
{
	/* TODO - Implement. */

	return NULL;
}



void ofc_sema_typeval_delete(
	ofc_sema_typeval_t* typeval)
{
	if (!typeval)
		return;

	if (typeval->type
		&& (typeval->type->type == OFC_SEMA_TYPE_CHARACTER))
		free(typeval->character);

	free(typeval);
}

bool ofc_sema_typeval_get(
	const ofc_sema_typeval_t* typeval,
	const ofc_sema_type_t* type,
	void* value)
{
	if (!typeval)
		return false;

	return ofc_sema_type_cast_value(
		typeval->type, &typeval->logical, type, value);
}
