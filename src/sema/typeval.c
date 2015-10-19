#include <ofc/sema.h>


ofc_sema_typeval_t* ofc_sema_typeval_literal(
	const ofc_parse_literal_t* literal,
	const ofc_sema_type_t* type)
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


#define offsetof(type, member)  __builtin_offsetof(type, member)

unsigned ofc_sema_typeval_size(
	const ofc_sema_typeval_t* typeval)
{
	if (!typeval)
		return 0;
	return ofc_sema_type_size(typeval->type);
}

ofc_sema_typeval_t* ofc_sema_typeval_copy(
	const ofc_sema_typeval_t* typeval)
{
	if (!typeval)
		return NULL;

	unsigned size = ofc_sema_typeval_size(typeval);

	ofc_sema_typeval_t* copy
		= (ofc_sema_typeval_t*)malloc(
			offsetof(ofc_sema_typeval_t, logical) + size);
	if (!copy) return NULL;

	copy->type = typeval->type;
	memcpy(&copy->logical, &typeval->logical, size);
	return copy;
}


bool ofc_sema_typeval_get(
	const ofc_sema_typeval_t* typeval,
	const ofc_sema_type_t* type,
	void* value, bool* lossy)
{
	if (!typeval)
		return false;

	return ofc_sema_type_cast_value(
		typeval->type, &typeval->logical, type,
		value, lossy);
}
