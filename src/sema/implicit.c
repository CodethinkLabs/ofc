#include <ofc/sema.h>



ofc_sema_implicit_t* ofc_sema_implicit_create(void)
{
	const ofc_sema_type_t* real
		= ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_REAL, 0, false, false, false);
	const ofc_sema_type_t* integer
		= ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_INTEGER, 0, false, false, false);

	if (!real || !integer)
		return NULL;

	ofc_sema_implicit_t* implicit
		= (ofc_sema_implicit_t*)malloc(
			sizeof(ofc_sema_implicit_t));
	if (!implicit) return NULL;

	unsigned i;
	for (i = 0; i < 26; i++)
		implicit->type[i] = NULL;

	for (i = 0; i < 7; i++)
		implicit->type[i] = real;

	/* I, J, K, L, M, N */
	for (; i < 14; i++)
		implicit->type[i] = integer;

	for (; i < 26; i++)
		implicit->type[i] = real;

	return implicit;
}


bool ofc_sema_implicit_none(ofc_sema_implicit_t* implicit)
{
	if (!implicit)
		return false;

	unsigned i;
	for (i = 0; i < 26; i++)
		implicit->type[i] = NULL;

	return true;
}


bool ofc_sema_implicit_set(
	ofc_sema_implicit_t* implicit,
	const ofc_sema_type_t* type, char c)
{
	if (!implicit || !isalpha(c))
		return false;

	unsigned i = (toupper(c) - 'A');
	implicit->type[i] = type;

	return true;
}

const ofc_sema_type_t* ofc_sema_implicit_get(
	const ofc_sema_implicit_t* implicit, char c)
{
	if (!implicit || !isalpha(c))
		return NULL;
	return implicit->type[toupper(c) - 'A'];
}

void ofc_sema_implicit_delete(
	ofc_sema_implicit_t* implicit)
{
	free(implicit);
}
