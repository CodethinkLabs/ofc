#include <ofc/sema.h>


ofc_sema_array_t* ofc_sema_array_create(
	unsigned dimensions,
	unsigned* base, unsigned* count)
{
    if (!count) return NULL;

	ofc_sema_array_t* array
		= (ofc_sema_array_t*)(sizeof(ofc_sema_array_t)
			+ (dimensions * 2 * sizeof(unsigned)));
	if (!array) return NULL;

	array->dimensions = dimensions;

	unsigned i;
	for (i = 0; i < dimensions; i++)
	{
		unsigned b = (base ? base[i] : 1);
		array->size[(i << 1) + 0] = b;
		array->size[(i << 1) + 1] = count[i];
	}

	return array;
}


void ofc_sema_array_delete(ofc_sema_array_t* array)
{
	free(array);
}


uint8_t ofc_sema_array_hash(
	const ofc_sema_array_t* array)
{
	if (!array)
		return 0;

	uint8_t hash = array->dimensions;

	unsigned i;
	for (i = 0; i < array->dimensions; i++)
	{
		hash += array->size[(i << 1) + 0];
		hash += array->size[(i << 1) + 1];
	}

	return hash;
}

bool ofc_sema_array_compare(
	const ofc_sema_array_t* a,
	const ofc_sema_array_t* b)
{
	if (!a || !b)
		return false;

	if (a == b)
		return true;

	if (a->dimensions != b->dimensions)
		return false;

	unsigned i, j;
	for (i = 0, j = 0; i < a->dimensions; i++, j += 2)
	{
		if ((a->size[j + 0] != b->size[j + 0])
			|| (a->size[j + 1] != b->size[j + 1]))
			return false;
	}

	return true;
}


unsigned ofc_sema_array_total(const ofc_sema_array_t* array)
{
	if (!array || array->dimensions == 0)
		return 0;

	unsigned total = 1;
	unsigned i;
	for (i = 0; i < array->dimensions; i++)
		total *= array->size[(i << 1) + 1];

	return total;
}
