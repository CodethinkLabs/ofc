/* Copyright 2015 Codethink Ltd.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include <string.h>

#include "ofc/sema.h"
#include "ofc/target.h"


static bool ofc_sema_type__kind_valid(
	unsigned kind)
{
	if (kind == 0)
		return false;

	if ((kind & (kind - 1)) == 0)
		return true;

	if ((kind % 3) == 0)
		return true;

	if (kind == 7)
		return true;

	if ((kind % 5) == 0)
		return ofc_sema_type__kind_valid(kind / 5);

	return false;
}

static bool ofc_sema_type__kind_absolute(
	unsigned kind)
{
	return ((kind > 0) && ((kind % 3) == 0));
}

bool ofc_sema_type_kind_size(
	unsigned def, unsigned kind, unsigned* size)
{
	if (ofc_sema_type__kind_absolute(kind))
	{
		if (size) *size = (kind / 3);
		return true;
	}

	if (kind == 7)
	{
		if (size) *size = ofc_target_pointer_size_get();
		return true;
	}

	if ((kind & (kind - 1)) == 0)
	{
		if (size)
			for (*size = def; kind > 1; kind >>= 1, *size <<= 1);
		return true;
	}

	if ((kind % 5) != 0)
		return false;

	unsigned s;
	if (!ofc_sema_type_kind_size(
		def, (kind / 5), &s))
		return false;

	if (s % 2)
		return false;

	if (size) *size = (s / 2);
	return true;
}



static ofc_hashmap_t* ofc_sema_type__map = NULL;

static const char* ofc_sema_type__name[] =
{
	"LOGICAL",
	"INTEGER",
	"REAL",
	"COMPLEX",
	"BYTE",
	"CHARACTER",
	"STRUCTURE",
	"POINTER",
	"FUNCTION",
	"SUBROUTINE",

	NULL
};

static const char* ofc_sema_type__cast[] =
{
	NULL,
	"INT",
	"REAL",
	"CMPLX",
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,

	NULL
};

const char* ofc_sema_type_str_rep(
	const ofc_sema_type_t* type)
{
	if (!type || (type->type >= OFC_SEMA_TYPE_COUNT))
		return "<UNDEFINED>";

	return ofc_sema_type__name[type->type];
}

const char* ofc_sema_type_enum_str_rep(
	const ofc_sema_type_e type_enum)
{
	if (type_enum >= OFC_SEMA_TYPE_COUNT)
		return "<UNDEFINED>";

	return ofc_sema_type__name[type_enum];
}

const char* ofc_sema_type_str_cast_rep(
	const ofc_sema_type_t* type)
{
	if (!type)
		return NULL;

	if (type->type >= OFC_SEMA_TYPE_COUNT)
		return NULL;

	return ofc_sema_type__cast[type->type];
}

static void ofc_sema_type__delete(ofc_sema_type_t* type)
{
	if (!type)
		return;

	ofc_sema_array_delete(type->array);
	free(type);
}

uint8_t ofc_sema_type_hash(
	const ofc_sema_type_t* type)
{
	if (!type)
		return 0;

	uint8_t hash = type->type;

	hash += ofc_sema_array_hash(type->array);

	switch (type->type)
	{
		case OFC_SEMA_TYPE_STRUCTURE:
			hash += ofc_sema_structure_hash(
				type->structure);
			break;

		case OFC_SEMA_TYPE_POINTER:
			hash += ofc_sema_type_hash(
				type->subtype);
			break;

		case OFC_SEMA_TYPE_CHARACTER:
			hash += type->kind + type->len;
			break;

		case OFC_SEMA_TYPE_FUNCTION:
			hash += ofc_sema_type_hash(
				type->subtype);
			break;

		default:
			hash += type->kind;
			break;
	}

	return hash;
}

static const ofc_sema_type_t* ofc_sema_type__key(
	const ofc_sema_type_t* type)
{
	return type;
}

static void ofc_sema_type__map_cleanup(void)
{
	ofc_hashmap_delete(ofc_sema_type__map);
}

static const ofc_sema_type_t* ofc_sema_type__create(
	ofc_sema_type_e type,
	unsigned kind, unsigned len,
	const ofc_sema_array_t* array,
	const ofc_sema_type_t* subtype,
	const ofc_sema_structure_t* structure)
{
	switch (type)
	{
		case OFC_SEMA_TYPE_BYTE:
			if (len != 0)
				return NULL;
			if ((kind != 1)
				&& (kind != 3))
				return NULL;
			break;

		case OFC_SEMA_TYPE_CHARACTER:
			if (!ofc_sema_type__kind_valid(kind))
				return NULL;
			break;

		case OFC_SEMA_TYPE_LOGICAL:
		case OFC_SEMA_TYPE_INTEGER:
		case OFC_SEMA_TYPE_REAL:
		case OFC_SEMA_TYPE_COMPLEX:
			if (!ofc_sema_type__kind_valid(kind)
				|| (len != 0))
				return NULL;
			break;

		default:
			break;
	}

	if (!ofc_sema_type__map)
	{
		ofc_sema_type__map = ofc_hashmap_create(
			(void*)ofc_sema_type_hash,
			(void*)ofc_sema_type_compare,
			(void*)ofc_sema_type__key,
			(void*)ofc_sema_type__delete);
		if (!ofc_sema_type__map)
			return NULL;

		atexit(ofc_sema_type__map_cleanup);
	}

	ofc_sema_type_t stype =
		{
			.type  = type,
			.array = NULL,
		};

	if (array)
	{
		stype.array = ofc_sema_array_copy(array);
		if (!stype.array) return NULL;
	}

	switch (type)
	{
		case OFC_SEMA_TYPE_POINTER:
		case OFC_SEMA_TYPE_FUNCTION:
			stype.subtype = subtype;
			break;
		case OFC_SEMA_TYPE_STRUCTURE:
			stype.structure = structure;
			break;
		default:
			stype.kind = kind;
			stype.len = len;
			break;
	}

	/* A LOGICAL*1 is a synonym of BYTE. */
	if (stype.type == OFC_SEMA_TYPE_LOGICAL)
	{
		unsigned bsize;
		if (ofc_sema_type_kind_size(
			ofc_target_logical_size_get(),
			stype.kind, &bsize) && (bsize == 1))
		{
			stype.type = OFC_SEMA_TYPE_BYTE;
			stype.kind = 1;
		}
	}

	const ofc_sema_type_t* gtype
		= ofc_hashmap_find(
			ofc_sema_type__map, &stype);
	if (gtype)
	{
		ofc_sema_array_delete(stype.array);
		return gtype;
	}

	ofc_sema_type_t* ntype
		= (ofc_sema_type_t*)malloc(
			sizeof(ofc_sema_type_t));
	if (!ntype)
	{
		ofc_sema_array_delete(stype.array);
		return NULL;
	}
	*ntype = stype;

	if (!ofc_hashmap_add(
		ofc_sema_type__map, ntype))
	{
		ofc_sema_type__delete(ntype);
		return NULL;
	}

	return ntype;
}

const ofc_sema_type_t* ofc_sema_type_create_primitive(
	ofc_sema_type_e type,
	unsigned kind)
{
	switch (type)
	{
		case OFC_SEMA_TYPE_LOGICAL:
		case OFC_SEMA_TYPE_INTEGER:
		case OFC_SEMA_TYPE_REAL:
		case OFC_SEMA_TYPE_COMPLEX:
		case OFC_SEMA_TYPE_BYTE:
			break;
		default:
			return NULL;
	}

	return ofc_sema_type__create(
		type, kind, 0,
		NULL, NULL, NULL);
}

const ofc_sema_type_t* ofc_sema_type_create_character(
	unsigned kind, unsigned len)
{
	return ofc_sema_type__create(
		OFC_SEMA_TYPE_CHARACTER, kind, len,
		NULL, NULL, NULL);
}

const ofc_sema_type_t* ofc_sema_type_create_structure(
	const ofc_sema_structure_t* structure)
{
	return ofc_sema_type__create(
		OFC_SEMA_TYPE_STRUCTURE, 0, 0,
		NULL, NULL, structure);
}

const ofc_sema_type_t* ofc_sema_type_create_pointer(
	ofc_sema_type_t* target)
{
	return ofc_sema_type__create(
		OFC_SEMA_TYPE_POINTER, 0, 0,
		NULL, target, NULL);
}

const ofc_sema_type_t* ofc_sema_type_create_array(
	const ofc_sema_type_t* type,
	const ofc_sema_array_t* array)
{
	if (ofc_sema_type_is_procedure(type))
		return NULL;

	return ofc_sema_type__create(
		type->type, type->kind, type->len,
		array, type->subtype, type->structure);
}

const ofc_sema_type_t* ofc_sema_type_create_function(
	const ofc_sema_type_t* type)
{
	if (!type)
		return NULL;

	if (ofc_sema_type_is_procedure(type))
		return NULL;

	return ofc_sema_type__create(
		OFC_SEMA_TYPE_FUNCTION, 0, 0,
		NULL, type, NULL);
}


const ofc_sema_type_t* ofc_sema_type_star_len(
	const ofc_sema_type_t* type, unsigned star_len)
{
	if (!type || (star_len == 0))
		return NULL;

	if (type->type == OFC_SEMA_TYPE_CHARACTER)
	{
		if (type->len != 0)
		{
			/* TODO - Warn that length is being overridden. */
		}

		return ofc_sema_type__create(
			type->type, type->kind, star_len,
			type->array, type->subtype, type->structure);
	}

	/* TODO - Warn if a kind isn't implicit. */

	if (type->type == OFC_SEMA_TYPE_FUNCTION)
	{
		const ofc_sema_type_t* subtype
			= ofc_sema_type_star_len(
				type->subtype, star_len);
		if (!subtype) return NULL;

		return ofc_sema_type_create_function(subtype);
	}

	switch (type->type)
	{
		case OFC_SEMA_TYPE_LOGICAL:
		case OFC_SEMA_TYPE_INTEGER:
		case OFC_SEMA_TYPE_REAL:
		case OFC_SEMA_TYPE_COMPLEX:
			return ofc_sema_type__create(
				type->type, star_len, type->len,
				type->array, type->subtype, type->structure);
			break;
		default:
			break;
	}

	return NULL;
}


const ofc_sema_type_t* ofc_sema_type_logical_default(void)
{
	static const ofc_sema_type_t* logical = NULL;

	if (!logical)
	{
		logical = ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_LOGICAL, 1);
	}

	return logical;
}

const ofc_sema_type_t* ofc_sema_type_integer_default(void)
{
	static const ofc_sema_type_t* integer = NULL;

	if (!integer)
	{
		integer = ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_INTEGER, 1);
	}

	return integer;
}

const ofc_sema_type_t* ofc_sema_type_real_default(void)
{
	static const ofc_sema_type_t* real = NULL;

	if (!real)
	{
		real = ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_REAL, 1);
	}

	return real;
}

const ofc_sema_type_t* ofc_sema_type_double_default(void)
{
	static const ofc_sema_type_t* dbl = NULL;

	if (!dbl)
	{
		const ofc_sema_type_t* real
			= ofc_sema_type_real_default();
		if (!real) return NULL;

		dbl = ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_REAL, 2);
	}

	return dbl;
}

const ofc_sema_type_t* ofc_sema_type_complex_default(void)
{
	static const ofc_sema_type_t* complex = NULL;

	if (!complex)
	{
		complex = ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_COMPLEX, 1);
	}

	return complex;
}

const ofc_sema_type_t* ofc_sema_type_byte_default(void)
{
	static const ofc_sema_type_t* byte = NULL;

	if (!byte)
	{
		byte = ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_BYTE, 1);
	}

	return byte;
}

const ofc_sema_type_t* ofc_sema_type_subroutine(void)
{
	static const ofc_sema_type_t* subroutine = NULL;

	if (!subroutine)
	{
		subroutine = ofc_sema_type__create(
			OFC_SEMA_TYPE_SUBROUTINE, 0, 0,
			NULL, NULL,NULL);
	}

	return subroutine;
}


const ofc_sema_type_t* ofc_sema_type_spec(
	const ofc_sema_spec_t* spec)
{
	if (!spec)
		return NULL;

	if (spec->type_implicit)
		return NULL;

	unsigned kind = spec->kind;
	if (kind == 0) kind = 1;

	const ofc_sema_type_t* type;
	switch (spec->type)
	{
		case OFC_SEMA_TYPE_LOGICAL:
		case OFC_SEMA_TYPE_INTEGER:
		case OFC_SEMA_TYPE_REAL:
		case OFC_SEMA_TYPE_COMPLEX:
		case OFC_SEMA_TYPE_BYTE:
			if ((spec->len != 0)
				|| spec->len_var)
				return NULL;
			type = ofc_sema_type_create_primitive(
				spec->type, kind);
			break;

		case OFC_SEMA_TYPE_CHARACTER:
			type = ofc_sema_type_create_character(
				kind, (spec->len_var ? 0 : spec->len));
			break;

		default:
			return NULL;
	}

	if (spec->array)
	{
		type = ofc_sema_type_create_array(
			type, spec->array);
	}

	return type;
}


static bool ofc_sema_type__compare(
	const ofc_sema_type_t* a,
	const ofc_sema_type_t* b)
{
	if (!a || !b)
		return false;

	if (a == b)
		return true;

	if (a->type != b->type)
		return false;

	if ((a->array || b->array)
		&& !ofc_sema_array_compare(
			a->array, b->array))
		return false;

	switch (a->type)
	{
		case OFC_SEMA_TYPE_STRUCTURE:
			return ofc_sema_structure_compare(
				a->structure, b->structure);

		case OFC_SEMA_TYPE_POINTER:
		case OFC_SEMA_TYPE_FUNCTION:
			return ofc_sema_type_compare(
				a->subtype, b->subtype);

		case OFC_SEMA_TYPE_CHARACTER:
			if (a->len != b->len)
				return false;
			break;

		case OFC_SEMA_TYPE_SUBROUTINE:
			return true;

		default:
			break;
	}

	return true;
}

bool ofc_sema_type_compare(
	const ofc_sema_type_t* a,
	const ofc_sema_type_t* b)
{
	if (!ofc_sema_type__compare(a, b))
		return false;
	return (a->kind == b->kind);
}

bool ofc_sema_type_compatible(
	const ofc_sema_type_t* a,
	const ofc_sema_type_t* b)
{
	if (!ofc_sema_type__compare(a, b))
		return false;

	unsigned asize, bsize;
	return (ofc_sema_type_base_size(a, &asize)
		&& ofc_sema_type_base_size(b, &bsize)
		&& (asize == bsize));
}


bool ofc_sema_type_base_size(
	const ofc_sema_type_t* type,
	unsigned* size)
{
	if (!type)
		return false;

	unsigned s = 0, def = 0;
	switch (type->type)
	{
		case OFC_SEMA_TYPE_LOGICAL:
			def = ofc_target_logical_size_get();
			break;

		case OFC_SEMA_TYPE_INTEGER:
			def = ofc_target_integer_size_get();
			break;

		case OFC_SEMA_TYPE_REAL:
		case OFC_SEMA_TYPE_COMPLEX:
			def = ofc_target_real_size_get();
			break;

		case OFC_SEMA_TYPE_BYTE:
			s = 1;
			break;

		case OFC_SEMA_TYPE_CHARACTER:
			def = 1;
			break;

		case OFC_SEMA_TYPE_STRUCTURE:
			if (!ofc_sema_structure_size(
				type->structure, &s))
				return false;
			break;

		case OFC_SEMA_TYPE_POINTER:
			s = ofc_target_pointer_size_get();
			break;

		default:
			return false;
	}

	if (def > 0)
	{
		if (!ofc_sema_type_kind_size(
			def, type->kind, &s))
			return false;
	}

	if (type->type == OFC_SEMA_TYPE_COMPLEX)
		s *= 2;

	if (s == 0)
		return false;

	if (size) *size = s;
	return true;
}

bool ofc_sema_type_size(
	const ofc_sema_type_t* type,
	unsigned* size)
{
	unsigned s;
	if (!ofc_sema_type_base_size(type, &s))
		return false;

	if ((type->type == OFC_SEMA_TYPE_CHARACTER)
		&& (type->len == 0))
		return false;

	if (type->len > 1)
		s *= type->len;

	if (type->array)
	{
		unsigned count;
		if (!ofc_sema_array_total(
			type->array, &count))
			return false;
		s *= count;
	}

	if (s == 0)
		return false;

	if (size) *size = s;
	return true;
}

bool ofc_sema_type_elem_count(
	const ofc_sema_type_t* type,
	unsigned* count)
{
	if (!type)
		return false;

	unsigned c;
	switch (type->type)
	{
		case OFC_SEMA_TYPE_LOGICAL:
		case OFC_SEMA_TYPE_INTEGER:
		case OFC_SEMA_TYPE_REAL:
		case OFC_SEMA_TYPE_COMPLEX:
		case OFC_SEMA_TYPE_BYTE:
		case OFC_SEMA_TYPE_CHARACTER:
		case OFC_SEMA_TYPE_POINTER:
			c = 1;
			break;

		case OFC_SEMA_TYPE_STRUCTURE:
			if (!ofc_sema_structure_elem_count(
				type->structure, &c))
				return false;
			break;

		default:
			return false;
	}

	if (type->array)
	{
		unsigned e;
		if (!ofc_sema_array_total(
			type->array, &e))
			return false;
		c *= e;
	}

	if (count) *count = c;
	return true;
}

bool ofc_sema_type_is_integer(const ofc_sema_type_t* type)
{
	if (!type)
		return false;

	switch (type->type)
	{
		case OFC_SEMA_TYPE_INTEGER:
		case OFC_SEMA_TYPE_BYTE:
			return true;
		default:
			break;
	}

	return false;
}

bool ofc_sema_type_is_scalar(const ofc_sema_type_t* type)
{
	if (!type)
		return false;

	switch (type->type)
	{
		case OFC_SEMA_TYPE_REAL:
		case OFC_SEMA_TYPE_INTEGER:
		case OFC_SEMA_TYPE_BYTE:
			return true;
		default:
			break;
	}

	return false;
}

bool ofc_sema_type_is_logical(const ofc_sema_type_t* type)
{
	if (!type)
		return false;

	switch (type->type)
	{
		case OFC_SEMA_TYPE_LOGICAL:
		case OFC_SEMA_TYPE_BYTE:
			return true;
		default:
			break;
	}

	return false;
}

bool ofc_sema_type_is_character(
	const ofc_sema_type_t* type)
{
	return (type && (type->type == OFC_SEMA_TYPE_CHARACTER));
}

bool ofc_sema_type_is_array(const ofc_sema_type_t* type)
{
	return (type && type->array);
}

bool ofc_sema_type_is_structure(const ofc_sema_type_t* type)
{
	return (type && (type->type == OFC_SEMA_TYPE_STRUCTURE));
}

bool ofc_sema_type_is_composite(const ofc_sema_type_t* type)
{
	return (ofc_sema_type_is_array(type)
		|| ofc_sema_type_is_structure(type));
}


bool ofc_sema_type_is_subroutine(const ofc_sema_type_t* type)
{
	return (type && (type->type == OFC_SEMA_TYPE_SUBROUTINE));
}

bool ofc_sema_type_is_function(const ofc_sema_type_t* type)
{
	return (type && (type->type == OFC_SEMA_TYPE_FUNCTION));
}

bool ofc_sema_type_is_procedure(const ofc_sema_type_t* type)
{
	return (ofc_sema_type_is_subroutine(type)
		|| ofc_sema_type_is_function(type));
}


const ofc_sema_type_t* ofc_sema_type_base(
	const ofc_sema_type_t* type)
{
	if (!type)
		return NULL;

	if (type->array)
	{
		type = ofc_sema_type__create(
			type->type, type->kind, type->len,
			NULL, type->subtype, type->structure);
		return type;
	}

	switch (type->type)
	{
		case OFC_SEMA_TYPE_POINTER:
		case OFC_SEMA_TYPE_FUNCTION:
			return type->subtype;

		case OFC_SEMA_TYPE_SUBROUTINE:
			return NULL;

		default:
			break;
	}

	return type;
}



static unsigned umax(unsigned a, unsigned b)
	{ return (a > b ? a : b); }

const ofc_sema_type_t* ofc_sema_type_promote(
	const ofc_sema_type_t* a,
	const ofc_sema_type_t* b)
{
	if (!a) return b;
	if (!b) return a;

	if (ofc_sema_type_compare(a, b))
		return a;

	/* TODO - Support type promotion of arrays. */
	if (a->array || b->array)
		return NULL;

	/* BYTE is always promoted. */
	if (a->type == OFC_SEMA_TYPE_BYTE)
		return b;
	if (b->type == OFC_SEMA_TYPE_BYTE)
		return a;

	unsigned asize, bsize;
	if (!ofc_sema_type_base_size(a, &asize)
		|| !ofc_sema_type_base_size(b, &bsize))
		return NULL;

	if (a->type == b->type)
		return (asize > bsize ? a : b);

	if (a->type == OFC_SEMA_TYPE_COMPLEX)
		asize /= 2;
	if (b->type == OFC_SEMA_TYPE_COMPLEX)
		bsize /= 2;

	unsigned size = umax(asize, bsize);
	unsigned kind = (3 * size);

	bool logical = ((a->type == OFC_SEMA_TYPE_LOGICAL)
		|| (b->type == OFC_SEMA_TYPE_LOGICAL));
	bool integer = ((a->type == OFC_SEMA_TYPE_INTEGER)
		|| (b->type == OFC_SEMA_TYPE_INTEGER));
	bool real = ((a->type == OFC_SEMA_TYPE_REAL)
		|| (b->type == OFC_SEMA_TYPE_REAL));
	bool complex = ((a->type == OFC_SEMA_TYPE_COMPLEX)
		|| (b->type == OFC_SEMA_TYPE_COMPLEX));

	/* Promoted types ignore decl attributes. */

	if (logical && integer)
	{
		return ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_INTEGER, kind);
	}
	else if (real && (logical || integer))
	{
		/* TODO - Take mantissa bits into account. */

		return ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_REAL, kind);
	}
	else if (complex && (real || logical || integer))
	{
		return ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_COMPLEX, kind);
	}

	/* We can't promote characters, arrays, structures or pointers. */

	return NULL;
}

bool ofc_sema_type_cast_valid(
	const ofc_sema_type_t* a,
	const ofc_sema_type_t* b)
{
	if (!a) return false;
	if (!b) return false;

	if (a->type == b->type)
		return true;

	/* BYTE can always be cast. */
	if ((a->type == OFC_SEMA_TYPE_BYTE)
		|| (b->type == OFC_SEMA_TYPE_BYTE))
		return true;

	bool logical = ((a->type == OFC_SEMA_TYPE_LOGICAL)
		|| (b->type == OFC_SEMA_TYPE_LOGICAL));
	bool integer = ((a->type == OFC_SEMA_TYPE_INTEGER)
		|| (b->type == OFC_SEMA_TYPE_INTEGER));
	bool real = ((a->type == OFC_SEMA_TYPE_REAL)
		|| (b->type == OFC_SEMA_TYPE_REAL));
	bool complex = ((a->type == OFC_SEMA_TYPE_COMPLEX)
		|| (b->type == OFC_SEMA_TYPE_COMPLEX));

	if ((logical && integer)
		|| (real && (logical || integer))
		|| (complex && (real || logical || integer)))
		return true;

	/* We can't cast characters, arrays, structures or pointers. */

	return false;
}

bool ofc_sema_type_cast_is_lossless(
	const ofc_sema_type_t* base,
	const ofc_sema_type_t* target)
{
	if (!base || !target)
		return false;

	if (ofc_sema_type_compare(base, target))
		return true;

	unsigned bsize, tsize;
	if (!ofc_sema_type_base_size(base, &bsize)
		|| !ofc_sema_type_base_size(target, &tsize))
		return false;

	switch (base->type)
	{
		case OFC_SEMA_TYPE_INTEGER:
			switch (target->type)
			{
				case OFC_SEMA_TYPE_INTEGER:
					return (tsize > bsize);

				case OFC_SEMA_TYPE_REAL:
				case OFC_SEMA_TYPE_COMPLEX:
					switch (tsize)
					{
						case 4:
							return (bsize <= 3);
						case 8:
							return (bsize <= 5);
						case 10:
							return (bsize <= 8);
						default:
							break;
					}
					break;

				case OFC_SEMA_TYPE_BYTE:
					return (bsize == 1);

				default:
					break;
			}
			break;

		case OFC_SEMA_TYPE_REAL:
			switch (target->type)
			{
				case OFC_SEMA_TYPE_REAL:
				case OFC_SEMA_TYPE_COMPLEX:
					return (tsize >= bsize);

				default:
					break;
			}
			break;

		case OFC_SEMA_TYPE_COMPLEX:
			switch (target->type)
			{
				case OFC_SEMA_TYPE_COMPLEX:
					return (tsize >= bsize);

				default:
					break;
			}
			break;

		case OFC_SEMA_TYPE_BYTE:
			switch (target->type)
			{
				case OFC_SEMA_TYPE_INTEGER:
					return (tsize >= 1);

				case OFC_SEMA_TYPE_REAL:
				case OFC_SEMA_TYPE_COMPLEX:
				case OFC_SEMA_TYPE_BYTE:
					return true;

				default:
					break;
			}
			break;

		case OFC_SEMA_TYPE_CHARACTER:
			return ((target->type == OFC_SEMA_TYPE_CHARACTER)
				&& (tsize >= bsize)
				&& (target->len >= base->len));

		default:
			break;
	}

	return false;
}

bool ofc_sema_type_print(
	ofc_colstr_t* cs,
	const ofc_sema_type_t* type)
{
	if (!cs || !type)
		return false;

	if (type->type >= OFC_SEMA_TYPE_COUNT)
		return false;

	bool print_double = false;
	switch (type->type)
	{
		case OFC_SEMA_TYPE_REAL:
		case OFC_SEMA_TYPE_COMPLEX:
			print_double = (type->kind == 2);
			break;
		default:
			break;
	}

	bool kind_abs = false;
	if (print_double)
	{
		if (!ofc_colstr_atomic_writef(cs, "DOUBLE")
			|| !ofc_colstr_atomic_writef(cs, " "))
			return false;

		if (!ofc_colstr_atomic_writef(cs,
			(type->type == OFC_SEMA_TYPE_COMPLEX
				? "COMPLEX" : "PRECISION")))
			return false;
	}
	else
	{
		if (!ofc_colstr_atomic_writef(cs, "%s",
			ofc_sema_type__name[type->type]))
			return false;

		kind_abs = ofc_sema_type__kind_absolute(type->kind);
		if (kind_abs)
		{
			if (!ofc_colstr_atomic_writef(cs, "*")
				|| !ofc_colstr_atomic_writef(
					cs, "%u", (type->kind / 3)))
				return false;
		}
	}

	bool print_len = ((type->type == OFC_SEMA_TYPE_CHARACTER)
		&& (type->len != 1));
	bool print_kind = ((type->kind != 1) && !kind_abs && !print_double);

	if (print_len || print_kind)
	{
		if (!ofc_colstr_atomic_writef(cs, "("))
			return false;

		if (print_kind)
		{
			if (!ofc_colstr_atomic_writef(cs, "KIND")
				|| !ofc_colstr_atomic_writef(cs, "=")
				|| !ofc_colstr_atomic_writef(cs, "%u", type->kind))
				return false;

		}

		if (print_len)
		{
			if (print_kind)
			{
				if (!ofc_colstr_atomic_writef(cs, ",")
					|| !ofc_colstr_atomic_writef(cs, " ")
					|| !ofc_colstr_atomic_writef(cs, "LEN")
					|| !ofc_colstr_atomic_writef(cs, "="))
					return false;
			}

			if (type->len == 0
				? !ofc_colstr_atomic_writef(cs, "*")
				: !ofc_colstr_atomic_writef(cs, "%u", type->len))
				return false;
		}

		if (!ofc_colstr_atomic_writef(cs, ")"))
			return false;
	}

	return true;
}
