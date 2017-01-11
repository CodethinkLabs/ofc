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
	ofc_sema_kind_e kind)
{
	if (kind == OFC_SEMA_KIND_NONE)
		return false;

	if ((kind & (kind - 1)) == 0)
		return true;

	if ((kind % 3) == 0)
		return true;

	if (kind == OFC_SEMA_KIND_POINTER)
		return true;

	if ((kind % 5) == 0)
		return ofc_sema_type__kind_valid(kind / 5);

	return false;
}

static bool ofc_sema_type__kind_absolute(
	ofc_sema_kind_e kind)
{
	return ((kind > 0) && ((kind % 3) == 0));
}

bool ofc_sema_type_kind_size(
	unsigned def, ofc_sema_kind_e kind, unsigned* size)
{
	if (ofc_sema_type__kind_absolute(kind))
	{
		if (size) *size = (kind / 3);
		return true;
	}

	if (kind == OFC_SEMA_KIND_POINTER)
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

ofc_sema_kind_e ofc_sema_type_get_kind(
	const ofc_sema_type_t* type)
{
	if (!type) return -1;

	switch (type->type)
	{
		case OFC_SEMA_TYPE_LOGICAL:
		case OFC_SEMA_TYPE_INTEGER:
		case OFC_SEMA_TYPE_REAL:
		case OFC_SEMA_TYPE_COMPLEX:
		case OFC_SEMA_TYPE_BYTE:
		case OFC_SEMA_TYPE_CHARACTER:
			return type->kind;
		default:
			break;
	}

	return -1;
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

	"POINTER",

	"FUNCTION",
	"SUBROUTINE",

	"TYPE",
	"RECORD",

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

	free(type);
}

uint8_t ofc_sema_type_hash(
	const ofc_sema_type_t* type)
{
	if (!type)
		return 0;

	uint8_t hash = type->type;

	switch (type->type)
	{
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
	ofc_sema_kind_e kind, unsigned len, bool len_var,
	const ofc_sema_type_t* subtype)
{
	switch (type)
	{
		case OFC_SEMA_TYPE_BYTE:
			if ((len != 0) || len_var)
				return NULL;
			if ((kind != OFC_SEMA_KIND_NONE)
				&& (kind != OFC_SEMA_KIND_DEFAULT)
				&& (kind != OFC_SEMA_KIND_1_BYTE))
				return NULL;
			break;

		case OFC_SEMA_TYPE_CHARACTER:
			if ((kind != OFC_SEMA_KIND_NONE)
				&& !ofc_sema_type__kind_valid(kind))
				return NULL;
			break;

		case OFC_SEMA_TYPE_LOGICAL:
		case OFC_SEMA_TYPE_INTEGER:
		case OFC_SEMA_TYPE_REAL:
		case OFC_SEMA_TYPE_COMPLEX:
			if ((kind != OFC_SEMA_KIND_NONE)
				&& !ofc_sema_type__kind_valid(kind))
				return NULL;
			if ((len != 0) || len_var)
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
		};

	switch (type)
	{
		case OFC_SEMA_TYPE_POINTER:
		case OFC_SEMA_TYPE_FUNCTION:
			stype.subtype = subtype;
			break;
		default:
			stype.kind = kind;
			stype.len = len;
			stype.len_var = len_var;
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
			stype.kind = OFC_SEMA_KIND_DEFAULT;
		}
	}

	const ofc_sema_type_t* gtype
		= ofc_hashmap_find(
			ofc_sema_type__map, &stype);
	if (gtype) return gtype;

	ofc_sema_type_t* ntype
		= (ofc_sema_type_t*)malloc(
			sizeof(ofc_sema_type_t));
	if (!ntype) return NULL;
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
	ofc_sema_kind_e kind)
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
		type, kind, 0, false, NULL);
}

const ofc_sema_type_t* ofc_sema_type_create_character(
	ofc_sema_kind_e kind, unsigned len, bool len_var)
{
	return ofc_sema_type__create(
		OFC_SEMA_TYPE_CHARACTER, kind, len, len_var, NULL);
}

const ofc_sema_type_t* ofc_sema_type_create_pointer(
	ofc_sema_type_t* target)
{
	return ofc_sema_type__create(
		OFC_SEMA_TYPE_POINTER, OFC_SEMA_KIND_NONE, 0, false, target);
}

const ofc_sema_type_t* ofc_sema_type_create_function(
	const ofc_sema_type_t* type)
{
	if (!type)
		return NULL;

	if (ofc_sema_type_is_procedure(type))
		return NULL;

	return ofc_sema_type__create(
		OFC_SEMA_TYPE_FUNCTION, OFC_SEMA_KIND_NONE, 0, false, type);
}


static const ofc_sema_type_t* ofc_sema__type(
	ofc_sema_scope_t* scope,
	const ofc_parse_type_t* ptype,
	ofc_sema_structure_t** structure,
	bool type_scan)
{
    if (!ptype)
		return NULL;

	ofc_sema_structure_t* struct_type = NULL;
	ofc_sema_type_e etype;
	switch (ptype->type)
	{
		case OFC_PARSE_TYPE_NONE:
			return NULL;

		case OFC_PARSE_TYPE_LOGICAL:
			etype = OFC_SEMA_TYPE_LOGICAL;
			break;

		case OFC_PARSE_TYPE_CHARACTER:
			etype = OFC_SEMA_TYPE_CHARACTER;
			break;

		case OFC_PARSE_TYPE_INTEGER:
			etype = OFC_SEMA_TYPE_INTEGER;
			break;

		case OFC_PARSE_TYPE_REAL:
		case OFC_PARSE_TYPE_DOUBLE_PRECISION:
			etype = OFC_SEMA_TYPE_REAL;
			break;

		case OFC_PARSE_TYPE_COMPLEX:
		case OFC_PARSE_TYPE_DOUBLE_COMPLEX:
			etype = OFC_SEMA_TYPE_COMPLEX;
			break;

		case OFC_PARSE_TYPE_BYTE:
			etype = OFC_SEMA_TYPE_BYTE;
			break;

		case OFC_PARSE_TYPE_TYPE:
			etype = OFC_SEMA_TYPE_TYPE;
			struct_type = ofc_sema_scope_derived_type_find(
				scope, ptype->type_name.string);
			if (!struct_type)
			{
				struct_type = ofc_sema_scope_structure_find(
					scope, ptype->type_name.string);
				if (!struct_type)
				{
					ofc_sparse_ref_error(ptype->type_name,
						"Referencing undefined TYPE name");
					return NULL;
				}
				ofc_sparse_ref_warning(ptype->type_name,
					"Referencing STRUCTURE in TYPE declaration");
			}
			break;

		case OFC_PARSE_TYPE_RECORD:
			etype = OFC_SEMA_TYPE_RECORD;
			break;

		default:
			return NULL;
	}

	unsigned len     = 0;
	bool     len_var = ptype->count_var;
	if (ptype->count_expr && !type_scan)
	{
		if (len_var)
		{
			ofc_sparse_ref_error(ptype->src,
				"Type LEN specified as both fixed and variable");
			return NULL;
		}

		ofc_sema_expr_t* expr
			= ofc_sema_expr(scope, ptype->count_expr);

		bool resolved = ofc_sema_expr_resolve_uint(expr, &len);
		ofc_sema_expr_delete(expr);

		if (!resolved)
		{
			ofc_sparse_ref_error(ptype->count_expr->src,
				"Type LEN expression couldn't be resolved");
			return NULL;
		}

		if (len == 0)
		{
			ofc_sparse_ref_error(ptype->count_expr->src,
				"Type LEN must be greater than zero");
			return NULL;
		}
	}

	ofc_sema_kind_e kind = OFC_SEMA_KIND_NONE;
	switch (ptype->type)
	{
		case OFC_PARSE_TYPE_DOUBLE_PRECISION:
		case OFC_PARSE_TYPE_DOUBLE_COMPLEX:
			if (ptype->size != 0)
			{
				ofc_sparse_ref_error(ptype->count_expr->src,
					"Can't specify size of DOUBLE type");
				return NULL;
			}
			kind = OFC_SEMA_KIND_DOUBLE;
			break;
		default:
			if (ptype->size > 0)
				kind = (OFC_SEMA_KIND_1_BYTE * ptype->size);
			break;
	}

	if (ptype->params)
	{
		unsigned i;
		for (i = 0; i < ptype->params->count; i++)
		{
			/* TODO - Handle unnamed kind, len */
			if (ofc_str_ref_equal_strz_ci(
				ptype->params->call_arg[i]->name.string, "LEN"))
			{
				if (ptype->params->call_arg[i]->type
					== OFC_PARSE_CALL_ARG_ASTERISK)
				{
					if (len > 0)
					{
						ofc_sparse_ref_error(ptype->src,
							"Type LEN specified as both fixed and variable");
						return NULL;
					}

					len_var = true;
				}
				else if (!type_scan)
				{
					if (ptype->params->call_arg[i]->type
						!= OFC_PARSE_CALL_ARG_EXPR)
						return NULL;

					ofc_sema_expr_t* expr = ofc_sema_expr(
						scope, ptype->params->call_arg[i]->expr);
					if (!expr) return NULL;

					unsigned plen;
					bool resolved = ofc_sema_expr_resolve_uint(expr, &plen);
					ofc_sema_expr_delete(expr);
					if (!resolved)
					{
						ofc_sparse_ref_error(ptype->src,
							"Type LEN expression couldn't be resolved");
						return NULL;
					}

					if (plen == 0)
					{
						ofc_sparse_ref_error(ptype->src,
							"Type LEN paramater must be greater than zero");
						return NULL;
					}

					if (len_var)
					{
						ofc_sparse_ref_error(ptype->src,
							"Type LEN specified as both fixed and variable");
						return NULL;
					}
					else if (len > 0)
					{
						if(len != plen)
						{
							ofc_sparse_ref_error(ptype->src,
								"Conflicting type LEN specifications");
							return NULL;
						}

						ofc_sparse_ref_warning(ptype->src,
							"Multiple type LEN specifications");
					}

					len = plen;
				}
			}
			else if (ofc_str_ref_equal_strz_ci(
				ptype->params->call_arg[i]->name.string, "KIND"))
			{
				if (ptype->params->call_arg[i]->type
					!= OFC_PARSE_CALL_ARG_EXPR)
					return NULL;

				ofc_sema_expr_t* expr = ofc_sema_expr(
					scope, ptype->params->call_arg[i]->expr);
				if (!expr) return NULL;

				unsigned pkind;
				bool resolved = ofc_sema_expr_resolve_uint(expr, &pkind);
				ofc_sema_expr_delete(expr);
				if (!resolved)
				{
					ofc_sparse_ref_error(ptype->src,
						"Type KIND expression couldn't be resolved");
					return NULL;
				}

				if (pkind == OFC_SEMA_KIND_NONE)
				{
					ofc_sparse_ref_error(ptype->src,
						"Type KIND paramater must be greater than zero");
					return NULL;
				}

				if (kind != OFC_SEMA_KIND_NONE)
				{
					if (kind != pkind)
					{
						ofc_sparse_ref_error(ptype->src,
							"Conflicting type KIND specifications");
						return NULL;
					}

					ofc_sparse_ref_warning(ptype->src,
						"Multiple type KIND specifications");
				}

				kind = pkind;
			}
			else
			{
				ofc_sparse_ref_error(ptype->src,
					"Unknown parameter in type");
				return NULL;
			}
		}
	}

	if (etype == OFC_SEMA_TYPE_BYTE)
	{
		if ((kind != OFC_SEMA_KIND_NONE)
			&& (kind != OFC_SEMA_KIND_DEFAULT)
			&& (kind != OFC_SEMA_KIND_1_BYTE))
		{
			ofc_sparse_ref_error(ptype->src,
				"BYTE kind must represent a size of 1");
			return NULL;
		}
	}

	const ofc_sema_type_t* type;
	switch (etype)
	{
		case OFC_SEMA_TYPE_LOGICAL:
		case OFC_SEMA_TYPE_INTEGER:
		case OFC_SEMA_TYPE_REAL:
		case OFC_SEMA_TYPE_COMPLEX:
		case OFC_SEMA_TYPE_BYTE:
			type = ofc_sema_type_create_primitive(
				etype, kind);
			break;

		case OFC_SEMA_TYPE_CHARACTER:
			type = ofc_sema_type_create_character(
				kind, len, len_var);
			break;

		case OFC_SEMA_TYPE_TYPE:
			type = ofc_sema_type_type();
			break;

		case OFC_SEMA_TYPE_RECORD:
			type = ofc_sema_type_record();
			break;

		default:
			return NULL;
	}
	if (!type) return NULL;

	if (structure) *structure = struct_type;
	return type;
}

const ofc_sema_type_t* ofc_sema_type(
	ofc_sema_scope_t* scope,
	const ofc_parse_type_t* ptype,
	ofc_sema_structure_t** structure)
{
	return ofc_sema__type(
		scope, ptype, structure, false);
}

const ofc_sema_type_t* ofc_sema_type_scan(
	ofc_sema_scope_t* scope,
	const ofc_parse_type_t* ptype,
	ofc_sema_structure_t** structure)
{
	return ofc_sema__type(
		scope, ptype, structure, true);
}


const ofc_sema_type_t* ofc_sema_type_set_kind(
	const ofc_sema_type_t* type, ofc_sema_kind_e kind)
{
	if (!type)
		return NULL;

	switch (type->type)
	{
		case OFC_SEMA_TYPE_LOGICAL:
		case OFC_SEMA_TYPE_INTEGER:
		case OFC_SEMA_TYPE_REAL:
		case OFC_SEMA_TYPE_COMPLEX:
		case OFC_SEMA_TYPE_BYTE:
			return ofc_sema_type_create_primitive(
				type->type, kind);
		case OFC_SEMA_TYPE_CHARACTER:
			return ofc_sema_type_create_character(
				kind, type->len, type->len_var);
		default:
			break;
	}

	return NULL;
}

const ofc_sema_type_t* ofc_sema_type_set_len(
	const ofc_sema_type_t* type, unsigned len, bool len_var)
{
	if (!type)
		return NULL;

	if (type->type != OFC_SEMA_TYPE_CHARACTER)
		return NULL;

	return ofc_sema_type_create_character(
		type->kind, len, len_var);
}


const ofc_sema_type_t* ofc_sema_type_logical_default(void)
{
	static const ofc_sema_type_t* logical = NULL;

	if (!logical)
	{
		logical = ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_LOGICAL,
			OFC_SEMA_KIND_DEFAULT);
	}

	return logical;
}

const ofc_sema_type_t* ofc_sema_type_integer_default(void)
{
	static const ofc_sema_type_t* integer = NULL;

	if (!integer)
	{
		integer = ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_INTEGER,
			OFC_SEMA_KIND_DEFAULT);
	}

	return integer;
}

const ofc_sema_type_t* ofc_sema_type_real_default(void)
{
	static const ofc_sema_type_t* real = NULL;

	if (!real)
	{
		real = ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_REAL,
			OFC_SEMA_KIND_DEFAULT);
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
			OFC_SEMA_TYPE_REAL,
			OFC_SEMA_KIND_DOUBLE);
	}

	return dbl;
}

const ofc_sema_type_t* ofc_sema_type_complex_default(void)
{
	static const ofc_sema_type_t* complex = NULL;

	if (!complex)
	{
		complex = ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_COMPLEX,
			OFC_SEMA_KIND_DEFAULT);
	}

	return complex;
}

const ofc_sema_type_t* ofc_sema_type_double_complex_default(void)
{
	static const ofc_sema_type_t* dbl_complex = NULL;

	if (!dbl_complex)
	{
		const ofc_sema_type_t* real
			= ofc_sema_type_real_default();
		if (!real) return NULL;

		dbl_complex = ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_COMPLEX,
			OFC_SEMA_KIND_DOUBLE);
	}

	return dbl_complex;
}

const ofc_sema_type_t* ofc_sema_type_byte_default(void)
{
	static const ofc_sema_type_t* byte = NULL;

	if (!byte)
	{
		byte = ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_BYTE,
			OFC_SEMA_KIND_DEFAULT);
	}

	return byte;
}

const ofc_sema_type_t* ofc_sema_type_subroutine(void)
{
	static const ofc_sema_type_t* subroutine = NULL;

	if (!subroutine)
	{
		subroutine = ofc_sema_type__create(
			OFC_SEMA_TYPE_SUBROUTINE, OFC_SEMA_KIND_NONE, 0, false, NULL);
	}

	return subroutine;
}

const ofc_sema_type_t* ofc_sema_type_type(void)
{
	static const ofc_sema_type_t* type = NULL;

	if (!type)
	{
		type = ofc_sema_type__create(
			OFC_SEMA_TYPE_TYPE, OFC_SEMA_KIND_NONE, 0, false, NULL);
	}

	return type;
}

const ofc_sema_type_t* ofc_sema_type_record(void)
{
	static const ofc_sema_type_t* type = NULL;

	if (!type)
	{
		type = ofc_sema_type__create(
			OFC_SEMA_TYPE_RECORD, OFC_SEMA_KIND_NONE, 0, false, NULL);
	}

	return type;
}


const ofc_sema_type_t* ofc_sema_type_scalar(
	const ofc_sema_type_t* type)
{
	if (!type)
		return NULL;

	if (ofc_sema_type_is_scalar(type))
		return type;

	if (type->type == OFC_SEMA_TYPE_COMPLEX)
		return ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_REAL, type->kind);

	return NULL;
}


static bool ofc_sema_type__compare(
	bool compat,
	const ofc_sema_type_t* a,
	const ofc_sema_type_t* b)
{
	if (!a || !b)
		return false;

	if (a == b)
		return true;

	if (a->type != b->type)
		return false;

	switch (a->type)
	{
		case OFC_SEMA_TYPE_POINTER:
		case OFC_SEMA_TYPE_FUNCTION:
			return ofc_sema_type__compare(
				compat, a->subtype, b->subtype);

		case OFC_SEMA_TYPE_CHARACTER:
			if (compat)
			{
				if (!a->len_var && !b->len_var
					&& (a->len != b->len))
					return false;
			}
			else if ((a->len != b->len)
				|| (a->len_var != b->len_var))
				return false;
			break;

		case OFC_SEMA_TYPE_SUBROUTINE:
			return true;

		case OFC_SEMA_TYPE_TYPE:
		case OFC_SEMA_TYPE_RECORD:
			return false;

		default:
			break;
	}

	if (compat)
	{
		unsigned asize, bsize;
		return (ofc_sema_type_base_size(a, &asize)
			&& ofc_sema_type_base_size(b, &bsize)
			&& (asize == bsize));
	}

	return (a->kind == b->kind);
}

bool ofc_sema_type_compare(
	const ofc_sema_type_t* a,
	const ofc_sema_type_t* b)
{
	return ofc_sema_type__compare(false, a, b);
}

bool ofc_sema_type_compatible(
	const ofc_sema_type_t* a,
	const ofc_sema_type_t* b)
{
	return ofc_sema_type__compare(true, a, b);
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

	if (type->type == OFC_SEMA_TYPE_CHARACTER)
	{
		if (type->len_var)
			return false;
		s *= type->len;
	}

	if (size) *size = s;
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

bool ofc_sema_type_is_complex(const ofc_sema_type_t* type)
{
	return (type && (type->type == OFC_SEMA_TYPE_COMPLEX));
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


bool ofc_sema_type_is_type(const ofc_sema_type_t* type)
{
	return (type && (type->type == OFC_SEMA_TYPE_TYPE));
}

bool ofc_sema_type_is_record(const ofc_sema_type_t* type)
{
	return (type && (type->type == OFC_SEMA_TYPE_RECORD));
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

	switch (type->type)
	{
		case OFC_SEMA_TYPE_POINTER:
		case OFC_SEMA_TYPE_FUNCTION:
			return type->subtype;

		case OFC_SEMA_TYPE_SUBROUTINE:
		case OFC_SEMA_TYPE_TYPE:
		case OFC_SEMA_TYPE_RECORD:
			return NULL;

		default:
			break;
	}

	return type;
}



static unsigned umax(unsigned a, unsigned b)
	{ return (a > b ? a : b); }

const ofc_sema_type_t* ofc_sema_character_pad_to_type(
	const ofc_sema_type_t* character,
	const ofc_sema_type_t* type)
{
	if (!character || !type
		|| (character->type != OFC_SEMA_TYPE_CHARACTER))
		return NULL;

	unsigned tsize;
	if (!ofc_sema_type_size(type, &tsize))
		return NULL;

	unsigned ksize;
	if (!ofc_sema_type_kind_size(
		1, character->kind, &ksize))
		return NULL;

	unsigned len = (tsize + (ksize - 1)) / ksize;
	if (!character->len_var
		&& (character->len >= len))
		return character;

	return ofc_sema_type_create_character(
		character->kind, len, false);
}

const ofc_sema_type_t* ofc_sema_type_promote(
	const ofc_sema_type_t* a,
	const ofc_sema_type_t* b)
{
	if (!a) return b;
	if (!b) return a;

	if (ofc_sema_type_compare(a, b))
		return a;

	unsigned asize, bsize;
	if (!ofc_sema_type_base_size(a, &asize)
		|| !ofc_sema_type_base_size(b, &bsize))
		return NULL;

	if ((a->type == OFC_SEMA_TYPE_CHARACTER)
		&& ((b->type == OFC_SEMA_TYPE_INTEGER)
			|| (b->type == OFC_SEMA_TYPE_BYTE)))
	{
		if (bsize >= asize)
			return b;
		return ofc_sema_type_create_primitive(
			b->type, (asize * 3));
	}
	else if ((b->type == OFC_SEMA_TYPE_CHARACTER)
		&& ((a->type == OFC_SEMA_TYPE_INTEGER)
			|| (a->type == OFC_SEMA_TYPE_BYTE)))
	{
		return ofc_sema_type_promote(b, a);
	}

	/* BYTE is always promoted. */
	if (a->type == OFC_SEMA_TYPE_BYTE)
		return b;
	if (b->type == OFC_SEMA_TYPE_BYTE)
		return a;

	if (a->type == b->type)
	{
		if (a->type == OFC_SEMA_TYPE_CHARACTER)
		{
			bool len_var = (a->len_var || b->len_var);
			unsigned len = (len_var ? 0
				: (a->len > b->len ? a->len : b->len));
			unsigned kind = (asize > bsize ? a->kind : b->kind);

			return ofc_sema_type_create_character(
				kind, len, len_var);
		}

		return (asize > bsize ? a : b);
	}

	if (a->type == OFC_SEMA_TYPE_COMPLEX)
		asize /= 2;
	if (b->type == OFC_SEMA_TYPE_COMPLEX)
		bsize /= 2;

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
		ofc_sema_kind_e kind
			= (asize > bsize ? a->kind : b->kind);
		return ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_INTEGER, kind);
	}
	else if (real && complex)
	{
		ofc_sema_kind_e kind
			= (asize > bsize ? a->kind : b->kind);
		return ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_COMPLEX, kind);
	}
	else if ((real || complex) && (logical || integer))
	{
		ofc_sema_kind_e k[] =
		{
			OFC_SEMA_KIND_DEFAULT,
			OFC_SEMA_KIND_DOUBLE,
			OFC_SEMA_KIND_QUAD,

			OFC_SEMA_KIND_NONE
		};

		unsigned size = umax(asize, bsize);

		/* TODO - TYPE - Find a better way to type promote. */
		ofc_sema_kind_e kind
			= OFC_SEMA_KIND_NONE;

		unsigned i;
		for (i = 0; k[i] != OFC_SEMA_KIND_NONE; i++)
		{
			unsigned ksize;
			if (!ofc_sema_type_kind_size(
				ofc_target_real_size_get(), k[i], &ksize))
				continue;

			/* TODO - TYPE - Take mantissa bits into account. */
			if (ksize >= size)
			{
				kind = k[i];
				break;
			}
		}
		if (kind == OFC_SEMA_KIND_NONE)
			return NULL;

		return ofc_sema_type_create_primitive(
			(complex ? OFC_SEMA_TYPE_COMPLEX : OFC_SEMA_TYPE_REAL), kind);
	}

	/* We can't promote characters or pointers. */

	return NULL;
}

bool ofc_sema_type_cast_valid(
	const ofc_sema_type_t* from,
	const ofc_sema_type_t* to)
{
	if (!from) return false;
	if (!to  ) return false;

	if (from->type == to->type)
		return true;

	/* CHARACTERs can be cast to INTEGERs. */
	if ((from->type == OFC_SEMA_TYPE_CHARACTER)
		&& ((to->type == OFC_SEMA_TYPE_INTEGER)
			|| (to->type == OFC_SEMA_TYPE_BYTE)))
		return true;

	switch (from->type)
	{
		case OFC_SEMA_TYPE_BYTE:
		case OFC_SEMA_TYPE_LOGICAL:
		case OFC_SEMA_TYPE_INTEGER:
		case OFC_SEMA_TYPE_REAL:
		case OFC_SEMA_TYPE_COMPLEX:
			break;
		default:
			/* We can't cast from anyting else. */
			return false;
	}

	switch (to->type)
	{
		case OFC_SEMA_TYPE_BYTE:
		case OFC_SEMA_TYPE_LOGICAL:
		case OFC_SEMA_TYPE_INTEGER:
		case OFC_SEMA_TYPE_REAL:
		case OFC_SEMA_TYPE_COMPLEX:
			break;
		default:
			/* We can't cast to anyting else. */
			return false;
	}

	/* BYTE can always be cast. */
	if ((from->type == OFC_SEMA_TYPE_BYTE)
		|| (to->type == OFC_SEMA_TYPE_BYTE))
		return true;

	bool logical = ((from->type == OFC_SEMA_TYPE_LOGICAL)
		|| (to->type == OFC_SEMA_TYPE_LOGICAL));
	bool integer = ((from->type == OFC_SEMA_TYPE_INTEGER)
		|| (to->type == OFC_SEMA_TYPE_INTEGER));
	bool real = ((from->type == OFC_SEMA_TYPE_REAL)
		|| (to->type == OFC_SEMA_TYPE_REAL));
	bool complex = ((from->type == OFC_SEMA_TYPE_COMPLEX)
		|| (to->type == OFC_SEMA_TYPE_COMPLEX));

	return ((logical && integer)
		|| (real && (logical || integer))
		|| (complex && (real || logical || integer)));
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
			print_double = (type->kind == OFC_SEMA_KIND_DOUBLE);
			break;
		default:
			break;
	}

	bool kind_abs = false;
	if (print_double)
	{
		if (!ofc_colstr_keyword_atomic_writez(cs, "DOUBLE")
			|| !ofc_colstr_atomic_writef(cs, " "))
			return false;

		if (!ofc_colstr_keyword_atomic_writez(cs,
			(type->type == OFC_SEMA_TYPE_COMPLEX
				? "COMPLEX" : "PRECISION")))
			return false;
	}
	else
	{
		if (!ofc_colstr_keyword_atomic_writez(cs,
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
	bool print_kind = ((type->kind != OFC_SEMA_KIND_DEFAULT)
		&& !kind_abs && !print_double);

	if (print_len || print_kind)
	{
		if (!ofc_colstr_atomic_writef(cs, "("))
			return false;

		if (print_kind)
		{
			if (!ofc_colstr_keyword_atomic_writez(cs, "KIND")
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
					|| !ofc_colstr_keyword_atomic_writez(cs, "LEN")
					|| !ofc_colstr_atomic_writef(cs, "="))
					return false;
			}

			if (type->len_var
				? !ofc_colstr_atomic_writef(cs, "*")
				: !ofc_colstr_atomic_writef(cs, "%u", type->len))
				return false;
		}

		if (!ofc_colstr_atomic_writef(cs, ")"))
			return false;
	}

	return true;
}

bool ofc_sema_type_print_zero(
	ofc_colstr_t* cs,
	const ofc_sema_type_t* type)
{
	if (!cs || !type)
		return false;

	switch (type->type)
	{
		case OFC_SEMA_TYPE_LOGICAL:
			return ofc_colstr_writef(cs, ".FALSE.");

		case OFC_SEMA_TYPE_INTEGER:
		case OFC_SEMA_TYPE_BYTE:
			return ofc_colstr_writef(cs, "0");

		case OFC_SEMA_TYPE_REAL:
			return ofc_colstr_writef(cs, "0.0");

		case OFC_SEMA_TYPE_COMPLEX:
			return ofc_colstr_writef(cs, "(0.0, 0.0)");

		case OFC_SEMA_TYPE_CHARACTER:
			return ofc_colstr_writef(cs, "\"\"");

		default:
			break;
	}

	return false;
}
