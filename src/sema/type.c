#include <ofc/sema.h>
#include <string.h>

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
	"SUBROUTINE",
	"FUNCTION",

	NULL
};

const char* ofc_sema_type_str_rep(
	const ofc_sema_type_e type)
{
	if (type >= OFC_SEMA_TYPE_COUNT)
		return NULL;

	return ofc_sema_type__name[type];
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

	if (type->is_static   ) hash +=  8;
	if (type->is_automatic) hash += 16;
	if (type->is_volatile ) hash += 32;

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
	ofc_sema_array_t* array,
	const ofc_sema_type_t* subtype,
	const ofc_sema_structure_t* structure,
	bool is_static,
	bool is_automatic,
	bool is_volatile)
{
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
			.type         = type,
			.array        = array,
			.is_static    = is_static,
			.is_automatic = is_automatic,
			.is_volatile  = is_volatile,
		};

	switch (type)
	{
		case OFC_SEMA_TYPE_POINTER:
		case OFC_SEMA_TYPE_FUNCTION:
			stype.subtype = subtype;
			break;
		case OFC_SEMA_TYPE_STRUCTURE:
			stype.structure = structure;
			break;
		case OFC_SEMA_TYPE_BYTE:
			stype.kind = 1;
			stype.len = 0;
			break;
		case OFC_SEMA_TYPE_CHARACTER:
			if (kind == 0)
				kind = 1;
			stype.kind = kind;
			stype.len = len;
			break;
		default:
			if (kind == 0)
			{
				/* TODO - Work this out per-kind from lang_opts. */
				kind = 4;
			}
			stype.kind = kind;
			stype.len = 0;
			break;
	}

	/* A LOGICAL*1 is a synonym of BYTE. */
	if ((stype.type == OFC_SEMA_TYPE_LOGICAL)
		&& (stype.kind == 1))
		stype.type = OFC_SEMA_TYPE_BYTE;

	const ofc_sema_type_t* gtype
		= ofc_hashmap_find(
			ofc_sema_type__map, &stype);
	if (gtype)
	{
		ofc_sema_array_delete(array);
		return gtype;
	}

	ofc_sema_type_t* ntype
		= (ofc_sema_type_t*)malloc(
			sizeof(ofc_sema_type_t));
	if (!ntype) return NULL;
	*ntype = stype;

	if (!ofc_hashmap_add(
		ofc_sema_type__map, ntype))
	{
		free(ntype);
		return NULL;
	}

	return ntype;
}

const ofc_sema_type_t* ofc_sema_type_create_primitive(
	ofc_sema_type_e type,
	unsigned kind,
	bool is_static,
	bool is_automatic,
	bool is_volatile)
{
	switch (type)
	{
		case OFC_SEMA_TYPE_LOGICAL:
		case OFC_SEMA_TYPE_INTEGER:
		case OFC_SEMA_TYPE_REAL:
		case OFC_SEMA_TYPE_COMPLEX:
			break;
		case OFC_SEMA_TYPE_BYTE:
			if (kind > 1)
				return NULL;
			break;
		default:
			return NULL;
	}

	return ofc_sema_type__create(
		type, kind, 0,
		NULL, NULL, NULL,
		is_static, is_automatic, is_volatile);
}

const ofc_sema_type_t* ofc_sema_type_create_character(
	unsigned kind, unsigned len,
	bool is_static, bool is_automatic, bool is_volatile)
{
	return ofc_sema_type__create(
		OFC_SEMA_TYPE_CHARACTER, kind, len,
		NULL, NULL, NULL,
		is_static, is_automatic, is_volatile);
}

const ofc_sema_type_t* ofc_sema_type_create_byte(
	bool is_static,
	bool is_automatic,
	bool is_volatile)
{
	return ofc_sema_type_create_primitive(
		OFC_SEMA_TYPE_BYTE, 0,
		is_static, is_automatic, is_volatile);
}

const ofc_sema_type_t* ofc_sema_type_create_structure(
	const ofc_sema_structure_t* structure,
	bool is_static,
	bool is_automatic,
	bool is_volatile)
{
	return ofc_sema_type__create(
		OFC_SEMA_TYPE_STRUCTURE, 0, 0,
		NULL, NULL, structure,
		is_static, is_automatic, is_volatile);
}

const ofc_sema_type_t* ofc_sema_type_create_pointer(
	ofc_sema_type_t* target,
	bool is_static,
	bool is_automatic,
	bool is_volatile)
{
	return ofc_sema_type__create(
		OFC_SEMA_TYPE_POINTER, 0, 0,
		NULL, target, NULL,
		is_static, is_automatic, is_volatile);
}

const ofc_sema_type_t* ofc_sema_type_create_array(
	const ofc_sema_type_t* type,
	ofc_sema_array_t* array,
	bool is_static,
	bool is_automatic,
	bool is_volatile)
{
	if (ofc_sema_type_is_procedure(type))
		return NULL;

	return ofc_sema_type__create(
		type->type, type->kind, type->len,
		array, type->subtype, type->structure,
		is_static, is_automatic, is_volatile);
}

const ofc_sema_type_t* ofc_sema_type_create_function(
	const ofc_sema_type_t* type,
	bool is_static,
	bool is_automatic,
	bool is_volatile)
{
	if (!type)
		return NULL;

	if (ofc_sema_type_is_procedure(type))
		return NULL;

	return ofc_sema_type__create(
		OFC_SEMA_TYPE_FUNCTION, 0, 0,
		NULL, type, NULL,
		is_static, is_automatic, is_volatile);
}


const ofc_sema_type_t* ofc_sema_type_star_len(
	const ofc_sema_type_t* type, unsigned star_len)
{
	if (!type || (star_len == 0))
		return NULL;

	if (type->type == OFC_SEMA_TYPE_CHARACTER)
	{
		if (type->len != 0)
			return NULL;

		return ofc_sema_type__create(
			type->type, type->kind, star_len,
			type->array, type->subtype, type->structure,
			type->is_static, type->is_automatic, type->is_volatile);
	}

	/* TODO - Fail if a star_length has already been set. */

	if (type->type == OFC_SEMA_TYPE_FUNCTION)
	{
		const ofc_sema_type_t* subtype
			= ofc_sema_type_star_len(
				type->subtype, star_len);
		if (!subtype) return NULL;

		return ofc_sema_type_create_function(subtype,
			type->is_static, type->is_automatic, type->is_volatile);
	}

	switch (type->type)
	{
		case OFC_SEMA_TYPE_LOGICAL:
		case OFC_SEMA_TYPE_INTEGER:
		case OFC_SEMA_TYPE_REAL:
		case OFC_SEMA_TYPE_COMPLEX:
			return ofc_sema_type__create(
				type->type, star_len, type->len,
				type->array, type->subtype, type->structure,
				type->is_static, type->is_automatic, type->is_volatile);
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
			OFC_SEMA_TYPE_LOGICAL, 0, false, false, false);
	}

	return logical;
}

const ofc_sema_type_t* ofc_sema_type_integer_default(void)
{
	static const ofc_sema_type_t* integer = NULL;

	if (!integer)
	{
		integer = ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_INTEGER, 0, false, false, false);
	}

	return integer;
}

const ofc_sema_type_t* ofc_sema_type_real_default(void)
{
	static const ofc_sema_type_t* real = NULL;

	if (!real)
	{
		real = ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_REAL, 0, false, false, false);
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
			OFC_SEMA_TYPE_REAL, (real->kind * 2), false, false, false);
	}

	return dbl;
}

const ofc_sema_type_t* ofc_sema_type_complex_default(void)
{
	static const ofc_sema_type_t* complex = NULL;

	if (!complex)
	{
		complex = ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_COMPLEX, 0, false, false, false);
	}

	return complex;
}

const ofc_sema_type_t* ofc_sema_type_byte_default(void)
{
	static const ofc_sema_type_t* byte = NULL;

	if (!byte)
	{
		byte = ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_BYTE, 1, false, false, false);
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
			NULL, NULL,NULL, false, false ,false);
	}

	return subroutine;
}


const ofc_sema_type_t* ofc_sema_type(
	ofc_sema_scope_t* scope,
	const ofc_parse_type_t* ptype)
{
	if (!ptype)
		return NULL;


	unsigned kind = 0;
	unsigned len = 0;
	bool len_var = false;

	if (ptype->params)
	{
		unsigned i;
		for (i = 0; i < ptype->params->count; i++)
		{
			/* TODO - Handle unnamed kind, len */
			if (ofc_str_ref_equal_strz_ci(ptype->params->call_arg[i]->name, "LEN"))
			{
				if ((len != 0) || len_var)
				{
					ofc_sema_scope_error(scope, ptype->src,
						"Type LEN specified multiple times.");
					return NULL;
				}

				if (ptype->params->call_arg[i]->type
					== OFC_PARSE_CALL_ARG_ASTERISK)
				{
					len_var = true;
				}
				else
				{
					if (ptype->params->call_arg[i]->type
						!= OFC_PARSE_CALL_ARG_EXPR)
						return NULL;

					ofc_sema_expr_t* expr = ofc_sema_expr(
						scope, ptype->params->call_arg[i]->expr);
					if (!expr) return NULL;

					bool resolved = ofc_sema_expr_resolve_uint(expr, &len);
					ofc_sema_expr_delete(expr);
					if (!resolved)
					{
						ofc_sema_scope_error(scope, ptype->src,
							"Type LEN expression couldn't be resolved.");
						return NULL;
					}


					if (len == 0)
					{
						ofc_sema_scope_error(scope, ptype->src,
							"Type LEN paramater must be greater than zero.");
						return NULL;
					}
				}
			}
			else if (ofc_str_ref_equal_strz_ci(ptype->params->call_arg[i]->name, "KIND"))
			{
				if (kind != 0)
				{
					ofc_sema_scope_error(scope, ptype->src,
						"Type KIND specified multiple times.");
					return NULL;
				}

				if (ptype->params->call_arg[i]->type
					!= OFC_PARSE_CALL_ARG_EXPR)
					return NULL;

				ofc_sema_expr_t* expr = ofc_sema_expr(
					scope, ptype->params->call_arg[i]->expr);
				if (!expr) return NULL;

				bool resolved = ofc_sema_expr_resolve_uint(expr, &kind);
				ofc_sema_expr_delete(expr);
				if (!resolved)
				{
					ofc_sema_scope_error(scope, ptype->src,
						"Type KIND expression couldn't be resolved.");
					return NULL;
				}


				if (kind == 0)
				{
					ofc_sema_scope_error(scope, ptype->src,
						"Type KIND paramater must be greater than zero.");
					return NULL;
				}
			}
			else
			{
				ofc_sema_scope_error(scope, ptype->src,
					"Unknown parameter in type.");
				return NULL;
			}
		}
	}

	if (ptype->kind > 0)
	{
		if (kind > 0)
		{
			if (ptype->kind == kind)
			{
				ofc_sema_scope_warning(scope, ptype->src,
					"KIND specified multiple times in type.");
			}
			else
			{
				ofc_sema_scope_error(scope, ptype->src,
					"KIND specified differently in multiple places.");
				return NULL;
			}
		}

		kind = ptype->kind;
	}
	else if (kind == 0)
	{
		/* TODO - If KIND is not set, get default from lang_opts. */
		kind = 4;
	}

	switch (ptype->type)
	{
		case OFC_PARSE_TYPE_DOUBLE_PRECISION:
			kind *= 2;
			break;
		case OFC_PARSE_TYPE_DOUBLE_COMPLEX:
			kind *= 2;
			break;
		default:
			break;
	}

	if (ptype->count_var)
	{
		if (len != 0)
		{
			ofc_sema_scope_error(scope, ptype->src,
				"Type LEN specified as both static and variable.");
			return NULL;
		}

		len_var = true;
	}
	else if (ptype->count_expr)
	{
		if (len_var)
		{
			ofc_sema_scope_error(scope, ptype->src,
				"Type LEN specified as both static and variable.");
			return NULL;
		}

		ofc_sema_expr_t* expr
			= ofc_sema_expr(scope, ptype->count_expr);

		unsigned nlen;
		bool resolved = ofc_sema_expr_resolve_uint(expr, &nlen);
		ofc_sema_expr_delete(expr);

		if (!resolved)
		{
			ofc_sema_scope_error(scope, ptype->count_expr->src,
				"Type LEN expression couldn't be resolved.");
			return NULL;
		}

		if (nlen == 0)
		{
			ofc_sema_scope_error(scope, ptype->count_expr->src,
				"Type LEN must be greater than zero");
			return NULL;
		}

		if (len > 0)
		{
			if (len != nlen)
			{
				ofc_sema_scope_error(scope, ptype->src,
					"Type LEN specified differently in multiple places.");
				return NULL;
			}
			else
			{
				ofc_sema_scope_warning(scope, ptype->src,
					"Type LEN specified in multiple places.");
			}
		}

		len = nlen;
	}

	/* TODO - Use LEN parameter to create an array of other types. */
	if ((len != 0) || len_var)
	{
		if (ptype->type != OFC_PARSE_TYPE_CHARACTER)
		{
			ofc_sema_scope_error(scope, ptype->src,
					"LEN parameter only supported for CHARACTER type.");
			return NULL;
		}
	}

	const ofc_sema_type_t* stype = NULL;
	switch (ptype->type)
	{
		case OFC_PARSE_TYPE_LOGICAL:
			stype = ofc_sema_type__create(
				OFC_SEMA_TYPE_LOGICAL, kind, 0,
				NULL, NULL, NULL,
				ptype->attr.is_static,
				ptype->attr.is_automatic,
				ptype->attr.is_volatile);
			break;
		case OFC_PARSE_TYPE_CHARACTER:
			stype = ofc_sema_type__create(
				OFC_SEMA_TYPE_CHARACTER, kind, len,
				NULL, NULL, NULL,
				ptype->attr.is_static,
				ptype->attr.is_automatic,
				ptype->attr.is_volatile);
			break;
		case OFC_PARSE_TYPE_INTEGER:
			stype = ofc_sema_type__create(
				OFC_SEMA_TYPE_INTEGER, kind, 0,
				NULL, NULL, NULL,
				ptype->attr.is_static,
				ptype->attr.is_automatic,
				ptype->attr.is_volatile);
			break;
		case OFC_PARSE_TYPE_REAL:
		case OFC_PARSE_TYPE_DOUBLE_PRECISION:
			stype = ofc_sema_type__create(
				OFC_SEMA_TYPE_REAL, kind, 0,
				NULL, NULL, NULL,
				ptype->attr.is_static,
				ptype->attr.is_automatic,
				ptype->attr.is_volatile);
			break;
		case OFC_PARSE_TYPE_COMPLEX:
		case OFC_PARSE_TYPE_DOUBLE_COMPLEX:
			stype = ofc_sema_type__create(
				OFC_SEMA_TYPE_COMPLEX, kind, 0,
				NULL, NULL, NULL,
				ptype->attr.is_static,
				ptype->attr.is_automatic,
				ptype->attr.is_volatile);
			break;
		case OFC_PARSE_TYPE_BYTE:
			stype = ofc_sema_type__create(
				OFC_SEMA_TYPE_BYTE, kind, 0,
				NULL, NULL, NULL,
				ptype->attr.is_static,
				ptype->attr.is_automatic,
				ptype->attr.is_volatile);
			break;
		case OFC_PARSE_TYPE_TYPE:
			break;

		default:
			return NULL;
	}

	return stype;
}

bool ofc_sema_type_compare(
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

	return (a->kind == b->kind);
}


unsigned ofc_sema_type_size(const ofc_sema_type_t* type)
{
	if (!type)
		return 0;

	unsigned size;
	switch (type->type)
	{
		case OFC_SEMA_TYPE_LOGICAL:
		case OFC_SEMA_TYPE_INTEGER:
		case OFC_SEMA_TYPE_REAL:
			size = type->kind;
			break;

		case OFC_SEMA_TYPE_COMPLEX:
			size = (type->kind * 2);
			break;

		case OFC_SEMA_TYPE_BYTE:
			size = 1;
			break;

		case OFC_SEMA_TYPE_CHARACTER:
			size = (type->kind * type->len);
			break;

		case OFC_SEMA_TYPE_STRUCTURE:
			size = ofc_sema_structure_size(
				type->structure);
			break;

		case OFC_SEMA_TYPE_POINTER:
			/* TODO - Do this based on target arch. */
			size = sizeof(void*);
			break;

		default:
			return 0;
	}

	if (type->array)
		size *= ofc_sema_array_total(type->array);

	return size;
}

unsigned ofc_sema_type_elem_count(const ofc_sema_type_t* type)
{
	if (!type)
		return 0;

	unsigned count;
	switch (type->type)
	{
		case OFC_SEMA_TYPE_LOGICAL:
		case OFC_SEMA_TYPE_INTEGER:
		case OFC_SEMA_TYPE_REAL:
		case OFC_SEMA_TYPE_COMPLEX:
		case OFC_SEMA_TYPE_BYTE:
		case OFC_SEMA_TYPE_CHARACTER:
		case OFC_SEMA_TYPE_POINTER:
			count = 1;
			break;

		case OFC_SEMA_TYPE_STRUCTURE:
			count = ofc_sema_structure_elem_count(
				type->structure);
			break;

		default:
			return 0;
	}

	if (type->array)
		count *= ofc_sema_array_total(type->array);

	return count;
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
			NULL, type->subtype, type->structure,
			type->is_static, type->is_automatic, type->is_volatile);
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

	if (a->type == b->type)
		return (a->kind > b->kind ? a : b);

	/* BYTE is always promoted. */
	if (a->type == OFC_SEMA_TYPE_BYTE)
		return b;
	if (b->type == OFC_SEMA_TYPE_BYTE)
		return a;

	bool logical = ((a->type == OFC_SEMA_TYPE_LOGICAL)
		|| (b->type == OFC_SEMA_TYPE_LOGICAL));
	bool integer = ((a->type == OFC_SEMA_TYPE_INTEGER)
		|| (b->type == OFC_SEMA_TYPE_INTEGER));
	bool real = ((a->type == OFC_SEMA_TYPE_REAL)
		|| (b->type == OFC_SEMA_TYPE_REAL));
	bool complex = ((a->type == OFC_SEMA_TYPE_COMPLEX)
		|| (b->type == OFC_SEMA_TYPE_COMPLEX));

	unsigned kind = umax(a->kind, b->kind);

	/* Promoted types ignore decl attributes. */

	if (logical && integer)
	{
		return ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_INTEGER, kind,
			false, false, false);
	}
	else if (real && (logical || integer))
	{
		return ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_REAL, kind,
			false, false, false);
	}
	else if (complex && (real || logical || integer))
	{
		return ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_COMPLEX, kind,
			false, false, false);
	}

	/* We can't promote characters, arrays, structures or pointers. */

	return NULL;
}


bool ofc_sema_type_cast_is_lossless(
	const ofc_sema_type_t* base,
	const ofc_sema_type_t* target)
{
	if (!base || !target)
		return false;

	if (ofc_sema_type_compare(base, target))
		return true;

    switch (base->type)
	{
		case OFC_SEMA_TYPE_INTEGER:
			switch (target->type)
			{
				case OFC_SEMA_TYPE_INTEGER:
					return (target->kind >= base->kind);

				case OFC_SEMA_TYPE_REAL:
				case OFC_SEMA_TYPE_COMPLEX:
					switch (target->kind)
					{
						case 4:
							return (base->kind <= 3);
						case 8:
							return (base->kind <= 5);
						case 10:
							return (base->kind <= 8);
						default:
							break;
					}
					break;

				case OFC_SEMA_TYPE_BYTE:
					return (base->kind <= 1);

				default:
					break;
			}
			break;

		case OFC_SEMA_TYPE_REAL:
			switch (target->type)
			{
				case OFC_SEMA_TYPE_REAL:
				case OFC_SEMA_TYPE_COMPLEX:
					return (target->kind >= base->kind);

				default:
					break;
			}
			break;

		case OFC_SEMA_TYPE_COMPLEX:
			switch (target->type)
			{
				case OFC_SEMA_TYPE_COMPLEX:
					return (target->kind >= base->kind);

				default:
					break;
			}
			break;

		case OFC_SEMA_TYPE_BYTE:
			switch (target->type)
			{
				case OFC_SEMA_TYPE_INTEGER:
					return (target->kind >= 1);

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
				&& (target->kind >= base->kind)
				&& (target->len >= base->len));

		default:
			break;
	}

	return false;
}
