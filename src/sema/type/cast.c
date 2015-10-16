#include <ofc/sema.h>

static void ofc_sema_type_cast_value__int_int(
	unsigned ikind,
	const void* ivalue,
	unsigned okind,
	void* ovalue)
{
	uintptr_t i;
	for (i = okind; i < ikind; i++)
	{
		if (((const uint8_t*)ivalue)[i] != 0x00)
		{
			/* TODO - Warn about lossy truncation. */
			break;
		}
	}

	if (ovalue)
	{
		if (okind > ikind)
			memset(ovalue, 0x00, okind);
		memcpy(ovalue, ivalue, ikind);
	}
}

static void ofc_sema_type_cast_value__int_byte(
	unsigned ikind,
	const void* ivalue,
	unsigned okind,
	void* ovalue)
{
	if (okind != 1)
		return false;
	return ofc_sema_type_cast_value__int_int(
		ikind, ivalue, 1, ovalue);
}

static void ofc_sema_type_cast_value__int_logical(
	unsigned ikind,
	const void* ivalue,
	unsigned okind,
	void* ovalue)
{
	bool v = false;

	uintptr_t i;
	for (i = okind; i < ikind; i++)
	{
		if (((const uint8_t*)ivalue)[i] != 0x00)
		{
			v = true;
			break;
		}
	}

	if (ovalue)
	{
		memset(ovalue, 0x00, okind);
		((uint8_t*)ovalue)[0] = v;
	}
}

static bool ofc_sema_type_cast_value__int_real(
	unsigned ikind,
	const void* ivalue,
	unsigned okind,
	void* ovalue)
{
	if (ikind > 8)
		return false;

	int64_t v = 0;
	memcpy(&v, ivalue, ikind);

	if (okind == 4)
	{
		float ov = (float)v;

		if ((int64_t)ov != v)
		{
			/* TODO - Warn that conversion was lossy. */
		}

		if (ovalue)
			memcpy(ovalue, ov, okind);
	}
	else if (okind == 8)
	{
		double ov = (double)v;

		if ((int64_t)ov != v)
		{
			/* TODO - Warn that conversion was lossy. */
		}

		if (ovalue)
			memcpy(ovalue, ov, okind);
	}
	else if (okind == 10)
	{
		long double ov = (long double)v;

		/* int64 to long double should never be lossy. */

		if (ovalue)
			memcpy(ovalue, ov, okind);
	}
	else
	{
		return false;
	}

	return true;
}

static bool ofc_sema_type_cast_value__int_complex(
	unsigned ikind,
	const void* ivalue,
	unsigned okind,
	void* ovalue)
{
	uint8_t v[okind];
	if(!ofc_sema_type_cast_value__int_real(
		ikind, ivalue, okind, v))
		return false;

	return ofc_sema_type_cast_value__real_complex(
		okind, &v, okind, ovalue);
}

static void ofc_sema_type_cast_value__byte_int(
	unsigned ikind,
	const void* ivalue,
	unsigned okind,
	void* ovalue)
{
	if (ikind != 1)
		return false;
	return ofc_sema_type_cast_value__int_int(
		ikind, ivalue, okind, ovalue);
}

static void ofc_sema_type_cast_value__byte_byte(
	unsigned ikind,
	const void* ivalue,
	unsigned okind,
	void* ovalue)
{
	if (ikind != 1)
		return false;
	return ofc_sema_type_cast_value__int_byte(
		ikind, ivalue, okind, ovalue);
}

static void ofc_sema_type_cast_value__byte_logical(
	unsigned ikind,
	const void* ivalue,
	unsigned okind,
	void* ovalue)
{
	if (ikind != 1)
		return false;
	return ofc_sema_type_cast_value__int_logical(
		ikind, ivalue, okind, ovalue);
}

static bool ofc_sema_type_cast_value__byte_real(
	unsigned ikind,
	const void* ivalue,
	unsigned okind,
	void* ovalue)
{
	if (ikind != 1)
		return false;
	return ofc_sema_type_cast_value__int_real(
		ikind, ivalue, okind, ovalue);
}

static bool ofc_sema_type_cast_value__byte_complex(
	unsigned ikind,
	const void* ivalue,
	unsigned okind,
	void* ovalue)
{
	if (ikind != 1)
		return false;
	return ofc_sema_type_cast_value__int_complex(
		ikind, ivalue, okind, ovalue);
}


static void ofc_sema_type_cast_value__logical_int(
	unsigned ikind,
	const void* ivalue,
	unsigned okind,
	void* ovalue)
{
	return ofc_sema_type_cast_value__int_logical(
		ikind, ivalue, okind, ovalue);
}

static void ofc_sema_type_cast_value__logical_byte(
	unsigned ikind,
	const void* ivalue,
	unsigned okind,
	void* ovalue)
{
	if (okind != 1)
		return false;
	return ofc_sema_type_cast_value__logical_int(
		ikind, ivalue, 1, ovalue);
}

static void ofc_sema_type_cast_value__logical_logical(
	unsigned ikind,
	const void* ivalue,
	unsigned okind,
	void* ovalue)
{
	return ofc_sema_type_cast_value__logical_int(
		ikind, ivalue, okind, ovalue);
}

static void ofc_sema_type_cast_value__logical_real(
	unsigned ikind,
	const void* ivalue,
	unsigned okind,
	void* ovalue)
{
	int64_t v;
	if(!ofc_sema_type_cast_value__logical_int(
		ikind, ivalue, 8, &v))
		return false;

	return ofc_sema_type_cast_value__int_real(
		8, &v, okind, ovalue);
}

static void ofc_sema_type_cast_value__logical_complex(
	unsigned ikind,
	const void* ivalue,
	unsigned okind,
	void* ovalue)
{
	uint8_t v[okind];
	if(!ofc_sema_type_cast_value__logical_real(
		ikind, ivalue, okind, v))
		return false;

	return ofc_sema_type_cast_value__real_complex(
		okind, &v, okind, ovalue);
}

static bool ofc_sema_type_cast_value__real_int64(
	unsigned ikind,
	const void* ivalue,
	int64_t* ovalue)
{
	long double iv;
	switch (ikind)
	{
		case 4:
			iv = *((const float*)ivalue);
			break;
		case 8:
			iv = *((const double*)ivalue);
			break;
		case 10:
			iv = *((const long double*)ivalue);
			break;
		default:
			return false;
	}

	int64_t ov = (int64_t)iv;
	if ((long double)ov != iv)
	{
		/* TODO - Warn about lossy conversion. */
	}

	if (ovalue) *ovalue = ov;
	return true;
}

static bool ofc_sema_type_cast_value__real_int(
	unsigned ikind,
	const void* ivalue,
	unsigned okind,
	void* ovalue)
{
	if (okind > 8)
		return false;

	int64_t v;
	if (!ofc_sema_type_cast_value__real_int64(
		ikind, ivalue, &v))
		return false;

	return ofc_sema_type_cast_value__int_int(
		8, &v, okind, ovalue);
}

static void ofc_sema_type_cast_value__real_byte(
	unsigned ikind,
	const void* ivalue,
	unsigned okind,
	void* ovalue)
{
	if (okind != 1)
		return false;
	return ofc_sema_type_cast_value__real_int(
		ikind, ivalue, 1, ovalue);
}

static bool ofc_sema_type_cast_value__real_logical(
	unsigned ikind,
	const void* ivalue,
	unsigned okind,
	void* ovalue)
{
	uint8_t v[okind]
	if (!ofc_sema_type_cast_value__real_int(
		ikind, ivalue, okind, &v))
		return false;

	return ofc_sema_type_cast_value__int_logical(
		okind, v, okind, ovalue);
}

static bool ofc_sema_type_cast_value__real_real(
	unsigned ikind,
	const void* ivalue,
	unsigned okind,
	void* ovalue)
{
	long double iv;

	switch (ikind)
	{
		case 4:
			iv = *((const float*)ivalue);
			break;
		case 8:
			iv = *((const double*)ivalue);
			break;
		case 10:
			iv = *((const long double*)ivalue);
			break;
		default:
			return false;
	}

	/* TODO - Warn about lossy conversion? */

	if (okind == 4)
	{
		float ov = (float)iv;
		if (ovalue)
			memcpy(ovalue, ov, okind);
	}
	else if (okind == 8)
	{
		double ov = (double)iv;
		if (ovalue)
			memcpy(ovalue, ov, okind);
	}
	else if (okind == 10)
	{
		long double ov = (long double)iv;
		if (ovalue)
			memcpy(ovalue, ov, okind);
	}
	else
	{
		return false;
	}

	return true;
}

static bool ofc_sema_type_cast_value__real_complex(
	unsigned ikind,
	const void* ivalue,
	unsigned okind,
	void* ovalue)
{
	uint8_t v[okind * 2] = {0};
	if (!ofc_sema_type_cast_value__real_real(
		ikind, ivalue, okind, &v))
		return false;

	if (ovalue)
		memcpy(ovalue, &v, (okind * 2));
	return true;
}

static bool ofc_sema_type_cast_value__complex_int(
	unsigned ikind,
	const void* ivalue,
	unsigned okind,
	void* ovalue)
{
	return ofc_sema_type_cast_value__real_int(
		ikind, ivalue, okind, ovalue);
}

static void ofc_sema_type_cast_value__complex_byte(
	unsigned ikind,
	const void* ivalue,
	unsigned okind,
	void* ovalue)
{
	if (okind != 1)
		return false;
	return ofc_sema_type_cast_value__complex_int(
		ikind, ivalue, 1, ovalue);
}

static bool ofc_sema_type_cast_value__complex_logical(
	unsigned ikind,
	const void* ivalue,
	unsigned okind,
	void* ovalue)
{
	uint8_t v[okind];
	if (!ofc_sema_type_cast_value__complex_int(
		ikind, ivalue, okind, v))
		return false;

	return ofc_sema_type_cast_value__int_logical(
		okind, v, okind, ovalue);
}

static bool ofc_sema_type_cast_value__complex_real(
	unsigned ikind,
	const void* ivalue,
	unsigned okind,
	void* ovalue)
{
	return ofc_sema_type_cast_value__real_real(
		ikind, ivalue, okind, ovalue);
}

static bool ofc_sema_type_cast_value__complex_complex(
	unsigned ikind,
	const void* ivalue,
	unsigned okind,
	void* ovalue)
{
	uint8_t v[okind][2];

	const void* ireal = ivalue;
	const void* iimag = (const void*)((uintptr_t)ivalue + ikind);

	if (!ofc_sema_type_cast_value__real_real(
		ireal, ivalue, okind, v[0])
		|| !ofc_sema_type_cast_value__real_real(
			iimag, ivalue, okind, v[1]))
		return false;

	if (ovalue)
		memcpy(ovalue, &v[0][0], (okind * 2));
	return true;
}

static bool ofc_sema_type_cast_value__character_character(
	unsigned ikind,
	const void* ivalue,
	unsigned okind,
	void* ovalue)
{
	/* TODO - Implement character truncation and (space) padding */
	return false;
}

bool ofc_sema_type_cast_value(
	const ofc_sema_type_t* itype,
	const void* ivalue,
	const ofc_sema_type_t* otype,
	void* ovalue)
{
	if (!itype || !ivalue || !otype)
		return false;

	if ((itype->type >= OFC_SEMA_TYPE_COUNT)
		|| (otype->type >= OFC_SEMA_TYPE_COUNT))
		return false;

	/* This has to match the members of ofc_sema_type_e */
	static bool (*ft)(unsigned, void*, unsigned, void*)[OFC_SEMA_TYPE_COUNT][OFC_SEMA_TYPE_COUNT] =
	{
		/* OFC_SEMA_TYPE_LOGICAL */
		{
			ofc_sema_type_cast_value__logical_logical,
			ofc_sema_type_cast_value__logical_int,
			ofc_sema_type_cast_value__logical_real,
			ofc_sema_type_cast_value__logical_complex,
			ofc_sema_type_cast_value__logical_byte,
			NULL,
			NULL, NULL, NULL
		},

		/* OFC_SEMA_TYPE_INTEGER */
		{
			ofc_sema_type_cast_value__int_logical,
			ofc_sema_type_cast_value__int_int,
			ofc_sema_type_cast_value__int_real,
			ofc_sema_type_cast_value__int_complex,
			ofc_sema_type_cast_value__int_byte,
			NULL,
			NULL, NULL, NULL
		},

		/* OFC_SEMA_TYPE_REAL */
		{
			ofc_sema_type_cast_value__real_logical,
			ofc_sema_type_cast_value__real_int,
			ofc_sema_type_cast_value__real_real,
			ofc_sema_type_cast_value__real_complex,
			ofc_sema_type_cast_value__real_byte,
			NULL,
			NULL, NULL, NULL
		},

		/* OFC_SEMA_TYPE_COMPLEX */
		{
			ofc_sema_type_cast_value__complex_logical,
			ofc_sema_type_cast_value__complex_int,
			ofc_sema_type_cast_value__complex_real,
			ofc_sema_type_cast_value__complex_complex,
			ofc_sema_type_cast_value__complex_byte,
			NULL,
			NULL, NULL, NULL
		},

		/* OFC_SEMA_TYPE_BYTE */
		{
			ofc_sema_type_cast_value__byte_logical,
			ofc_sema_type_cast_value__byte_int,
			ofc_sema_type_cast_value__byte_real,
			ofc_sema_type_cast_value__byte_complex,
			ofc_sema_type_cast_value__byte_byte,
			NULL,
			NULL, NULL, NULL
		},

		/* OFC_SEMA_TYPE_CHARACTER */
		{
			NULL,
			NULL,
			NULL,
			NULL,
			NULL,
			sema_type_cast_value__character_character,
			NULL, NULL, NULL
		},

		/* OFC_SEMA_TYPE_STRUCTURE */
		{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL },

		/* OFC_SEMA_TYPE_POINTER */
		{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL },

		/* OFC_SEMA_TYPE_ARRAY */
		{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL },
	};

	bool (*f)(unsigned, void*, unsigned, void*)
		= ft[itype->type][otype->type];
	if (!ft) return NULL;

	return f(
		itype->kind, ivalue,
		otype->kind, ovalue);
}
