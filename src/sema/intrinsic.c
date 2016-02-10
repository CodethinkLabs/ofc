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

#include "ofc/sema.h"

static const char* ofc_sema_intrinsics__reserved_list[]=
{
	"AdjustL",
	"AdjustR",
	"All",
	"Allocated",
	"Any",
	"Associated",
	"Ceiling",
	"Count",
	"CShift",
	"Digits",
	"Dot_Product",
	"EOShift",
	"Epsilon",
	"Exponent",
	"Floor",
	"Fraction",
	"Huge",
	"Kind",
	"LBound",
	"Logical",
	"MatMul",
	"MaxExponent",
	"MaxLoc",
	"MaxVal",
	"Merge",
	"MinExponent",
	"MinLoc",
	"MinVal",
	"Modulo",
	"Nearest",
	"Pack",
	"Precision",
	"Present",
	"Product",
	"Radix",
	"Random_Number",
	"Random_Seed",
	"Range",
	"Repeat",
	"Reshape",
	"RRSpacing",
	"Scale",
	"Scan",
	"Selected_Int_Kind",
	"Selected_Real_Kind",
	"Set_Exponent",
	"Shape",
	"Spacing",
	"Spread",
	"Sum",
	"Tiny",
	"Transfer",
	"Transpose",
	"Trim",
	"UBound",
	"Unpack",
	"Verify",

	NULL
};

bool ofc_sema_intrinsic_name_reserved(char* name)
{
    unsigned i = 0;

	/* TODO - Use a hash map to speed this up. */
	while (ofc_sema_intrinsics__reserved_list[i])
	{
		if (strcasecmp(ofc_sema_intrinsics__reserved_list[i], name) == 0)
			return true;
	}

	return false;
}

typedef enum
{
	OFC_SEMA_INTRINSIC_OP,
	OFC_SEMA_INTRINSIC_FUNC,
	OFC_SEMA_INTRINSIC_SUBR,

	OFC_SEMA_INTRINSIC_INVALID
} ofc_sema_intrinsic_e;

typedef enum
{
	IT_ANY,     /* Any type */
	IT_SAME,    /* Same as argument */
	IT_SCALAR,  /* Any scalar type */
	IT_LOGICAL,
	IT_INTEGER,
	IT_REAL,
	IT_COMPLEX,
	IT_CHARACTER,

	IT_CHARACTER_1,

	IT_DEF_LOGICAL,
	IT_DEF_INTEGER,
	IT_DEF_REAL,
	IT_DEF_COMPLEX,

	IT_DEF_DOUBLE,
	IT_DEF_DOUBLE_COMPLEX,

	IT_DEF_HALF_INTEGER,

	IT_INTEGER_1,
	IT_INTEGER_2,
	IT_INTEGER_4,

	IT_COUNT
} ofc_sema_intrinsic_type_e;

typedef struct
{
	const char*               name;
	unsigned                  arg_min, arg_max;
	ofc_sema_intrinsic_type_e return_type;
	ofc_sema_intrinsic_type_e arg_type;
} ofc_sema_intrinsic_op_t;

static const ofc_sema_intrinsic_op_t ofc_sema_intrinsic__op_list[] =
{
	/* Casts */
	{ "INT"   , 1, 1, IT_DEF_INTEGER        , IT_ANY                },
	{ "IFIX"  , 1, 1, IT_DEF_INTEGER        , IT_DEF_REAL           },
	{ "IDINT" , 1, 1, IT_DEF_INTEGER        , IT_DEF_DOUBLE         },
	{ "HFIX"  , 1, 1, IT_DEF_HALF_INTEGER   , IT_ANY                },
	{ "INT1"  , 1, 1, IT_INTEGER_1          , IT_ANY                },
	{ "INT2"  , 1, 1, IT_INTEGER_2          , IT_ANY                },
	{ "INT4"  , 1, 1, IT_INTEGER_4          , IT_ANY                },
	{ "INTC"  , 1, 1, IT_INTEGER_2          , IT_ANY                },
	{ "JFIX"  , 1, 1, IT_INTEGER_4          , IT_ANY                },
	{ "REAL"  , 1, 1, IT_DEF_REAL           , IT_ANY                },
	{ "FLOAT" , 1, 1, IT_DEF_REAL           , IT_DEF_INTEGER        },
	{ "SNGL"  , 1, 1, IT_DEF_REAL           , IT_DEF_DOUBLE         },
	{ "DREAL" , 1, 1, IT_DEF_DOUBLE         , IT_DEF_DOUBLE_COMPLEX },
	{ "DBLE"  , 1, 1, IT_DEF_DOUBLE         , IT_ANY                },
	{ "DFLOAT", 1, 1, IT_DEF_DOUBLE         , IT_ANY                },
	{ "CMPLX" , 1, 2, IT_DEF_COMPLEX        , IT_ANY                },
	{ "DCMPLX", 1, 2, IT_DEF_DOUBLE_COMPLEX , IT_ANY                },
	/* TODO - CHAR, ICHAR */

	/* Truncation */
	{ "AINT", 1, 1, IT_SAME, IT_REAL       },
	{ "DINT", 1, 1, IT_SAME, IT_DEF_DOUBLE },

	/* Rounding */
	{ "ANINT" , 1, 1, IT_SAME       , IT_REAL       },
	{ "DNINT" , 1, 1, IT_SAME       , IT_DEF_DOUBLE },
	{ "NINT"  , 1, 1, IT_DEF_INTEGER, IT_REAL       },
	{ "IDNINT", 1, 1, IT_DEF_INTEGER, IT_DEF_DOUBLE },

	{ "ABS" , 1, 1, IT_SCALAR  , IT_ANY         },
	{ "IABS", 1, 1, IT_SAME    , IT_DEF_INTEGER },
	{ "DABS", 1, 1, IT_SAME    , IT_DEF_DOUBLE  },
	{ "CABS", 1, 1, IT_DEF_REAL, IT_DEF_COMPLEX },

	{ "MOD"   , 2, 2, IT_SAME, IT_SCALAR     },
	{ "AMOD"  , 2, 2, IT_SAME, IT_DEF_REAL   },
	{ "DMOD"  , 2, 2, IT_SAME, IT_DEF_DOUBLE },
	{ "MODULO", 2, 2, IT_SAME, IT_SCALAR     },

	{ "FLOOR"  , 1, 1, IT_SAME, IT_REAL },
	{ "CEILING", 1, 1, IT_SAME, IT_REAL },

	/* Transfer of sign */
	{ "SIGN" , 2, 2, IT_SAME, IT_SCALAR      },
	{ "ISIGN", 2, 2, IT_SAME, IT_DEF_INTEGER },
	{ "DSIGN", 2, 2, IT_SAME, IT_DEF_DOUBLE  },

	/* Positive difference */
	{ "DIM" , 2, 2, IT_SAME, IT_SCALAR      },
	{ "IDIM", 2, 2, IT_SAME, IT_DEF_INTEGER },
	{ "DDIM", 2, 2, IT_SAME, IT_DEF_DOUBLE  },

	/* Inner product */
	{ "DRPOD", 2, 2, IT_DEF_DOUBLE, IT_DEF_REAL },

	{ "MAX"  , 2, 0, IT_SAME       , IT_SCALAR      },
	{ "MAX0" , 2, 0, IT_SAME       , IT_DEF_INTEGER },
	{ "AMAX1", 2, 0, IT_SAME       , IT_DEF_REAL    },
	{ "DMAX1", 2, 0, IT_SAME       , IT_DEF_DOUBLE  },
	{ "AMAX0", 2, 0, IT_DEF_REAL   , IT_DEF_INTEGER },
	{ "MAX1" , 2, 0, IT_DEF_INTEGER, IT_DEF_REAL    },
	{ "MIN"  , 2, 0, IT_SAME       , IT_SCALAR      },
	{ "MIN0" , 2, 0, IT_SAME       , IT_DEF_INTEGER },
	{ "AMIN1", 2, 0, IT_SAME       , IT_DEF_REAL    },
	{ "DMIN1", 2, 0, IT_SAME       , IT_DEF_DOUBLE  },
	{ "AMIN0", 2, 0, IT_DEF_REAL   , IT_DEF_INTEGER },
	{ "MIN1" , 2, 0, IT_DEF_INTEGER, IT_DEF_REAL    },

	{ "AIMG" , 1, 1, IT_SCALAR, IT_COMPLEX },
	{ "CONJG", 1, 1, IT_SCALAR, IT_COMPLEX },

	{ "SQRT" , 1, 1, IT_SAME, IT_ANY         },
	{ "DSQRT", 1, 1, IT_SAME, IT_DEF_DOUBLE  },
	{ "CSQRT", 1, 1, IT_SAME, IT_DEF_COMPLEX },

	{ "EXP" , 1, 1, IT_SAME, IT_ANY },
	{ "DEXP", 1, 1, IT_SAME, IT_ANY },
	{ "CEXP", 1, 1, IT_SAME, IT_ANY },

	{ "LOG" , 1, 1, IT_SAME, IT_ANY         },
	{ "ALOG", 1, 1, IT_SAME, IT_DEF_REAL    },
	{ "DLOG", 1, 1, IT_SAME, IT_DEF_DOUBLE  },
	{ "CLOG", 1, 1, IT_SAME, IT_DEF_COMPLEX },

	{ "LOG10" , 1, 1, IT_SAME, IT_ANY        },
	{ "ALOG10", 1, 1, IT_SAME, IT_DEF_REAL   },
	{ "DLOG10", 1, 1, IT_SAME, IT_DEF_DOUBLE },

	{ "SIN" , 1, 1, IT_SAME, IT_ANY         },
	{ "DSIN", 1, 1, IT_SAME, IT_DEF_DOUBLE  },
	{ "CSIN", 1, 1, IT_SAME, IT_DEF_COMPLEX },

	{ "COS" , 1, 1, IT_SAME, IT_ANY         },
	{ "DCOS", 1, 1, IT_SAME, IT_DEF_DOUBLE  },
	{ "CCOS", 1, 1, IT_SAME, IT_DEF_COMPLEX },

	{ "TAN" , 1, 1, IT_SAME, IT_ANY        },
	{ "DTAN", 1, 1, IT_SAME, IT_DEF_DOUBLE },

	{ "ASIN" , 1, 1, IT_SAME, IT_ANY        },
	{ "DASIN", 1, 1, IT_SAME, IT_DEF_DOUBLE },

	{ "ACOS" , 1, 1, IT_SAME, IT_ANY        },
	{ "DACOS", 1, 1, IT_SAME, IT_DEF_DOUBLE },

	{ "ATAN"  , 1, 2, IT_SAME, IT_ANY        },
	{ "DATAN" , 1, 2, IT_SAME, IT_DEF_DOUBLE },
	{ "ATAN2" , 2, 2, IT_SAME, IT_ANY        },
	{ "DATAN2", 2, 2, IT_SAME, IT_DEF_DOUBLE },

	{ "SINH" , 1, 1, IT_SAME, IT_ANY        },
	{ "DSINH", 1, 1, IT_SAME, IT_DEF_DOUBLE },

	{ "COSH" , 1, 1, IT_SAME, IT_ANY        },
	{ "DCOSH", 1, 1, IT_SAME, IT_DEF_DOUBLE },

	{ "TANH"  , 1, 1, IT_SAME, IT_ANY        },
	{ "DTANH" , 1, 1, IT_SAME, IT_DEF_DOUBLE },

	{ "ASINH" , 1, 1, IT_SAME, IT_ANY        },
	{ "DASINH", 1, 1, IT_SAME, IT_DEF_DOUBLE },

	{ "ACOSH" , 1, 1, IT_SAME, IT_ANY        },
	{ "DACOSH", 1, 1, IT_SAME, IT_DEF_DOUBLE },

	{ "ATANH"  , 1, 1, IT_SAME, IT_ANY        },
	{ "DATANH" , 1, 1, IT_SAME, IT_DEF_DOUBLE },

	{ "IAND", 2, 2, IT_SAME, IT_INTEGER },
	{ "IEOR", 2, 2, IT_SAME, IT_INTEGER },
	{ "IOR" , 2, 2, IT_SAME, IT_INTEGER },
	{ "NOT" , 1, 1, IT_SAME, IT_INTEGER },

	{ NULL, 0, 0, 0, 0 }
};

typedef enum
{
	IN = 0,
	OUT,

	NS
} ofc_sema_intrinsic_arg_intent_e;

typedef struct
{
	ofc_sema_intrinsic_type_e       type;
	int                             length;
	ofc_sema_intrinsic_arg_intent_e intent;
} ofc_sema_intrinsic_arg_t;


typedef struct
{
	const char*               name;
	unsigned                  arg_min, arg_max;
	ofc_sema_intrinsic_type_e return_type;
	ofc_sema_intrinsic_arg_t  arg_type[3];
} ofc_sema_intrinsic_func_t;

static const ofc_sema_intrinsic_func_t ofc_sema_intrinsic__func_list[] =
{
	{ "MClock",  0, 0, IT_INTEGER_1, {{ 0 }} },
	{ "MClock8", 0, 0, IT_INTEGER_2, {{ 0 }} },
	{ "FDate",   0, 0, IT_CHARACTER, {{ 0 }} },
	{ "Second",  0, 0, IT_DEF_REAL,  {{ 0 }} },

	{ "Loc",      1, 1, IT_DEF_INTEGER, {{ IT_ANY,         0, IN  }} },
	{ "IRand",    0, 1, IT_DEF_INTEGER, {{ IT_INTEGER,     0, IN  }} },
	{ "LnBlnk",   1, 1, IT_DEF_INTEGER, {{ IT_CHARACTER,   0, IN  }} },
	{ "IsaTty",   1, 1, IT_LOGICAL,     {{ IT_INTEGER,     0, IN  }} },
	{ "Len",      1, 1, IT_INTEGER_1,   {{ IT_CHARACTER,   0, IN  }} },
	{ "AImag",    1, 1, IT_REAL,        {{ IT_DEF_COMPLEX, 0, IN  }} },
	{ "Len_Trim", 1, 1, IT_DEF_INTEGER, {{ IT_CHARACTER,   0, IN  }} },
	{ "AChar",    1, 1, IT_CHARACTER,   {{ IT_INTEGER,     0, IN  }} },
	{ "IChar",    1, 1, IT_DEF_INTEGER, {{ IT_CHARACTER_1, 0, IN  }} },
	{ "BesJ0",    1, 1, IT_REAL,        {{ IT_REAL,        0, IN  }} },
	{ "BesJ1",    1, 1, IT_REAL,        {{ IT_REAL,        0, IN  }} },
	{ "BesJN",    1, 1, IT_DEF_INTEGER, {{ IT_REAL,        0, IN  }} },
	{ "BesY0",    1, 1, IT_REAL,        {{ IT_REAL,        0, IN  }} },
	{ "BesY1",    1, 1, IT_REAL,        {{ IT_REAL,        0, IN  }} },
	{ "CTime",    1, 1, IT_CHARACTER,   {{ IT_INTEGER,     0, IN  }} },
	{ "DErF",     1, 1, IT_DEF_DOUBLE,  {{ IT_DEF_DOUBLE,  0, IN  }} },
	{ "DErFC",    1, 1, IT_DEF_DOUBLE,  {{ IT_DEF_DOUBLE,  0, IN  }} },
	{ "ErF",      1, 1, IT_REAL,        {{ IT_REAL,        0, IN  }} },
	{ "ErFC",     1, 1, IT_REAL,        {{ IT_REAL,        0, IN  }} },
	{ "ETime",    1, 1, IT_DEF_REAL,    {{ IT_DEF_REAL,    2, IN  }} },
	{ "FTell",    1, 1, IT_DEF_INTEGER, {{ IT_INTEGER,     0, IN  }} },
	{ "GetCWD",   1, 1, IT_DEF_INTEGER, {{ IT_CHARACTER,   0, OUT }} },
	{ "HostNm",   1, 1, IT_DEF_INTEGER, {{ IT_CHARACTER,   0, OUT }} },
	{ "TtyNam",   1, 1, IT_CHARACTER,   {{ IT_INTEGER,     0, IN  }} },

	{ "Stat",   2, 2, IT_DEF_INTEGER, {{ IT_CHARACTER, 0, IN }, { IT_INTEGER,   13, OUT }} },
	{ "LStat",  2, 2, IT_DEF_INTEGER, {{ IT_CHARACTER, 0, IN }, { IT_INTEGER,   13, OUT }} },
	{ "FStat",  2, 2, IT_DEF_INTEGER, {{ IT_INTEGER,   0, IN }, { IT_INTEGER,   13, OUT }} },
	{ "Access", 2, 2, IT_DEF_INTEGER, {{ IT_CHARACTER, 0, IN }, { IT_CHARACTER, 0,  IN  }} },
	{ "LGe",    2, 2, IT_LOGICAL,     {{ IT_CHARACTER, 0, IN }, { IT_CHARACTER, 0,  IN  }} },
	{ "LGt",    2, 2, IT_LOGICAL,     {{ IT_CHARACTER, 0, IN }, { IT_CHARACTER, 0,  IN  }} },
	{ "LLe",    2, 2, IT_LOGICAL,     {{ IT_CHARACTER, 0, IN }, { IT_CHARACTER, 0,  IN  }} },
	{ "LLt",    2, 2, IT_LOGICAL,     {{ IT_CHARACTER, 0, IN }, { IT_CHARACTER, 0,  IN  }} },
	{ "LShift", 2, 2, IT_DEF_INTEGER, {{ IT_INTEGER,   0, IN }, { IT_INTEGER,   0,  IN  }} },
	{ "IShft",  2, 2, IT_DEF_INTEGER, {{ IT_INTEGER,   0, IN }, { IT_INTEGER,   0,  IN  }} },
	{ "BesYN",  2, 2, IT_REAL,        {{ IT_INTEGER,   0, IN }, { IT_REAL,      0,  IN  }} },

	{ "IShftC", 3, 3, IT_INTEGER, {{ IT_INTEGER, 0, IN }, { IT_INTEGER, 0, IN }, { IT_INTEGER, 0, IN }} },

	{ NULL, 0, 0, 0, {{0}} }
};


typedef struct
{
	const char*               name;
	unsigned                  arg_min, arg_max;
	ofc_sema_intrinsic_arg_t  arg_type[3];
} ofc_sema_intrinsic_subr_t;

static const ofc_sema_intrinsic_subr_t ofc_sema_intrinsic__subr_list[] =
{
	{ "ITime",  1, 1, {{ IT_INTEGER,   3, OUT }} },
	{ "FDate",  1, 1, {{ IT_CHARACTER, 0, OUT }} },
	{ "Second", 1, 1, {{ IT_REAL,      0, OUT }} },

	{ "ChDir",  1, 2, {{ IT_CHARACTER, 0, IN  }, { IT_INTEGER,   0, OUT }} },
	{ "LTime",  2, 2, {{ IT_INTEGER,   0, IN  }, { IT_CHARACTER, 0, OUT }} },
	{ "CTime",  2, 2, {{ IT_INTEGER,   0, IN  }, { IT_CHARACTER, 0, OUT }} },
	{ "DTime",  2, 2, {{ IT_REAL,      2, OUT }, { IT_REAL,      0, OUT }} },
	{ "ETime",  2, 2, {{ IT_REAL,      2, OUT }, { IT_REAL,      0, OUT }} },
	{ "FGet",   1, 2, {{ IT_CHARACTER, 0, OUT }, { IT_INTEGER,   0, OUT }} },
	{ "FPut",   1, 2, {{ IT_CHARACTER, 0, IN  }, { IT_INTEGER,   0, OUT }} },
	{ "FTell",  2, 2, {{ IT_INTEGER,   0, IN  }, { IT_INTEGER,   0, OUT }} },
	{ "GetCWD", 1, 2, {{ IT_CHARACTER, 0, OUT }, { IT_INTEGER,   0, OUT }} },
	{ "HostNm", 1, 2, {{ IT_CHARACTER, 0, OUT }, { IT_INTEGER,   0, OUT }} },
	{ "System", 1, 2, {{ IT_CHARACTER, 0, IN  }, { IT_INTEGER,   0, OUT }} },
	{ "TtyNam", 2, 2, {{ IT_INTEGER,   0, IN  }, { IT_CHARACTER, 0, OUT }} },
	{ "UMask",  1, 2, {{ IT_INTEGER,   0, IN  }, { IT_INTEGER,   0, OUT }} },
	{ "Unlink", 1, 2, {{ IT_CHARACTER, 0, IN  }, { IT_INTEGER,   0, OUT }} },

	{ "ChMod",  2, 3, {{ IT_CHARACTER, 0, IN }, { IT_CHARACTER, 0,  IN  }, { IT_INTEGER, 0, OUT }} },
	{ "SymLnk", 2, 3, {{ IT_CHARACTER, 0, IN }, { IT_CHARACTER, 0,  IN  }, { IT_INTEGER, 0, OUT }} },
	{ "Kill",   2, 3, {{ IT_INTEGER,   0, IN }, { IT_INTEGER,   0,  IN  }, { IT_INTEGER, 0, OUT }} },
	{ "Stat",   2, 3, {{ IT_CHARACTER, 0, IN }, { IT_INTEGER,   13, OUT }, { IT_INTEGER, 0, OUT }} },
	{ "FStat",  2, 3, {{ IT_INTEGER,   0, IN }, { IT_INTEGER,   13, OUT }, { IT_INTEGER, 0, OUT }} },
	{ "LStat",  2, 3, {{ IT_CHARACTER, 0, IN }, { IT_INTEGER,   13, OUT }, { IT_INTEGER, 0, OUT }} },
	{ "Alarm",  2, 3, {{ IT_INTEGER,   0, IN }, { IT_INTEGER,   13, IN  }, { IT_INTEGER, 0, OUT }} },
	{ "FGetC",  2, 3, {{ IT_INTEGER,   0, IN }, { IT_CHARACTER, 0,  OUT }, { IT_INTEGER, 0, OUT }} },
	{ "FPutC",  2, 3, {{ IT_INTEGER,   0, IN }, { IT_CHARACTER, 0,  IN  }, { IT_INTEGER, 0, OUT }} },
	{ "Link",   2, 3, {{ IT_CHARACTER, 0, IN }, { IT_CHARACTER, 0,  IN  }, { IT_INTEGER, 0, OUT }} },
	{ "Rename", 2, 3, {{ IT_CHARACTER, 0, IN }, { IT_CHARACTER, 0,  IN  }, { IT_INTEGER, 0, OUT }} },

	{ NULL, 0, 0, {{0}} }
};


struct ofc_sema_intrinsic_s
{
	ofc_sema_intrinsic_e type;

	ofc_str_ref_t name;

	union
	{
		const ofc_sema_intrinsic_op_t*   op;
		const ofc_sema_intrinsic_func_t* func;
		const ofc_sema_intrinsic_subr_t* subr;
	};
};

static ofc_sema_intrinsic_t* ofc_sema_intrinsic__create_op(
	const ofc_sema_intrinsic_op_t* op)
{
	if (!op)
		return NULL;

	ofc_sema_intrinsic_t* intrinsic
		= (ofc_sema_intrinsic_t*)malloc(
			sizeof(ofc_sema_intrinsic_t));
	if (!intrinsic) return NULL;

	intrinsic->name = ofc_str_ref_from_strz(op->name);
	intrinsic->type = OFC_SEMA_INTRINSIC_OP;
	intrinsic->op = op;

	return intrinsic;
}

static ofc_sema_intrinsic_t* ofc_sema_intrinsic__create_func(
	const ofc_sema_intrinsic_func_t* func)
{
	if (!func)
		return NULL;

	ofc_sema_intrinsic_t* intrinsic
		= (ofc_sema_intrinsic_t*)malloc(
			sizeof(ofc_sema_intrinsic_t));
	if (!intrinsic) return NULL;

	intrinsic->name = ofc_str_ref_from_strz(func->name);
	intrinsic->type = OFC_SEMA_INTRINSIC_FUNC;
	intrinsic->func = func;

	return intrinsic;
}

static ofc_sema_intrinsic_t* ofc_sema_intrinsic__create_subr(
	const ofc_sema_intrinsic_subr_t* subr)
{
	if (!subr)
		return NULL;

	ofc_sema_intrinsic_t* intrinsic
		= (ofc_sema_intrinsic_t*)malloc(
			sizeof(ofc_sema_intrinsic_t));
	if (!intrinsic) return NULL;

	intrinsic->name = ofc_str_ref_from_strz(subr->name);
	intrinsic->type = OFC_SEMA_INTRINSIC_SUBR;
	intrinsic->subr = subr;

	return intrinsic;
}

static void ofc_sema_intrinsic__delete(
	ofc_sema_intrinsic_t* intrinsic)
{
	if (!intrinsic)
		return;

	free(intrinsic);
}

static const ofc_str_ref_t* ofc_sema_intrinsic__key(
	const ofc_sema_intrinsic_t* intrinsic)
{
	if (!intrinsic)
		return NULL;
	return &intrinsic->name;
}

static ofc_hashmap_t* ofc_sema_intrinsic__op_map   = NULL;
static ofc_hashmap_t* ofc_sema_intrinsic__func_map = NULL;
static ofc_hashmap_t* ofc_sema_intrinsic__subr_map = NULL;

static void ofc_sema_intrinsic__term(void)
{
	ofc_hashmap_delete(ofc_sema_intrinsic__op_map);
	ofc_hashmap_delete(ofc_sema_intrinsic__func_map);
	ofc_hashmap_delete(ofc_sema_intrinsic__subr_map);
}

static bool ofc_sema_intrinisc__op_map_init(void)
{
	ofc_sema_intrinsic__op_map = ofc_hashmap_create(
		(void*)ofc_str_ref_ptr_hash_ci,
		(void*)ofc_str_ref_ptr_equal_ci,
		(void*)ofc_sema_intrinsic__key,
		(void*)ofc_sema_intrinsic__delete);
	if (!ofc_sema_intrinsic__op_map)
		return false;

	unsigned i;
	for (i = 0; ofc_sema_intrinsic__op_list[i].name; i++)
	{
		ofc_sema_intrinsic_t* intrinsic
			= ofc_sema_intrinsic__create_op(
				&ofc_sema_intrinsic__op_list[i]);
		if (!intrinsic)
		{
			ofc_hashmap_delete(
				ofc_sema_intrinsic__op_map);
			ofc_sema_intrinsic__op_map = NULL;
			return false;
		}

		if (!ofc_hashmap_add(
			ofc_sema_intrinsic__op_map,
			intrinsic))
		{
			ofc_sema_intrinsic__delete(intrinsic);
			ofc_hashmap_delete(
				ofc_sema_intrinsic__op_map);
			ofc_sema_intrinsic__op_map = NULL;
			return false;
		}
	}

	return true;
}

static bool ofc_sema_intrinsic__func_map_init(void)
{
	ofc_sema_intrinsic__func_map = ofc_hashmap_create(
		(void*)ofc_str_ref_ptr_hash_ci,
		(void*)ofc_str_ref_ptr_equal_ci,
		(void*)ofc_sema_intrinsic__key,
		(void*)ofc_sema_intrinsic__delete);
	if (!ofc_sema_intrinsic__func_map)
		return false;

	unsigned i;
	for (i = 0; ofc_sema_intrinsic__func_list[i].name; i++)
	{
		ofc_sema_intrinsic_t* intrinsic
			= ofc_sema_intrinsic__create_func(
				&ofc_sema_intrinsic__func_list[i]);
		if (!intrinsic)
		{
			ofc_hashmap_delete(
				ofc_sema_intrinsic__func_map);
			ofc_sema_intrinsic__func_map = NULL;
			return false;
		}

		if (!ofc_hashmap_add(
			ofc_sema_intrinsic__func_map,
			intrinsic))
		{
			ofc_sema_intrinsic__delete(intrinsic);
			ofc_hashmap_delete(
				ofc_sema_intrinsic__func_map);
			ofc_sema_intrinsic__func_map = NULL;
			return false;
		}
	}

	return true;
}

static bool ofc_sema_intrinsic__subr_map_init(void)
{
	ofc_sema_intrinsic__subr_map = ofc_hashmap_create(
		(void*)ofc_str_ref_ptr_hash_ci,
		(void*)ofc_str_ref_ptr_equal_ci,
		(void*)ofc_sema_intrinsic__key,
		(void*)ofc_sema_intrinsic__delete);
	if (!ofc_sema_intrinsic__subr_map)
		return false;

	unsigned i;
	for (i = 0; ofc_sema_intrinsic__subr_list[i].name; i++)
	{
		ofc_sema_intrinsic_t* intrinsic
			= ofc_sema_intrinsic__create_subr(
				&ofc_sema_intrinsic__subr_list[i]);
		if (!intrinsic)
		{
			ofc_hashmap_delete(
				ofc_sema_intrinsic__subr_map);
			ofc_sema_intrinsic__subr_map = NULL;
			return false;
		}

		if (!ofc_hashmap_add(
			ofc_sema_intrinsic__subr_map,
			intrinsic))
		{
			ofc_sema_intrinsic__delete(intrinsic);
			ofc_hashmap_delete(
				ofc_sema_intrinsic__subr_map);
			ofc_sema_intrinsic__subr_map = NULL;
			return false;
		}
	}

	return true;
}

static bool ofc_sema_intrinsic__init(void)
{
	if (ofc_sema_intrinsic__op_map
		&& ofc_sema_intrinsic__func_map
		&& ofc_sema_intrinsic__subr_map)
		return true;

	/* TODO - Set case sensitivity based on lang_opts? */
	if (!ofc_sema_intrinisc__op_map_init()
		|| !ofc_sema_intrinsic__func_map_init()
		|| !ofc_sema_intrinsic__subr_map_init())
		return false;

	atexit(ofc_sema_intrinsic__term);
	return true;
}


const ofc_sema_intrinsic_t* ofc_sema_intrinsic(
	const ofc_sema_scope_t* scope,
	ofc_str_ref_t name)
{
	if (!ofc_sema_intrinsic__init())
		return NULL;

	(void)scope;

	const ofc_sema_intrinsic_t* op = ofc_hashmap_find(
		ofc_sema_intrinsic__op_map, &name);
	if (op)
		return op;

	const ofc_sema_intrinsic_t* func = ofc_hashmap_find(
		ofc_sema_intrinsic__func_map, &name);
	if (func)
		return func;

	return NULL;
}

static ofc_sema_expr_list_t* ofc_sema_intrinsic_cast__op(
	ofc_sparse_ref_t src,
	const ofc_sema_intrinsic_t* intrinsic,
	ofc_sema_expr_list_t* args)
{
	if (!intrinsic || !args
		|| (args->count == 0))
	{
		ofc_sema_expr_list_delete(args);
		return NULL;
	}

	if ((intrinsic->type != OFC_SEMA_INTRINSIC_OP)
		|| !intrinsic->op)
		return NULL;

	if (args->count < intrinsic->op->arg_min)
	{
		ofc_sparse_ref_error(src,
			"Not enough arguments for intrinsic function.");
		ofc_sema_expr_list_delete(args);
		return NULL;
	}
	if ((intrinsic->op->arg_max != 0)
		&& (args->count > intrinsic->op->arg_max))
	{
		ofc_sparse_ref_error(src,
			"Too many arguments for intrinsic function.");
		ofc_sema_expr_list_delete(args);
		return NULL;
	}

	const ofc_sema_type_t* stype = NULL;
	switch (intrinsic->op->arg_type)
	{
		case IT_DEF_LOGICAL:
			stype = ofc_sema_type_logical_default();
			break;

		case IT_DEF_INTEGER:
			stype = ofc_sema_type_integer_default();
			break;

		case IT_DEF_REAL:
			stype = ofc_sema_type_real_default();
			break;

		case IT_DEF_COMPLEX:
			stype = ofc_sema_type_complex_default();
			break;

		case IT_DEF_DOUBLE:
			stype = ofc_sema_type_double_default();
			break;

		case IT_DEF_DOUBLE_COMPLEX:
			stype = ofc_sema_type_double_complex_default();
			break;

		case IT_DEF_HALF_INTEGER:
			stype = ofc_sema_type_create_primitive(
				OFC_SEMA_TYPE_INTEGER, 5);
			break;

		case IT_INTEGER_1:
			stype = ofc_sema_type_create_primitive(
				OFC_SEMA_TYPE_INTEGER, 3);
			break;

		case IT_INTEGER_2:
			stype = ofc_sema_type_create_primitive(
				OFC_SEMA_TYPE_INTEGER, 6);
			break;

		case IT_INTEGER_4:
			stype = ofc_sema_type_create_primitive(
				OFC_SEMA_TYPE_INTEGER, 12);
			break;

		default:
			break;
	}

	const ofc_sema_type_t* ptype = stype;
	const ofc_sema_type_t* at[args->count];

	unsigned i;
	for (i = 0; i < args->count; i++)
	{
		at[i] = ofc_sema_expr_type(args->expr[i]);

		const ofc_sema_type_t* atype = at[i];
		if (!atype)
		{
			ofc_sema_expr_list_delete(args);
			return NULL;
		}

		bool valid = false;
		switch (intrinsic->op->arg_type)
		{
			case IT_ANY:
				valid = true;
				break;

			case IT_SCALAR:
				valid = ofc_sema_type_is_scalar(atype);
				break;

			case IT_LOGICAL:
				valid = ofc_sema_type_is_logical(atype);
				break;

			case IT_INTEGER:
				valid = ofc_sema_type_is_integer(atype);
				break;

			case IT_REAL:
				valid = (atype->type == OFC_SEMA_TYPE_REAL);
				break;

			case IT_COMPLEX:
				valid = (atype->type == OFC_SEMA_TYPE_COMPLEX);
				break;

			case IT_DEF_LOGICAL:
			case IT_DEF_INTEGER:
			case IT_DEF_REAL:
			case IT_DEF_COMPLEX:
			case IT_DEF_DOUBLE:
			case IT_DEF_DOUBLE_COMPLEX:
			case IT_DEF_HALF_INTEGER:
			case IT_INTEGER_1:
			case IT_INTEGER_2:
			case IT_INTEGER_4:
				valid = ofc_sema_type_compatible(atype, stype);
				break;

			default:
				break;
		}

		if (!valid)
		{
			ofc_sparse_ref_warning(args->expr[i]->src,
				"Incorrect argument type for intrinsic.");
		}

		if (!stype)
			ptype = (ptype ? ofc_sema_type_promote(ptype, atype) : atype);
	}

	for (i = 0; i < args->count; i++)
	{
		const ofc_sema_type_t* atype = at[i];

		if (!ofc_sema_type_compatible(atype, ptype))
		{
			ofc_sema_expr_t* cast
				= ofc_sema_expr_cast(
					args->expr[i], ptype);
			if (!cast)
			{
				ofc_sparse_ref_error(args->expr[i]->src,
					"Incompatible argument type for intrinsic.");
				ofc_sema_expr_list_delete(args);
				return NULL;
			}

			args->expr[i] = cast;
		}
	}

	return args;
}

static ofc_sema_expr_list_t* ofc_sema_intrinsic_cast__func(
	ofc_sparse_ref_t src,
	const ofc_sema_intrinsic_t* intrinsic,
	ofc_sema_expr_list_t* args)
{
	if (!intrinsic || !args
		|| (args->count == 0))
	{
		ofc_sema_expr_list_delete(args);
		return NULL;
	}

	if ((intrinsic->type != OFC_SEMA_INTRINSIC_FUNC)
		|| !intrinsic->func)
		return NULL;

	if (args->count < intrinsic->func->arg_min)
	{
		ofc_sparse_ref_error(src,
			"Not enough arguments for intrinsic function.");
		ofc_sema_expr_list_delete(args);
		return NULL;
	}
	if ((intrinsic->func->arg_max != 0)
		&& (args->count > intrinsic->func->arg_max))
	{
		ofc_sparse_ref_error(src,
			"Too many arguments for intrinsic function.");
		ofc_sema_expr_list_delete(args);
		return NULL;
	}


	const ofc_sema_type_t* at[args->count];

	unsigned i;
	for (i = 0; i < args->count; i++)
	{
		const ofc_sema_type_t* stype = NULL;
		switch (intrinsic->func->arg_type[i].type)
		{
			case IT_DEF_LOGICAL:
				stype = ofc_sema_type_logical_default();
				break;

			case IT_INTEGER:
			case IT_DEF_INTEGER:
				stype = ofc_sema_type_integer_default();
				break;

			case IT_DEF_REAL:
				stype = ofc_sema_type_real_default();
				break;

			case IT_DEF_COMPLEX:
				stype = ofc_sema_type_complex_default();
				break;

			case IT_DEF_DOUBLE:
				stype = ofc_sema_type_double_default();
				break;

			case IT_DEF_DOUBLE_COMPLEX:
				stype = ofc_sema_type_double_complex_default();
				break;

			case IT_CHARACTER:
				stype = ofc_sema_type_create_character(
					1, 1, true);
				break;

			case IT_DEF_HALF_INTEGER:
				stype = ofc_sema_type_create_primitive(
					OFC_SEMA_TYPE_INTEGER, 5);
				break;

			case IT_INTEGER_1:
				stype = ofc_sema_type_create_primitive(
					OFC_SEMA_TYPE_INTEGER, 3);
				break;

			case IT_INTEGER_2:
				stype = ofc_sema_type_create_primitive(
					OFC_SEMA_TYPE_INTEGER, 6);
				break;

			case IT_INTEGER_4:
				stype = ofc_sema_type_create_primitive(
					OFC_SEMA_TYPE_INTEGER, 12);
				break;

			case IT_CHARACTER_1:
				stype = ofc_sema_type_create_character(
					1, 1, false);
				break;

			default:
				break;
		}

		at[i] = ofc_sema_expr_type(args->expr[i]);

		const ofc_sema_type_t* atype = at[i];
		if (!atype)
		{
			ofc_sema_expr_list_delete(args);
			return NULL;
		}

		bool valid = false;
		switch (intrinsic->func->arg_type[i].type)
		{
			case IT_ANY:
				valid = true;
				break;

			case IT_SCALAR:
				valid = ofc_sema_type_is_scalar(atype);
				break;

			case IT_LOGICAL:
				valid = ofc_sema_type_is_logical(atype);
				break;

			case IT_INTEGER:
				valid = ofc_sema_type_is_integer(atype);
				break;

			case IT_REAL:
				valid = (atype->type == OFC_SEMA_TYPE_REAL);
				break;

			case IT_COMPLEX:
				valid = (atype->type == OFC_SEMA_TYPE_COMPLEX);
				break;

			case IT_CHARACTER:
				valid = (atype->type == OFC_SEMA_TYPE_CHARACTER);
				break;

			case IT_CHARACTER_1:
			case IT_DEF_LOGICAL:
			case IT_DEF_INTEGER:
			case IT_DEF_REAL:
			case IT_DEF_COMPLEX:
			case IT_DEF_DOUBLE:
			case IT_DEF_DOUBLE_COMPLEX:
			case IT_DEF_HALF_INTEGER:
			case IT_INTEGER_1:
			case IT_INTEGER_2:
			case IT_INTEGER_4:
				valid = ofc_sema_type_compatible(atype, stype);
				break;

			default:
				break;
		}

		if (intrinsic->func->arg_type[i].type != IT_ANY
			&& !ofc_sema_type_compatible(atype, stype))
		{
			ofc_sema_expr_t* cast
				= ofc_sema_expr_cast(
					args->expr[i], stype);
			if (!cast)
			{
				ofc_sparse_ref_error(args->expr[i]->src,
					"Incompatible argument type for intrinsic.");
				ofc_sema_expr_list_delete(args);
				return NULL;
			}
			else
			{
				valid = true;
			}

			args->expr[i] = cast;
		}

		if (!valid)
		{
			ofc_sparse_ref_warning(args->expr[i]->src,
				"Incorrect argument type for intrinsic.");
		}
	}

	return args;
}

ofc_sema_expr_list_t* ofc_sema_intrinsic_cast(
	ofc_sparse_ref_t src,
	const ofc_sema_intrinsic_t* intrinsic,
	ofc_sema_expr_list_t* args)
{
	switch (intrinsic->type)
	{
		case OFC_SEMA_INTRINSIC_OP:
			return ofc_sema_intrinsic_cast__op(src, intrinsic, args);

		case OFC_SEMA_INTRINSIC_FUNC:
			return ofc_sema_intrinsic_cast__func(src, intrinsic, args);

		/* TODO - Handle intrinsic subroutines */
		case OFC_SEMA_INTRINSIC_SUBR:
			break;

		default:
			break;
	}

	return NULL;
}


static const ofc_sema_type_t* ofc_sema_intrinsic__op_type(
	const ofc_sema_intrinsic_t* intrinsic,
	ofc_sema_expr_list_t* args)
{
	if (!intrinsic)
		return NULL;

	if ((intrinsic->type != OFC_SEMA_INTRINSIC_OP)
		|| !intrinsic->op)
		return NULL;

	switch (intrinsic->op->return_type)
	{
		case IT_ANY:
			return NULL;

		case IT_DEF_LOGICAL:
			return ofc_sema_type_logical_default();

		case IT_DEF_INTEGER:
			return ofc_sema_type_integer_default();

		case IT_DEF_REAL:
			return ofc_sema_type_real_default();

		case IT_DEF_COMPLEX:
			return ofc_sema_type_complex_default();

		case IT_DEF_DOUBLE:
			return ofc_sema_type_double_default();

		case IT_DEF_DOUBLE_COMPLEX:
			return ofc_sema_type_double_complex_default();

		case IT_DEF_HALF_INTEGER:
			return ofc_sema_type_create_primitive(
				OFC_SEMA_TYPE_INTEGER, 5);

		case IT_INTEGER_1:
			return ofc_sema_type_create_primitive(
				OFC_SEMA_TYPE_INTEGER, 3);

		case IT_INTEGER_2:
			return ofc_sema_type_create_primitive(
				OFC_SEMA_TYPE_INTEGER, 6);

		case IT_INTEGER_4:
			return ofc_sema_type_create_primitive(
				OFC_SEMA_TYPE_INTEGER, 12);

		default:
			break;
	}

	if (!args || (args->count < 1))
		return NULL;

	const ofc_sema_type_t* atype
		= ofc_sema_expr_type(args->expr[0]);
	if (!atype) return NULL;

	if (intrinsic->op->return_type == IT_SAME)
		return atype;

	if (atype->type == OFC_SEMA_TYPE_COMPLEX)
	{
		switch (intrinsic->op->return_type)
		{
			case IT_COMPLEX:
				return atype;

			case IT_SCALAR:
			case IT_REAL:
				return ofc_sema_type_create_primitive(
					OFC_SEMA_TYPE_REAL, atype->kind);

			default:
				break;
		}
	}
	else
	{
		/* TODO - Handle other conversions if used. */
		switch (intrinsic->op->return_type)
		{
			case IT_COMPLEX:
				if (atype->type == OFC_SEMA_TYPE_REAL)
				{
					return ofc_sema_type_create_primitive(
						OFC_SEMA_TYPE_COMPLEX, atype->kind);
				}
				else if (ofc_sema_type_is_scalar(atype))
				{
					const ofc_sema_type_t* ptype
						= ofc_sema_type_promote(atype,
							ofc_sema_type_real_default());
					return ofc_sema_type_create_primitive(
						OFC_SEMA_TYPE_COMPLEX, ptype->kind);
				}
				break;

			case IT_SCALAR:
				if (!ofc_sema_type_is_scalar(atype))
					return NULL;
				return atype;

			case IT_REAL:
				if (atype->type == OFC_SEMA_TYPE_REAL)
				{
					return atype;
				}
				else if (ofc_sema_type_is_scalar(atype))
				{
					return ofc_sema_type_promote(atype,
							ofc_sema_type_real_default());
				}
				break;

			default:
				break;
		}
	}

	return NULL;
}


static const ofc_sema_type_t* ofc_sema_intrinsic__func_type(
	const ofc_sema_intrinsic_t* intrinsic,
	ofc_sema_expr_list_t* args)
{
	if (!intrinsic)
		return NULL;

	if ((intrinsic->type != OFC_SEMA_INTRINSIC_FUNC)
		|| !intrinsic->op)
		return NULL;

	switch (intrinsic->func->return_type)
	{
		case IT_ANY:
			return NULL;

		case IT_DEF_LOGICAL:
			return ofc_sema_type_logical_default();

		case IT_DEF_INTEGER:
			return ofc_sema_type_integer_default();

		case IT_DEF_REAL:
			return ofc_sema_type_real_default();

		case IT_DEF_COMPLEX:
			return ofc_sema_type_complex_default();

		case IT_DEF_DOUBLE:
			return ofc_sema_type_double_default();

		case IT_DEF_DOUBLE_COMPLEX:
			return ofc_sema_type_double_complex_default();

		case IT_DEF_HALF_INTEGER:
			return ofc_sema_type_create_primitive(
				OFC_SEMA_TYPE_INTEGER, 5);

		case IT_INTEGER_1:
			return ofc_sema_type_create_primitive(
				OFC_SEMA_TYPE_INTEGER, 3);

		case IT_INTEGER_2:
			return ofc_sema_type_create_primitive(
				OFC_SEMA_TYPE_INTEGER, 6);

		case IT_INTEGER_4:
			return ofc_sema_type_create_primitive(
				OFC_SEMA_TYPE_INTEGER, 12);

		default:
			break;
	}

	if (!args || (args->count < 1))
		return NULL;

	const ofc_sema_type_t* atype
		= ofc_sema_expr_type(args->expr[0]);
	if (!atype) return NULL;

	if (intrinsic->func->return_type == IT_SAME)
		return atype;

	if (atype->type == OFC_SEMA_TYPE_COMPLEX)
	{
		switch (intrinsic->func->return_type)
		{
			case IT_COMPLEX:
				return atype;

			case IT_SCALAR:
			case IT_REAL:
				return ofc_sema_type_create_primitive(
					OFC_SEMA_TYPE_REAL, atype->kind);

			default:
				break;
		}
	}
	else
	{
		/* TODO - Handle other conversions if used. */
		switch (intrinsic->func->return_type)
		{
			case IT_COMPLEX:
				if (atype->type == OFC_SEMA_TYPE_REAL)
				{
					return ofc_sema_type_create_primitive(
						OFC_SEMA_TYPE_COMPLEX, atype->kind);
				}
				else if (ofc_sema_type_is_scalar(atype))
				{
					const ofc_sema_type_t* ptype
						= ofc_sema_type_promote(atype,
							ofc_sema_type_real_default());
					return ofc_sema_type_create_primitive(
						OFC_SEMA_TYPE_COMPLEX, ptype->kind);
				}
				break;

			case IT_SCALAR:
				if (!ofc_sema_type_is_scalar(atype))
					return NULL;
				return atype;

			case IT_REAL:
				if (atype->type == OFC_SEMA_TYPE_REAL)
				{
					return atype;
				}
				else if (ofc_sema_type_is_scalar(atype))
				{
					return ofc_sema_type_promote(atype,
							ofc_sema_type_real_default());
				}
				break;

			default:
				break;
		}
	}

	return NULL;
}

const ofc_sema_type_t* ofc_sema_intrinsic_type(
	const ofc_sema_intrinsic_t* intrinsic,
	ofc_sema_expr_list_t* args)
{
	switch (intrinsic->type)
	{
		case OFC_SEMA_INTRINSIC_OP:
			return ofc_sema_intrinsic__op_type(intrinsic, args);

		case OFC_SEMA_INTRINSIC_FUNC:
			return ofc_sema_intrinsic__func_type(intrinsic, args);

		/* TODO - Handle intrinsic subroutines */
		case OFC_SEMA_INTRINSIC_SUBR:
			break;

		default:
			break;
	}

	return NULL;
}

bool ofc_sema_intrinsic_print(
	ofc_colstr_t* cs,
	const ofc_sema_intrinsic_t* intrinsic)
{
	if (!cs || !intrinsic) return false;

	if (!ofc_str_ref_print(cs, intrinsic->name))
		return false;

	return true;
}
