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

bool ofc_sema_intrinsic_name_reserved(const char* name)
{
    unsigned i = 0;

	while (ofc_sema_intrinsics__reserved_list[i])
	{
		if (strcasecmp(ofc_sema_intrinsics__reserved_list[i], name) == 0)
			return true;
		i++;
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
	OFC_SEMA_INTRINSIC__TYPE_NORMAL = 0,

	OFC_SEMA_INTRINSIC__TYPE_ANY,

	/* Same as argument */
	OFC_SEMA_INTRINSIC__TYPE_SAME,

	OFC_SEMA_INTRINSIC__TYPE_SCALAR,

	/* Return type calculated in callback */
	OFC_SEMA_INTRINSIC__TYPE_CALLBACK,
} ofc_sema_intrinsic__type_e;

typedef struct
{
	ofc_sema_intrinsic__type_e type_type;
	ofc_sema_type_e            type;
	unsigned                   kind;
	unsigned                   size;
	bool                       intent_in;
	bool                       intent_out;
} ofc_sema_intrinsic__param_t;

static const ofc_sema_intrinsic__param_t ofc_sema_intrinsic__param[] =
{
	{ OFC_SEMA_INTRINSIC__TYPE_ANY   , 0, 0, 0, 1, 0 }, /* ANY  */
	{ OFC_SEMA_INTRINSIC__TYPE_SAME  , 0, 0, 0, 1, 0 }, /* SAME */
	{ OFC_SEMA_INTRINSIC__TYPE_SCALAR, 0, 0, 0, 1, 0 }, /* SCALAR */

	{ 0, OFC_SEMA_TYPE_LOGICAL  , 0, 0, 1, 0 }, /* LOGICAL */
	{ 0, OFC_SEMA_TYPE_INTEGER  , 0, 0, 1, 0 }, /* INTEGER */
	{ 0, OFC_SEMA_TYPE_REAL     , 0, 0, 1, 0 }, /* REAL */
	{ 0, OFC_SEMA_TYPE_COMPLEX  , 0, 0, 1, 0 }, /* COMPLEX */
	{ 0, OFC_SEMA_TYPE_CHARACTER, 0, 0, 1, 0 }, /* CHARACTER */

	{ 0, OFC_SEMA_TYPE_CHARACTER, 0, 1, 1, 0 }, /* CHARACTER_1 */

	{ 0, OFC_SEMA_TYPE_LOGICAL, 1, 0, 1, 0 }, /* DEF_LOGICAL */
	{ 0, OFC_SEMA_TYPE_INTEGER, 1, 0, 1, 0 }, /* DEF_INTEGER */
	{ 0, OFC_SEMA_TYPE_REAL   , 1, 0, 1, 0 }, /* DEF_REAL */
	{ 0, OFC_SEMA_TYPE_COMPLEX, 1, 0, 1, 0 }, /* DEF_COMPLEX */

	{ 0, OFC_SEMA_TYPE_REAL   , 2, 0, 1, 0 }, /* DEF_DOUBLE */
	{ 0, OFC_SEMA_TYPE_COMPLEX, 2, 0, 1, 0 }, /* DEF_DOUBLE_COMPLEX */

	{ 0, OFC_SEMA_TYPE_INTEGER, 5, 0, 1, 0 }, /* DEF_HALF_INTEGER */

	{ 0, OFC_SEMA_TYPE_INTEGER, 0, 0, 1, 0 }, /* INTEGER_KIND */

	{ 0, OFC_SEMA_TYPE_INTEGER,  3, 0, 1, 0 }, /* INTEGER_1 */
	{ 0, OFC_SEMA_TYPE_INTEGER,  6, 0, 1, 0 }, /* INTEGER_2 */
	{ 0, OFC_SEMA_TYPE_INTEGER, 12, 0, 1, 0 }, /* INTEGER_4 */

	{ 0, OFC_SEMA_TYPE_REAL, 1, 2, 1, 0 }, /* DEF_REAL_A2 */
	{ 0, OFC_SEMA_TYPE_REAL, 1, 2, 0, 1 }, /* DEF_REAL_A2_OUT */

	{ 0, OFC_SEMA_TYPE_CHARACTER, 0, 0, 0, 1 }, /* CHARACTER_OUT */
	{ 0, OFC_SEMA_TYPE_INTEGER  , 0, 0, 0, 1 }, /* INTEGER_OUT */
	{ 0, OFC_SEMA_TYPE_REAL     , 0, 0, 0, 1 }, /* REAL_OUT */

	{ 0, OFC_SEMA_TYPE_INTEGER, 0,  3, 0, 1 }, /* INTEGER_A3_OUT */
	{ 0, OFC_SEMA_TYPE_INTEGER, 0, 13, 1, 0 }, /* INTEGER_A13 */
	{ 0, OFC_SEMA_TYPE_INTEGER, 0, 13, 0, 1 }, /* INTEGER_A13_OUT */
};


typedef enum
{
	IP_ANY = 0, /* Any type */
	IP_SAME,    /* Same as argument */
	IP_SCALAR,  /* Any scalar type */

	IP_LOGICAL,
	IP_INTEGER,
	IP_REAL,
	IP_COMPLEX,
	IP_CHARACTER,

	IP_CHARACTER_1,

	IP_DEF_LOGICAL,
	IP_DEF_INTEGER,
	IP_DEF_REAL,
	IP_DEF_COMPLEX,

	IP_DEF_DOUBLE,
	IP_DEF_DOUBLE_COMPLEX,

	IP_DEF_HALF_INTEGER,

	IP_INTEGER_KIND,
	/* Represents an INTEGER initialization expression
	   indicating the kind parameter of the result.*/

	IP_INTEGER_1,
	IP_INTEGER_2,
	IP_INTEGER_4,

	IP_DEF_REAL_A2,
	IP_DEF_REAL_A2_OUT,

	IP_INTEGER_OUT,
	IP_REAL_OUT,
	IP_CHARACTER_OUT,

	IP_INTEGER_A3_OUT,
	IP_INTEGER_A13,
	IP_INTEGER_A13_OUT,

	IP_COUNT
} ofc_sema_intrinsic__param_e;

typedef struct
{
	const char*                 name;
	unsigned                    arg_min, arg_max;
	ofc_sema_intrinsic__param_e return_type;
	ofc_sema_intrinsic__param_e arg_type;
} ofc_sema_intrinsic_op_t;

static const ofc_sema_intrinsic_op_t ofc_sema_intrinsic__op_list[] =
{
	/* Casts */
	{ "INT"   , 1, 1, IP_DEF_INTEGER        , IP_ANY                },
	{ "IFIX"  , 1, 1, IP_DEF_INTEGER        , IP_DEF_REAL           },
	{ "IDINT" , 1, 1, IP_DEF_INTEGER        , IP_DEF_DOUBLE         },
	{ "HFIX"  , 1, 1, IP_DEF_HALF_INTEGER   , IP_ANY                },
	{ "INT1"  , 1, 1, IP_INTEGER_1          , IP_ANY                },
	{ "INT2"  , 1, 1, IP_INTEGER_2          , IP_ANY                },
	{ "INT4"  , 1, 1, IP_INTEGER_4          , IP_ANY                },
	{ "INTC"  , 1, 1, IP_INTEGER_2          , IP_ANY                },
	{ "JFIX"  , 1, 1, IP_INTEGER_4          , IP_ANY                },
	{ "REAL"  , 1, 1, IP_DEF_REAL           , IP_ANY                },
	{ "FLOAT" , 1, 1, IP_DEF_REAL           , IP_DEF_INTEGER        },
	{ "SNGL"  , 1, 1, IP_DEF_REAL           , IP_DEF_DOUBLE         },
	{ "DREAL" , 1, 1, IP_DEF_DOUBLE         , IP_DEF_DOUBLE_COMPLEX },
	{ "DBLE"  , 1, 1, IP_DEF_DOUBLE         , IP_ANY                },
	{ "DFLOAT", 1, 1, IP_DEF_DOUBLE         , IP_ANY                },
	{ "CMPLX" , 1, 2, IP_DEF_COMPLEX        , IP_ANY                },
	{ "DCMPLX", 1, 2, IP_DEF_DOUBLE_COMPLEX , IP_ANY                },
	/* TODO - CHAR, ICHAR */

	/* Truncation */
	{ "AINT", 1, 1, IP_SAME, IP_REAL       },
	{ "DINT", 1, 1, IP_SAME, IP_DEF_DOUBLE },

	/* Rounding */
	{ "ANINT" , 1, 1, IP_SAME       , IP_REAL       },
	{ "DNINT" , 1, 1, IP_SAME       , IP_DEF_DOUBLE },
	{ "NINT"  , 1, 1, IP_DEF_INTEGER, IP_REAL       },
	{ "IDNINT", 1, 1, IP_DEF_INTEGER, IP_DEF_DOUBLE },

	{ "ABS" , 1, 1, IP_SCALAR  , IP_ANY         },
	{ "IABS", 1, 1, IP_SAME    , IP_DEF_INTEGER },
	{ "DABS", 1, 1, IP_SAME    , IP_DEF_DOUBLE  },
	{ "CABS", 1, 1, IP_DEF_REAL, IP_DEF_COMPLEX },

	{ "MOD"   , 2, 2, IP_SAME, IP_SCALAR     },
	{ "AMOD"  , 2, 2, IP_SAME, IP_DEF_REAL   },
	{ "DMOD"  , 2, 2, IP_SAME, IP_DEF_DOUBLE },
	{ "MODULO", 2, 2, IP_SAME, IP_SCALAR     },

	{ "FLOOR"  , 1, 1, IP_SAME, IP_REAL },
	{ "CEILING", 1, 1, IP_SAME, IP_REAL },

	/* Transfer of sign */
	{ "SIGN" , 2, 2, IP_SAME, IP_SCALAR      },
	{ "ISIGN", 2, 2, IP_SAME, IP_DEF_INTEGER },
	{ "DSIGN", 2, 2, IP_SAME, IP_DEF_DOUBLE  },

	/* Positive difference */
	{ "DIM" , 2, 2, IP_SAME, IP_SCALAR      },
	{ "IDIM", 2, 2, IP_SAME, IP_DEF_INTEGER },
	{ "DDIM", 2, 2, IP_SAME, IP_DEF_DOUBLE  },

	/* Inner product */
	{ "DRPOD", 2, 2, IP_DEF_DOUBLE, IP_DEF_REAL },

	{ "MAX"  , 2, 0, IP_SAME       , IP_SCALAR      },
	{ "MAX0" , 2, 0, IP_SAME       , IP_DEF_INTEGER },
	{ "AMAX1", 2, 0, IP_SAME       , IP_DEF_REAL    },
	{ "DMAX1", 2, 0, IP_SAME       , IP_DEF_DOUBLE  },
	{ "AMAX0", 2, 0, IP_DEF_REAL   , IP_DEF_INTEGER },
	{ "MAX1" , 2, 0, IP_DEF_INTEGER, IP_DEF_REAL    },
	{ "MIN"  , 2, 0, IP_SAME       , IP_SCALAR      },
	{ "MIN0" , 2, 0, IP_SAME       , IP_DEF_INTEGER },
	{ "AMIN1", 2, 0, IP_SAME       , IP_DEF_REAL    },
	{ "DMIN1", 2, 0, IP_SAME       , IP_DEF_DOUBLE  },
	{ "AMIN0", 2, 0, IP_DEF_REAL   , IP_DEF_INTEGER },
	{ "MIN1" , 2, 0, IP_DEF_INTEGER, IP_DEF_REAL    },

	{ "AIMG" , 1, 1, IP_SCALAR, IP_COMPLEX },
	{ "CONJG", 1, 1, IP_SCALAR, IP_COMPLEX },

	{ "SQRT" , 1, 1, IP_SAME, IP_ANY         },
	{ "DSQRT", 1, 1, IP_SAME, IP_DEF_DOUBLE  },
	{ "CSQRT", 1, 1, IP_SAME, IP_DEF_COMPLEX },

	{ "EXP" , 1, 1, IP_SAME, IP_ANY },
	{ "DEXP", 1, 1, IP_SAME, IP_ANY },
	{ "CEXP", 1, 1, IP_SAME, IP_ANY },

	{ "LOG" , 1, 1, IP_SAME, IP_ANY         },
	{ "ALOG", 1, 1, IP_SAME, IP_DEF_REAL    },
	{ "DLOG", 1, 1, IP_SAME, IP_DEF_DOUBLE  },
	{ "CLOG", 1, 1, IP_SAME, IP_DEF_COMPLEX },

	{ "LOG10" , 1, 1, IP_SAME, IP_ANY        },
	{ "ALOG10", 1, 1, IP_SAME, IP_DEF_REAL   },
	{ "DLOG10", 1, 1, IP_SAME, IP_DEF_DOUBLE },

	{ "SIN" , 1, 1, IP_SAME, IP_ANY         },
	{ "DSIN", 1, 1, IP_SAME, IP_DEF_DOUBLE  },
	{ "CSIN", 1, 1, IP_SAME, IP_DEF_COMPLEX },

	{ "COS" , 1, 1, IP_SAME, IP_ANY         },
	{ "DCOS", 1, 1, IP_SAME, IP_DEF_DOUBLE  },
	{ "CCOS", 1, 1, IP_SAME, IP_DEF_COMPLEX },

	{ "TAN" , 1, 1, IP_SAME, IP_ANY        },
	{ "DTAN", 1, 1, IP_SAME, IP_DEF_DOUBLE },

	{ "ASIN" , 1, 1, IP_SAME, IP_ANY        },
	{ "DASIN", 1, 1, IP_SAME, IP_DEF_DOUBLE },

	{ "ACOS" , 1, 1, IP_SAME, IP_ANY        },
	{ "DACOS", 1, 1, IP_SAME, IP_DEF_DOUBLE },

	{ "ATAN"  , 1, 2, IP_SAME, IP_ANY        },
	{ "DATAN" , 1, 2, IP_SAME, IP_DEF_DOUBLE },
	{ "ATAN2" , 2, 2, IP_SAME, IP_ANY        },
	{ "DATAN2", 2, 2, IP_SAME, IP_DEF_DOUBLE },

	{ "SINH" , 1, 1, IP_SAME, IP_ANY        },
	{ "DSINH", 1, 1, IP_SAME, IP_DEF_DOUBLE },

	{ "COSH" , 1, 1, IP_SAME, IP_ANY        },
	{ "DCOSH", 1, 1, IP_SAME, IP_DEF_DOUBLE },

	{ "TANH"  , 1, 1, IP_SAME, IP_ANY        },
	{ "DTANH" , 1, 1, IP_SAME, IP_DEF_DOUBLE },

	{ "ASINH" , 1, 1, IP_SAME, IP_ANY        },
	{ "DASINH", 1, 1, IP_SAME, IP_DEF_DOUBLE },

	{ "ACOSH" , 1, 1, IP_SAME, IP_ANY        },
	{ "DACOSH", 1, 1, IP_SAME, IP_DEF_DOUBLE },

	{ "ATANH"  , 1, 1, IP_SAME, IP_ANY        },
	{ "DATANH" , 1, 1, IP_SAME, IP_DEF_DOUBLE },

	{ "IAND", 2, 2, IP_SAME, IP_INTEGER },
	{ "IEOR", 2, 2, IP_SAME, IP_INTEGER },
	{ "IOR" , 2, 2, IP_SAME, IP_INTEGER },
	{ "NOT" , 1, 1, IP_SAME, IP_INTEGER },

	{ NULL, 0, 0, 0, 0 }
};


typedef struct
{
	const char*                 name;
	unsigned                    arg_min, arg_max;
	ofc_sema_intrinsic__param_e return_type;
	ofc_sema_intrinsic__param_e arg_type[3];
} ofc_sema_intrinsic_func_t;

static const ofc_sema_intrinsic_func_t ofc_sema_intrinsic__func_list[] =
{
	{ "MClock",  0, 0, IP_INTEGER_1, { 0 } },
	{ "MClock8", 0, 0, IP_INTEGER_2, { 0 } },
	{ "FDate",   0, 0, IP_CHARACTER, { 0 } },
	{ "Second",  0, 0, IP_DEF_REAL,  { 0 } },

	{ "Loc",      1, 1, IP_DEF_INTEGER, { IP_ANY           } },
	{ "IRand",    0, 1, IP_DEF_INTEGER, { IP_INTEGER       } },
	{ "LnBlnk",   1, 1, IP_DEF_INTEGER, { IP_CHARACTER     } },
	{ "IsaTty",   1, 1, IP_LOGICAL,     { IP_INTEGER       } },
	{ "Len",      1, 1, IP_INTEGER_1,   { IP_CHARACTER     } },
	{ "AImag",    1, 1, IP_REAL,        { IP_DEF_COMPLEX   } },
	{ "Len_Trim", 1, 1, IP_DEF_INTEGER, { IP_CHARACTER     } },
	{ "AChar",    1, 1, IP_CHARACTER,   { IP_INTEGER       } },
	{ "IChar",    1, 1, IP_DEF_INTEGER, { IP_CHARACTER_1   } },
	{ "BesJ0",    1, 1, IP_REAL,        { IP_REAL          } },
	{ "BesJ1",    1, 1, IP_REAL,        { IP_REAL          } },
	{ "BesJN",    1, 1, IP_DEF_INTEGER, { IP_REAL          } },
	{ "BesY0",    1, 1, IP_REAL,        { IP_REAL          } },
	{ "BesY1",    1, 1, IP_REAL,        { IP_REAL          } },
	{ "CTime",    1, 1, IP_CHARACTER,   { IP_INTEGER       } },
	{ "DErF",     1, 1, IP_DEF_DOUBLE,  { IP_DEF_DOUBLE    } },
	{ "DErFC",    1, 1, IP_DEF_DOUBLE,  { IP_DEF_DOUBLE    } },
	{ "ErF",      1, 1, IP_REAL,        { IP_REAL          } },
	{ "ErFC",     1, 1, IP_REAL,        { IP_REAL          } },
	{ "ETime",    1, 1, IP_DEF_REAL,    { IP_DEF_REAL_A2   } },
	{ "FTell",    1, 1, IP_DEF_INTEGER, { IP_INTEGER       } },
	{ "GetCWD",   1, 1, IP_DEF_INTEGER, { IP_CHARACTER_OUT } },
	{ "HostNm",   1, 1, IP_DEF_INTEGER, { IP_CHARACTER_OUT } },
	{ "TtyNam",   1, 1, IP_CHARACTER,   { IP_INTEGER       } },

	{ "Stat",   2, 2, IP_DEF_INTEGER, { IP_CHARACTER, IP_INTEGER_A13_OUT } },
	{ "LStat",  2, 2, IP_DEF_INTEGER, { IP_CHARACTER, IP_INTEGER_A13_OUT } },
	{ "FStat",  2, 2, IP_DEF_INTEGER, { IP_INTEGER  , IP_INTEGER_A13_OUT } },
	{ "Access", 2, 2, IP_DEF_INTEGER, { IP_CHARACTER, IP_CHARACTER       } },
	{ "LGe",    2, 2, IP_LOGICAL,     { IP_CHARACTER, IP_CHARACTER       } },
	{ "LGt",    2, 2, IP_LOGICAL,     { IP_CHARACTER, IP_CHARACTER       } },
	{ "LLe",    2, 2, IP_LOGICAL,     { IP_CHARACTER, IP_CHARACTER       } },
	{ "LLt",    2, 2, IP_LOGICAL,     { IP_CHARACTER, IP_CHARACTER       } },
	{ "LShift", 2, 2, IP_DEF_INTEGER, { IP_INTEGER  , IP_INTEGER         } },
	{ "IShft",  2, 2, IP_DEF_INTEGER, { IP_INTEGER  , IP_INTEGER         } },
	{ "BesYN",  2, 2, IP_REAL,        { IP_INTEGER  , IP_REAL            } },
	{ "Char",   1, 2, IP_CHARACTER,   { IP_INTEGER  , IP_INTEGER_KIND    } },
	/* TODO - Return char must have the same kind as optional integer argument */

	{ "IShftC", 3, 3, IP_INTEGER, { IP_INTEGER, IP_INTEGER, IP_INTEGER } },

	{ NULL, 0, 0, 0, { 0 } }
};


typedef struct
{
	const char*                  name;
	unsigned                     arg_min, arg_max;
	ofc_sema_intrinsic__param_e  arg_type[3];
} ofc_sema_intrinsic_subr_t;

static const ofc_sema_intrinsic_subr_t ofc_sema_intrinsic__subr_list[] =
{
	{ "ITime",  1, 1, { IP_INTEGER_A3_OUT } },
	{ "FDate",  1, 1, { IP_CHARACTER_OUT  } },
	{ "Second", 1, 1, { IP_REAL_OUT       } },

	{ "ChDir",  1, 2, { IP_CHARACTER      , IP_INTEGER_OUT   } },
	{ "LTime",  2, 2, { IP_INTEGER        , IP_CHARACTER_OUT } },
	{ "CTime",  2, 2, { IP_INTEGER        , IP_CHARACTER_OUT } },
	{ "DTime",  2, 2, { IP_DEF_REAL_A2_OUT, IP_REAL_OUT      } },
	{ "ETime",  2, 2, { IP_DEF_REAL_A2_OUT, IP_REAL_OUT      } },
	{ "FGet",   1, 2, { IP_CHARACTER_OUT  , IP_INTEGER_OUT   } },
	{ "FPut",   1, 2, { IP_CHARACTER      , IP_INTEGER_OUT   } },
	{ "FTell",  2, 2, { IP_INTEGER        , IP_INTEGER_OUT   } },
	{ "GetCWD", 1, 2, { IP_CHARACTER_OUT  , IP_INTEGER_OUT   } },
	{ "HostNm", 1, 2, { IP_CHARACTER_OUT  , IP_INTEGER_OUT   } },
	{ "System", 1, 2, { IP_CHARACTER      , IP_INTEGER_OUT   } },
	{ "TtyNam", 2, 2, { IP_INTEGER        , IP_CHARACTER_OUT } },
	{ "UMask",  1, 2, { IP_INTEGER        , IP_INTEGER_OUT   } },
	{ "Unlink", 1, 2, { IP_CHARACTER      , IP_INTEGER_OUT   } },

	{ "ChMod",  2, 3, { IP_CHARACTER, IP_CHARACTER      , IP_INTEGER_OUT } },
	{ "SymLnk", 2, 3, { IP_CHARACTER, IP_CHARACTER      , IP_INTEGER_OUT } },
	{ "Kill",   2, 3, { IP_INTEGER  , IP_INTEGER        , IP_INTEGER_OUT } },
	{ "Stat",   2, 3, { IP_CHARACTER, IP_INTEGER_A13_OUT, IP_INTEGER_OUT } },
	{ "FStat",  2, 3, { IP_INTEGER  , IP_INTEGER_A13_OUT, IP_INTEGER_OUT } },
	{ "LStat",  2, 3, { IP_CHARACTER, IP_INTEGER_A13_OUT, IP_INTEGER_OUT } },
	{ "Alarm",  2, 3, { IP_INTEGER  , IP_INTEGER_A13    , IP_INTEGER_OUT } },
	{ "FGetC",  2, 3, { IP_INTEGER  , IP_CHARACTER_OUT  , IP_INTEGER_OUT } },
	{ "FPutC",  2, 3, { IP_INTEGER  , IP_CHARACTER      , IP_INTEGER_OUT } },
	{ "Link",   2, 3, { IP_CHARACTER, IP_CHARACTER      , IP_INTEGER_OUT } },
	{ "Rename", 2, 3, { IP_CHARACTER, IP_CHARACTER      , IP_INTEGER_OUT } },

	{ NULL, 0, 0, { 0 } }
};


static const ofc_sema_type_t* ofc_sema_intrinsic__param_rtype(
	ofc_sema_intrinsic__param_e param,
	const ofc_sema_expr_list_t* args)
{
	if (param >= IP_COUNT)
		return NULL;

	const ofc_sema_intrinsic__param_t p
		= ofc_sema_intrinsic__param[param];

	const ofc_sema_type_t* stype = NULL;
	if (args && (args->count > 0))
		stype = ofc_sema_expr_type(args->expr[0]);

	const ofc_sema_type_t* rtype = NULL;
	switch (p.type_type)
	{
		case OFC_SEMA_INTRINSIC__TYPE_NORMAL:
			break;

		case OFC_SEMA_INTRINSIC__TYPE_ANY:
			/* ANY is not a valid as a return type. */
			return NULL;

		case OFC_SEMA_INTRINSIC__TYPE_SAME:
			rtype = stype;
			if (!rtype) return NULL;
			break;

		case OFC_SEMA_INTRINSIC__TYPE_SCALAR:
			rtype = ofc_sema_type_scalar(stype);
			if (!rtype) return NULL;
			break;

		case OFC_SEMA_INTRINSIC__TYPE_CALLBACK:
			/* TODO - Special magic. */
			return NULL;

		default:
			return NULL;
	}

	/* Return value can never be an array. */
	if ((p.type != OFC_SEMA_TYPE_CHARACTER)
		&& (p.size != 0))
		return NULL;

	if (!rtype && stype
		&& (p.kind == 0))
	{
		if (p.type == stype->type)
		{
			switch (p.type)
			{
				case OFC_SEMA_TYPE_LOGICAL:
				case OFC_SEMA_TYPE_BYTE:
				case OFC_SEMA_TYPE_INTEGER:
				case OFC_SEMA_TYPE_REAL:
				case OFC_SEMA_TYPE_COMPLEX:
					rtype = stype;
					break;
				default:
					break;
			}
		}
		else if (((p.type == OFC_SEMA_TYPE_REAL)
				|| (p.type == OFC_SEMA_TYPE_COMPLEX))
			&& ((stype->type == OFC_SEMA_TYPE_REAL)
				|| (stype->type == OFC_SEMA_TYPE_COMPLEX)))
		{
			rtype = ofc_sema_type_create_primitive(
				p.type, stype->kind);
			if (!rtype) return NULL;
		}
	}

	if (!rtype)
	{
		if (p.type == OFC_SEMA_TYPE_CHARACTER)
		{
			unsigned kind = p.kind;
			if ((kind == 0)
				&& ofc_sema_type_is_character(stype))
				kind = stype->kind;
			if (kind == 0) kind = 1;

			rtype = ofc_sema_type_create_character(
				kind, p.size, (p.size == 0));
		}
		else
		{
			switch (p.type)
			{
				case OFC_SEMA_TYPE_LOGICAL:
				case OFC_SEMA_TYPE_BYTE:
				case OFC_SEMA_TYPE_INTEGER:
				case OFC_SEMA_TYPE_REAL:
				case OFC_SEMA_TYPE_COMPLEX:
					break;
				default:
					return false;
			}

			unsigned kind = p.kind;
			if (kind == 0) kind = 1;

			rtype = ofc_sema_type_create_primitive(
				p.type, kind);

			if (p.kind == 0)
			{
				rtype = ofc_sema_type_promote(
					rtype, stype);
			}
		}

		if (!rtype) return NULL;
	}

	return rtype;
}


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
		case IP_DEF_LOGICAL:
			stype = ofc_sema_type_logical_default();
			break;

		case IP_DEF_INTEGER:
			stype = ofc_sema_type_integer_default();
			break;

		case IP_DEF_REAL:
			stype = ofc_sema_type_real_default();
			break;

		case IP_DEF_COMPLEX:
			stype = ofc_sema_type_complex_default();
			break;

		case IP_DEF_DOUBLE:
			stype = ofc_sema_type_double_default();
			break;

		case IP_DEF_DOUBLE_COMPLEX:
			stype = ofc_sema_type_double_complex_default();
			break;

		case IP_DEF_HALF_INTEGER:
			stype = ofc_sema_type_create_primitive(
				OFC_SEMA_TYPE_INTEGER, 5);
			break;

		case IP_INTEGER_1:
			stype = ofc_sema_type_create_primitive(
				OFC_SEMA_TYPE_INTEGER, 3);
			break;

		case IP_INTEGER_2:
			stype = ofc_sema_type_create_primitive(
				OFC_SEMA_TYPE_INTEGER, 6);
			break;

		case IP_INTEGER_4:
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
			case IP_ANY:
				valid = true;
				break;

			case IP_SCALAR:
				valid = ofc_sema_type_is_scalar(atype);
				break;

			case IP_LOGICAL:
				valid = ofc_sema_type_is_logical(atype);
				break;

			case IP_INTEGER:
				valid = ofc_sema_type_is_integer(atype);
				break;

			case IP_REAL:
				valid = (atype->type == OFC_SEMA_TYPE_REAL);
				break;

			case IP_COMPLEX:
				valid = (atype->type == OFC_SEMA_TYPE_COMPLEX);
				break;

			case IP_DEF_LOGICAL:
			case IP_DEF_INTEGER:
			case IP_DEF_REAL:
			case IP_DEF_COMPLEX:
			case IP_DEF_DOUBLE:
			case IP_DEF_DOUBLE_COMPLEX:
			case IP_DEF_HALF_INTEGER:
			case IP_INTEGER_1:
			case IP_INTEGER_2:
			case IP_INTEGER_4:
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
		switch (intrinsic->func->arg_type[i])
		{
			case IP_DEF_LOGICAL:
				stype = ofc_sema_type_logical_default();
				break;

			case IP_INTEGER:
			case IP_DEF_INTEGER:
				stype = ofc_sema_type_integer_default();
				break;

			case IP_DEF_REAL:
				stype = ofc_sema_type_real_default();
				break;

			case IP_DEF_COMPLEX:
				stype = ofc_sema_type_complex_default();
				break;

			case IP_DEF_DOUBLE:
				stype = ofc_sema_type_double_default();
				break;

			case IP_DEF_DOUBLE_COMPLEX:
				stype = ofc_sema_type_double_complex_default();
				break;

			case IP_CHARACTER:
				stype = ofc_sema_type_create_character(
					1, 1, true);
				break;

			case IP_DEF_HALF_INTEGER:
				stype = ofc_sema_type_create_primitive(
					OFC_SEMA_TYPE_INTEGER, 5);
				break;

			case IP_INTEGER_1:
				stype = ofc_sema_type_create_primitive(
					OFC_SEMA_TYPE_INTEGER, 3);
				break;

			case IP_INTEGER_2:
				stype = ofc_sema_type_create_primitive(
					OFC_SEMA_TYPE_INTEGER, 6);
				break;

			case IP_INTEGER_4:
				stype = ofc_sema_type_create_primitive(
					OFC_SEMA_TYPE_INTEGER, 12);
				break;

			case IP_CHARACTER_1:
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
		switch (intrinsic->func->arg_type[i])
		{
			case IP_ANY:
				valid = true;
				break;

			case IP_SCALAR:
				valid = ofc_sema_type_is_scalar(atype);
				break;

			case IP_LOGICAL:
				valid = ofc_sema_type_is_logical(atype);
				break;

			case IP_INTEGER:
				valid = ofc_sema_type_is_integer(atype);
				break;

			case IP_REAL:
				valid = (atype->type == OFC_SEMA_TYPE_REAL);
				break;

			case IP_COMPLEX:
				valid = (atype->type == OFC_SEMA_TYPE_COMPLEX);
				break;

			case IP_CHARACTER:
				valid = (atype->type == OFC_SEMA_TYPE_CHARACTER);
				break;

			case IP_CHARACTER_1:
			case IP_DEF_LOGICAL:
			case IP_DEF_INTEGER:
			case IP_DEF_REAL:
			case IP_DEF_COMPLEX:
			case IP_DEF_DOUBLE:
			case IP_DEF_DOUBLE_COMPLEX:
			case IP_DEF_HALF_INTEGER:
			case IP_INTEGER_1:
			case IP_INTEGER_2:
			case IP_INTEGER_4:
				valid = ofc_sema_type_compatible(atype, stype);
				break;

			default:
				break;
		}

		if (intrinsic->func->arg_type[i] != IP_ANY
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


const ofc_sema_type_t* ofc_sema_intrinsic_type(
	const ofc_sema_intrinsic_t* intrinsic,
	ofc_sema_expr_list_t* args)
{
	switch (intrinsic->type)
	{
		case OFC_SEMA_INTRINSIC_OP:
			if (!intrinsic->op) return NULL;
			return ofc_sema_intrinsic__param_rtype(
				intrinsic->func->return_type, args);

		case OFC_SEMA_INTRINSIC_FUNC:
			if (!intrinsic->func) return NULL;
			return ofc_sema_intrinsic__param_rtype(
				intrinsic->func->return_type, args);

		/* Intrinsic subroutines have no return type*/
		case OFC_SEMA_INTRINSIC_SUBR:
			return NULL;

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
