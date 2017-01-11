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
	OFC_SEMA_INTRINSIC__TYPE_SCALAR,

	/* Same as argument(s) */
	OFC_SEMA_INTRINSIC__TYPE_SAME,

	/* Return type calculated in callback */
	OFC_SEMA_INTRINSIC__TYPE_CALLBACK,
} ofc_sema_intrinsic__type_e;

typedef struct
{
	ofc_sema_intrinsic__type_e type_type;
	ofc_sema_type_e            type;
	ofc_sema_kind_e            kind;
	unsigned                   size;
	bool                       intent_in;
	bool                       intent_out;
} ofc_sema_intrinsic__param_t;

static const ofc_sema_intrinsic__param_t ofc_sema_intrinsic__param[] =
{
	{ OFC_SEMA_INTRINSIC__TYPE_ANY     , 0, OFC_SEMA_KIND_NONE, 0, 1, 0 }, /* ANY  */
	{ OFC_SEMA_INTRINSIC__TYPE_SAME    , 0, OFC_SEMA_KIND_NONE, 0, 1, 0 }, /* SAME */
	{ OFC_SEMA_INTRINSIC__TYPE_SCALAR  , 0, OFC_SEMA_KIND_NONE, 0, 1, 0 }, /* SCALAR */
	{ OFC_SEMA_INTRINSIC__TYPE_CALLBACK, 0, OFC_SEMA_KIND_NONE, 0, 1, 0 }, /* CALLBACK */

	{ 0, OFC_SEMA_TYPE_LOGICAL  , OFC_SEMA_KIND_NONE, 0, 1, 0 }, /* LOGICAL */
	{ 0, OFC_SEMA_TYPE_INTEGER  , OFC_SEMA_KIND_NONE, 0, 1, 0 }, /* INTEGER */
	{ 0, OFC_SEMA_TYPE_REAL     , OFC_SEMA_KIND_NONE, 0, 1, 0 }, /* REAL */
	{ 0, OFC_SEMA_TYPE_COMPLEX  , OFC_SEMA_KIND_NONE, 0, 1, 0 }, /* COMPLEX */
	{ 0, OFC_SEMA_TYPE_CHARACTER, OFC_SEMA_KIND_NONE, 0, 1, 0 }, /* CHARACTER */

	{ 0, OFC_SEMA_TYPE_CHARACTER, OFC_SEMA_KIND_NONE, 1, 1, 0 }, /* CHARACTER_1 */

	{ 0, OFC_SEMA_TYPE_LOGICAL, OFC_SEMA_KIND_DEFAULT, 0, 1, 0 }, /* DEF_LOGICAL */
	{ 0, OFC_SEMA_TYPE_INTEGER, OFC_SEMA_KIND_DEFAULT, 0, 1, 0 }, /* DEF_INTEGER */
	{ 0, OFC_SEMA_TYPE_REAL   , OFC_SEMA_KIND_DEFAULT, 0, 1, 0 }, /* DEF_REAL */
	{ 0, OFC_SEMA_TYPE_COMPLEX, OFC_SEMA_KIND_DEFAULT, 0, 1, 0 }, /* DEF_COMPLEX */

	{ 0, OFC_SEMA_TYPE_REAL   , OFC_SEMA_KIND_DOUBLE, 0, 1, 0 }, /* DEF_DOUBLE */
	{ 0, OFC_SEMA_TYPE_COMPLEX, OFC_SEMA_KIND_DOUBLE, 0, 1, 0 }, /* DEF_DOUBLE_COMPLEX */

	{ 0, OFC_SEMA_TYPE_INTEGER, OFC_SEMA_KIND_HALF, 0, 1, 0 }, /* DEF_HALF_INTEGER */

	{ 0, OFC_SEMA_TYPE_INTEGER, OFC_SEMA_KIND_1_BYTE, 0, 1, 0 }, /* INTEGER_1 */
	{ 0, OFC_SEMA_TYPE_INTEGER, OFC_SEMA_KIND_2_BYTE, 0, 1, 0 }, /* INTEGER_2 */
	{ 0, OFC_SEMA_TYPE_INTEGER, OFC_SEMA_KIND_4_BYTE, 0, 1, 0 }, /* INTEGER_4 */

	{ 0, OFC_SEMA_TYPE_REAL   , OFC_SEMA_KIND_16_BYTE, 0, 1, 0 }, /* REAL_16 */
	{ 0, OFC_SEMA_TYPE_COMPLEX, OFC_SEMA_KIND_16_BYTE, 0, 1, 0 }, /* COMPLEX_32 */

	{ 0, OFC_SEMA_TYPE_REAL, OFC_SEMA_KIND_DEFAULT, 2, 1, 0 }, /* DEF_REAL_A2 */
	{ 0, OFC_SEMA_TYPE_REAL, OFC_SEMA_KIND_DEFAULT, 2, 0, 1 }, /* DEF_REAL_A2_OUT */

	{ 0, OFC_SEMA_TYPE_CHARACTER, OFC_SEMA_KIND_NONE, 0, 0, 1 }, /* CHARACTER_OUT */
	{ 0, OFC_SEMA_TYPE_INTEGER  , OFC_SEMA_KIND_NONE, 0, 0, 1 }, /* INTEGER_OUT */
	{ 0, OFC_SEMA_TYPE_REAL     , OFC_SEMA_KIND_NONE, 0, 0, 1 }, /* REAL_OUT */

	{ 0, OFC_SEMA_TYPE_INTEGER, OFC_SEMA_KIND_NONE,  3, 0, 1 }, /* INTEGER_A3_OUT */
	{ 0, OFC_SEMA_TYPE_INTEGER, OFC_SEMA_KIND_NONE, 13, 1, 0 }, /* INTEGER_A13 */
	{ 0, OFC_SEMA_TYPE_INTEGER, OFC_SEMA_KIND_NONE, 13, 0, 1 }, /* INTEGER_A13_OUT */
};


typedef enum
{
	IP_ANY = 0,  /* Any type */
	IP_SAME,     /* Same as argument */
	IP_SCALAR,   /* Any scalar type */
	IP_CALLBACK, /* Use a callback to determine return type. */

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

	IP_INTEGER_1,
	IP_INTEGER_2,
	IP_INTEGER_4,

	IP_REAL_16,
	IP_COMPLEX_32,

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


static ofc_sema_typeval_t* ofc_sema_intrinsic_op__constant_cast(
	const ofc_sema_intrinsic_t* intrinsic,
	const ofc_sema_dummy_arg_list_t* args)
{
	if (!intrinsic || !args
		|| (args->count != 1))
		return NULL;

	ofc_sema_expr_t* expr
		= ofc_sema_dummy_arg_get_expr(
			args->dummy_arg[0]);
	if (!expr) return NULL;

	if (!ofc_sema_expr_is_constant(expr))
		return NULL;

	return ofc_sema_typeval_cast(
		ofc_sema_expr_constant(expr),
		ofc_sema_intrinsic_type(intrinsic, args));
}

static ofc_sema_typeval_t* ofc_sema_intrinsic_op__constant_iand(
	const ofc_sema_intrinsic_t* intrinsic,
	const ofc_sema_dummy_arg_list_t* args)
{
	if (!intrinsic || !args
		|| (args->count != 2))
		return NULL;

	const ofc_sema_typeval_t* ctv[2];
	ofc_sema_expr_t* expr[2];

	expr[0] = ofc_sema_dummy_arg_get_expr(args->dummy_arg[0]);
	expr[1] = ofc_sema_dummy_arg_get_expr(args->dummy_arg[1]);

	ctv[0] = ofc_sema_expr_constant(expr[0]);
	ctv[1] = ofc_sema_expr_constant(expr[1]);
	if (!ctv[0] || !ctv[1]
		|| !ctv[0]->type || !ctv[1]->type
		|| !ofc_sema_type_is_integer(ctv[0]->type)
		|| !ofc_sema_type_is_integer(ctv[1]->type))
		return NULL;

	const ofc_sema_type_t* type
		= ofc_sema_type_promote(ctv[0]->type, ctv[1]->type);
	if (!type) return NULL;

	ofc_sparse_ref_t ref = OFC_SPARSE_REF_EMPTY;
	ofc_sparse_ref_bridge(
		expr[0]->src,
		expr[1]->src, &ref);

	ofc_sema_typeval_t* tv
		= ofc_sema_typeval_create_integer(0, type->kind, ref);
	if (!tv) return NULL;
	tv->integer = ctv[0]->integer & ctv[1]->integer;
	return tv;
}

static ofc_sema_typeval_t* ofc_sema_intrinsic_op__constant_ieor(
	const ofc_sema_intrinsic_t* intrinsic,
	const ofc_sema_dummy_arg_list_t* args)
{
	if (!intrinsic || !args
		|| (args->count != 2))
		return NULL;

	const ofc_sema_typeval_t* ctv[2];
	ofc_sema_expr_t* expr[2];

	expr[0] = ofc_sema_dummy_arg_get_expr(args->dummy_arg[0]);
	expr[1] = ofc_sema_dummy_arg_get_expr(args->dummy_arg[1]);

	ctv[0] = ofc_sema_expr_constant(expr[0]);
	ctv[1] = ofc_sema_expr_constant(expr[1]);
	if (!ctv[0] || !ctv[1]
		|| !ctv[0]->type || !ctv[1]->type
		|| !ofc_sema_type_is_integer(ctv[0]->type)
		|| !ofc_sema_type_is_integer(ctv[1]->type))
		return NULL;

	const ofc_sema_type_t* type
		= ofc_sema_type_promote(ctv[0]->type, ctv[1]->type);
	if (!type) return NULL;

	ofc_sparse_ref_t ref = OFC_SPARSE_REF_EMPTY;
	ofc_sparse_ref_bridge(
		expr[0]->src,
		expr[1]->src, &ref);

	ofc_sema_typeval_t* tv
		= ofc_sema_typeval_create_integer(0, type->kind, ref);
	if (!tv) return NULL;
	tv->integer = ctv[0]->integer ^ ctv[1]->integer;
	return tv;
}

static ofc_sema_typeval_t* ofc_sema_intrinsic_op__constant_ior(
	const ofc_sema_intrinsic_t* intrinsic,
	const ofc_sema_dummy_arg_list_t* args)
{
	if (!intrinsic || !args
		|| (args->count != 2))
		return NULL;

	const ofc_sema_typeval_t* ctv[2];
	ofc_sema_expr_t* expr[2];

	expr[0] = ofc_sema_dummy_arg_get_expr(args->dummy_arg[0]);
	expr[1] = ofc_sema_dummy_arg_get_expr(args->dummy_arg[1]);

	ctv[0] = ofc_sema_expr_constant(expr[0]);
	ctv[1] = ofc_sema_expr_constant(expr[1]);
	if (!ctv[0] || !ctv[1]
		|| !ctv[0]->type || !ctv[1]->type
		|| !ofc_sema_type_is_integer(ctv[0]->type)
		|| !ofc_sema_type_is_integer(ctv[1]->type))
		return NULL;

	const ofc_sema_type_t* type
		= ofc_sema_type_promote(ctv[0]->type, ctv[1]->type);
	if (!type) return NULL;

	ofc_sparse_ref_t ref = OFC_SPARSE_REF_EMPTY;
	ofc_sparse_ref_bridge(
		expr[0]->src,
		expr[1]->src, &ref);

	ofc_sema_typeval_t* tv
		= ofc_sema_typeval_create_integer(0, type->kind, ref);
	if (!tv) return NULL;
	tv->integer = ctv[0]->integer | ctv[1]->integer;
	return tv;
}

static ofc_sema_typeval_t* ofc_sema_intrinsic_op__constant_not(
	const ofc_sema_intrinsic_t* intrinsic,
	const ofc_sema_dummy_arg_list_t* args)
{
	if (!intrinsic || !args
		|| (args->count != 1))
		return NULL;

	ofc_sema_expr_t* expr
		= ofc_sema_dummy_arg_get_expr(
			args->dummy_arg[0]);
	const ofc_sema_typeval_t* ctv
		= ofc_sema_expr_constant(expr);
	if (!ctv || !ctv->type
		|| !ofc_sema_type_is_integer(ctv->type))
		return NULL;

	ofc_sema_typeval_t* tv
		= ofc_sema_typeval_copy(ctv);
	if (!tv) return NULL;
	tv->integer = ~ctv->integer;
	return tv;
}

typedef struct
{
	const char*                 name;
	unsigned                    arg_min, arg_max;
	ofc_sema_intrinsic__param_e return_type;
	ofc_sema_intrinsic__param_e arg_type;

	ofc_sema_typeval_t* (*constant)(
		const ofc_sema_intrinsic_t*,
		const ofc_sema_dummy_arg_list_t*);
} ofc_sema_intrinsic_op_t;

static const ofc_sema_intrinsic_op_t ofc_sema_intrinsic__op_list[] =
{
	/* Casts */
	{ "INT"   , 1, 1, IP_DEF_INTEGER        , IP_ANY               , ofc_sema_intrinsic_op__constant_cast },
	{ "IFIX"  , 1, 1, IP_DEF_INTEGER        , IP_DEF_REAL          , ofc_sema_intrinsic_op__constant_cast },
	{ "IDINT" , 1, 1, IP_DEF_INTEGER        , IP_DEF_DOUBLE        , ofc_sema_intrinsic_op__constant_cast },
	{ "IQINT" , 1, 1, IP_DEF_INTEGER        , IP_REAL_16           , ofc_sema_intrinsic_op__constant_cast },
	{ "HFIX"  , 1, 1, IP_DEF_HALF_INTEGER   , IP_ANY               , ofc_sema_intrinsic_op__constant_cast },
	{ "INT1"  , 1, 1, IP_INTEGER_1          , IP_ANY               , ofc_sema_intrinsic_op__constant_cast },
	{ "INT2"  , 1, 1, IP_INTEGER_2          , IP_ANY               , ofc_sema_intrinsic_op__constant_cast },
	{ "INT4"  , 1, 1, IP_INTEGER_4          , IP_ANY               , ofc_sema_intrinsic_op__constant_cast },
	{ "INTC"  , 1, 1, IP_INTEGER_2          , IP_ANY               , ofc_sema_intrinsic_op__constant_cast },
	{ "JFIX"  , 1, 1, IP_INTEGER_4          , IP_ANY               , ofc_sema_intrinsic_op__constant_cast },
	{ "REAL"  , 1, 1, IP_DEF_REAL           , IP_ANY               , ofc_sema_intrinsic_op__constant_cast },
	{ "FLOAT" , 1, 1, IP_DEF_REAL           , IP_DEF_INTEGER       , ofc_sema_intrinsic_op__constant_cast },
	{ "SNGL"  , 1, 1, IP_DEF_REAL           , IP_DEF_DOUBLE        , ofc_sema_intrinsic_op__constant_cast },
	{ "SNGLQ" , 1, 1, IP_DEF_REAL           , IP_REAL_16           , ofc_sema_intrinsic_op__constant_cast },
	{ "DBLE"  , 1, 1, IP_DEF_DOUBLE         , IP_ANY               , ofc_sema_intrinsic_op__constant_cast },
	{ "DFLOAT", 1, 1, IP_DEF_DOUBLE         , IP_DEF_INTEGER       , ofc_sema_intrinsic_op__constant_cast },
	{ "DREAL" , 1, 1, IP_DEF_DOUBLE         , IP_DEF_DOUBLE_COMPLEX, ofc_sema_intrinsic_op__constant_cast },
	{ "DBLEQ" , 1, 1, IP_DEF_DOUBLE         , IP_REAL_16           , ofc_sema_intrinsic_op__constant_cast },
	{ "QREAL" , 1, 1, IP_REAL_16            , IP_ANY               , ofc_sema_intrinsic_op__constant_cast },
	{ "QFLOAT", 1, 1, IP_REAL_16            , IP_DEF_INTEGER       , ofc_sema_intrinsic_op__constant_cast },
	/* http://kiwi.atmos.colostate.edu/rr/tidbits/xlf/pgs/lr298.htm */
	{ "QEXT"  , 1, 1, IP_REAL_16            , IP_ANY               , ofc_sema_intrinsic_op__constant_cast },
	{ "QEXTD" , 1, 1, IP_REAL_16            , IP_DEF_DOUBLE        , ofc_sema_intrinsic_op__constant_cast },
	{ "CMPLX" , 1, 2, IP_DEF_COMPLEX        , IP_ANY               , ofc_sema_intrinsic_op__constant_cast },
	{ "DCMPLX", 1, 2, IP_DEF_DOUBLE_COMPLEX , IP_ANY               , ofc_sema_intrinsic_op__constant_cast },
	{ "QCMPLX", 1, 2, IP_COMPLEX_32         , IP_ANY               , ofc_sema_intrinsic_op__constant_cast },

	/* Truncation */
	{ "AINT", 1, 1, IP_SAME, IP_REAL      , NULL },
	{ "DINT", 1, 1, IP_SAME, IP_DEF_DOUBLE, NULL },
	{ "QINT", 1, 1, IP_SAME, IP_REAL_16   , NULL },

	/* Rounding */
	{ "ANINT" , 1, 1, IP_SAME       , IP_REAL      , NULL },
	{ "DNINT" , 1, 1, IP_SAME       , IP_DEF_DOUBLE, NULL },
	{ "QNINT" , 1, 1, IP_SAME       , IP_REAL_16   , NULL },
	{ "NINT"  , 1, 1, IP_DEF_INTEGER, IP_REAL      , NULL },
	{ "IDNINT", 1, 1, IP_DEF_INTEGER, IP_DEF_DOUBLE, NULL },
	{ "IQNINT", 1, 1, IP_DEF_INTEGER, IP_REAL_16   , NULL },

	{ "ABS"  , 1, 1, IP_SCALAR    , IP_ANY               , NULL },
	{ "IABS" , 1, 1, IP_SAME      , IP_DEF_INTEGER       , NULL },
	{ "DABS" , 1, 1, IP_SAME      , IP_DEF_DOUBLE        , NULL },
	{ "CABS" , 1, 1, IP_DEF_REAL  , IP_DEF_COMPLEX       , NULL },
	{ "QABS" , 1, 1, IP_SAME      , IP_REAL_16           , NULL },
	{ "ZABS" , 1, 1, IP_DEF_DOUBLE, IP_DEF_DOUBLE_COMPLEX, NULL },
	{ "CDABS", 1, 1, IP_DEF_DOUBLE, IP_DEF_DOUBLE_COMPLEX, NULL },
	{ "CQABS", 1, 1, IP_SAME      , IP_COMPLEX_32        , NULL },

	{ "MOD"   , 2, 2, IP_SAME, IP_SCALAR    , NULL },
	{ "AMOD"  , 2, 2, IP_SAME, IP_DEF_REAL  , NULL },
	{ "DMOD"  , 2, 2, IP_SAME, IP_DEF_DOUBLE, NULL },
	{ "QMOD"  , 2, 2, IP_SAME, IP_REAL_16   , NULL },
	{ "MODULO", 2, 2, IP_SAME, IP_SCALAR    , NULL },

	{ "FLOOR"  , 1, 1, IP_SAME, IP_REAL, NULL },
	{ "CEILING", 1, 1, IP_SAME, IP_REAL, NULL },


	/* Transfer of sign */
	{ "SIGN" , 2, 2, IP_SAME, IP_SCALAR     , NULL },
	{ "ISIGN", 2, 2, IP_SAME, IP_DEF_INTEGER, NULL },
	{ "DSIGN", 2, 2, IP_SAME, IP_DEF_DOUBLE , NULL },
	{ "QSIGN", 2, 2, IP_SAME, IP_REAL_16    , NULL },

	/* Positive difference */
	{ "DIM" , 2, 2, IP_SAME, IP_SCALAR     , NULL },
	{ "IDIM", 2, 2, IP_SAME, IP_DEF_INTEGER, NULL },
	{ "DDIM", 2, 2, IP_SAME, IP_DEF_DOUBLE , NULL },
	{ "QDIM", 2, 2, IP_SAME, IP_REAL_16    , NULL },

	/* Inner product */
	{ "DPROD", 2, 2, IP_DEF_DOUBLE, IP_DEF_REAL, NULL },
	{ "QPROD", 2, 2, IP_DEF_DOUBLE, IP_REAL_16 , NULL },

	{ "MAX"  , 2, 0, IP_SAME       , IP_SCALAR     , NULL },
	{ "MAX0" , 2, 0, IP_SAME       , IP_DEF_INTEGER, NULL },
	{ "AMAX1", 2, 0, IP_SAME       , IP_DEF_REAL   , NULL },
	{ "DMAX1", 2, 0, IP_SAME       , IP_DEF_DOUBLE , NULL },
	{ "QMAX1", 2, 0, IP_SAME       , IP_REAL_16    , NULL },
	{ "AMAX0", 2, 0, IP_DEF_REAL   , IP_DEF_INTEGER, NULL },
	{ "MAX1" , 2, 0, IP_DEF_INTEGER, IP_DEF_REAL   , NULL },
	{ "MIN"  , 2, 0, IP_SAME       , IP_SCALAR     , NULL },
	{ "MIN0" , 2, 0, IP_SAME       , IP_DEF_INTEGER, NULL },
	{ "AMIN1", 2, 0, IP_SAME       , IP_DEF_REAL   , NULL },
	{ "DMIN1", 2, 0, IP_SAME       , IP_DEF_DOUBLE , NULL },
	{ "QMIN1", 2, 0, IP_SAME       , IP_REAL_16    , NULL },
	{ "AMIN0", 2, 0, IP_DEF_REAL   , IP_DEF_INTEGER, NULL },
	{ "MIN1" , 2, 0, IP_DEF_INTEGER, IP_DEF_REAL   , NULL },

	{ "IMAG" , 1, 1, IP_REAL      , IP_COMPLEX           , NULL},
	{ "AIMAG", 1, 1, IP_DEF_REAL  , IP_DEF_COMPLEX       , NULL},
	{ "DIMAG", 1, 1, IP_DEF_DOUBLE, IP_DEF_DOUBLE_COMPLEX, NULL},
	{ "QIMAG", 1, 1, IP_REAL_16   , IP_COMPLEX_32        , NULL},

	{ "CONJG" , 1, 1, IP_SAME, IP_COMPLEX           , NULL},
	{ "DCONJG", 1, 1, IP_SAME, IP_DEF_DOUBLE_COMPLEX, NULL},
	{ "QCONJG", 1, 1, IP_SAME, IP_COMPLEX_32        , NULL},

	{ "SQRT"  , 1, 1, IP_SAME, IP_ANY               , NULL },
	{ "DSQRT" , 1, 1, IP_SAME, IP_DEF_DOUBLE        , NULL },
	{ "QSQRT" , 1, 1, IP_SAME, IP_REAL_16           , NULL },
	{ "CSQRT" , 1, 1, IP_SAME, IP_DEF_COMPLEX       , NULL },
	{ "ZSQRT" , 1, 1, IP_SAME, IP_DEF_DOUBLE_COMPLEX, NULL },
	{ "CDSQRT", 1, 1, IP_SAME, IP_DEF_DOUBLE_COMPLEX, NULL },
	{ "CQSQRT", 1, 1, IP_SAME, IP_COMPLEX_32        , NULL },

	{ "CBRT"  , 1, 1, IP_SAME, IP_ANY               , NULL },
	{ "DCBRT" , 1, 1, IP_SAME, IP_DEF_DOUBLE        , NULL },
	{ "QCBRT" , 1, 1, IP_SAME, IP_REAL_16           , NULL },
	{ "CCBRT" , 1, 1, IP_SAME, IP_DEF_COMPLEX       , NULL },
	{ "ZCBRT" , 1, 1, IP_SAME, IP_DEF_DOUBLE_COMPLEX, NULL },
	{ "CDCBRT", 1, 1, IP_SAME, IP_DEF_DOUBLE_COMPLEX, NULL },
	{ "CQCBRT", 1, 1, IP_SAME, IP_COMPLEX_32        , NULL },

	{ "EXP"  , 1, 1, IP_SAME, IP_ANY               , NULL },
	{ "DEXP" , 1, 1, IP_SAME, IP_DEF_DOUBLE        , NULL },
	{ "QEXP" , 1, 1, IP_SAME, IP_REAL_16           , NULL },
	{ "CEXP" , 1, 1, IP_SAME, IP_DEF_COMPLEX       , NULL },
	{ "ZEXP" , 1, 1, IP_SAME, IP_DEF_DOUBLE_COMPLEX, NULL },
	{ "CDEXP", 1, 1, IP_SAME, IP_DEF_DOUBLE_COMPLEX, NULL },
	{ "CQEXP", 1, 1, IP_SAME, IP_COMPLEX_32        , NULL },

	{ "LOG"  , 1, 1, IP_SAME, IP_ANY               , NULL },
	{ "ALOG" , 1, 1, IP_SAME, IP_DEF_REAL          , NULL },
	{ "DLOG" , 1, 1, IP_SAME, IP_DEF_DOUBLE        , NULL },
	{ "QLOG" , 1, 1, IP_SAME, IP_REAL_16           , NULL },
	{ "CLOG" , 1, 1, IP_SAME, IP_DEF_COMPLEX       , NULL },
	{ "ZLOG" , 1, 1, IP_SAME, IP_DEF_DOUBLE_COMPLEX, NULL },
	{ "CDLOG", 1, 1, IP_SAME, IP_DEF_DOUBLE_COMPLEX, NULL },
	{ "CQLOG", 1, 1, IP_SAME, IP_COMPLEX_32        , NULL },

	{ "LOG10" , 1, 1, IP_SAME, IP_ANY       , NULL },
	{ "ALOG10", 1, 1, IP_SAME, IP_DEF_REAL  , NULL },
	{ "DLOG10", 1, 1, IP_SAME, IP_DEF_DOUBLE, NULL },
	{ "QLOG10", 1, 1, IP_SAME, IP_REAL_16   , NULL },

	/* Trigonometric functions */
	{ "SIN"  , 1, 1, IP_SAME, IP_ANY               , NULL },
	{ "DSIN" , 1, 1, IP_SAME, IP_DEF_DOUBLE        , NULL },
	{ "CSIN" , 1, 1, IP_SAME, IP_DEF_COMPLEX       , NULL },
	{ "QSIN" , 1, 1, IP_SAME, IP_REAL_16           , NULL },
	{ "ZSIN" , 1, 1, IP_SAME, IP_DEF_DOUBLE_COMPLEX, NULL },
	{ "CDSIN", 1, 1, IP_SAME, IP_DEF_DOUBLE_COMPLEX, NULL },
	{ "CQSIN", 1, 1, IP_SAME, IP_COMPLEX_32        , NULL },
	{ "SIND" , 1, 1, IP_SAME, IP_ANY               , NULL },
	{ "DSIND", 1, 1, IP_SAME, IP_DEF_DOUBLE        , NULL },
	{ "QSIND", 1, 1, IP_SAME, IP_REAL_16           , NULL },

	{ "COS"  , 1, 1, IP_SAME, IP_ANY               , NULL },
	{ "DCOS" , 1, 1, IP_SAME, IP_DEF_DOUBLE        , NULL },
	{ "CCOS" , 1, 1, IP_SAME, IP_DEF_COMPLEX       , NULL },
	{ "QCOS" , 1, 1, IP_SAME, IP_REAL_16           , NULL },
	{ "ZCOS" , 1, 1, IP_SAME, IP_DEF_DOUBLE_COMPLEX, NULL },
	{ "CDCOS", 1, 1, IP_SAME, IP_DEF_DOUBLE_COMPLEX, NULL },
	{ "CQCOS", 1, 1, IP_SAME, IP_COMPLEX_32        , NULL },
	{ "COSD" , 1, 1, IP_SAME, IP_ANY               , NULL },
	{ "DCOSD", 1, 1, IP_SAME, IP_DEF_DOUBLE        , NULL },
	{ "QCOSD", 1, 1, IP_SAME, IP_REAL_16           , NULL },

	{ "TAN"  , 1, 1, IP_SAME, IP_ANY       , NULL },
	{ "DTAN" , 1, 1, IP_SAME, IP_DEF_DOUBLE, NULL },
	{ "QTAN" , 1, 1, IP_SAME, IP_REAL_16   , NULL },
	{ "TAND" , 1, 1, IP_SAME, IP_ANY       , NULL },
	{ "DTAND", 1, 1, IP_SAME, IP_DEF_DOUBLE, NULL },
	{ "QTAND", 1, 1, IP_SAME, IP_REAL_16   , NULL },

	{ "ASIN"  , 1, 1, IP_SAME, IP_ANY       , NULL },
	{ "DASIN" , 1, 1, IP_SAME, IP_DEF_DOUBLE, NULL },
	{ "QASIN" , 1, 1, IP_SAME, IP_REAL_16   , NULL },
	{ "ASIND" , 1, 1, IP_SAME, IP_ANY       , NULL },
	{ "DASIND", 1, 1, IP_SAME, IP_DEF_DOUBLE, NULL },
	{ "QASIND", 1, 1, IP_SAME, IP_REAL_16   , NULL },

	{ "ACOS"  , 1, 1, IP_SAME, IP_ANY       , NULL },
	{ "DACOS" , 1, 1, IP_SAME, IP_DEF_DOUBLE, NULL },
	{ "QACOS" , 1, 1, IP_SAME, IP_REAL_16   , NULL },
	{ "ACOSD" , 1, 1, IP_SAME, IP_ANY       , NULL },
	{ "DACOSD", 1, 1, IP_SAME, IP_DEF_DOUBLE, NULL },
	{ "QACOSD", 1, 1, IP_SAME, IP_REAL_16   , NULL },

	{ "ATAN"  , 1, 2, IP_SAME, IP_ANY       , NULL },
	{ "DATAN" , 1, 2, IP_SAME, IP_DEF_DOUBLE, NULL },
	{ "QATAN" , 1, 2, IP_SAME, IP_REAL_16   , NULL },
	{ "ATAN2" , 2, 2, IP_SAME, IP_ANY       , NULL },
	{ "DATAN2", 2, 2, IP_SAME, IP_DEF_DOUBLE, NULL },
	{ "QATAN2", 2, 2, IP_SAME, IP_REAL_16   , NULL },

	{ "SINH" , 1, 1, IP_SAME, IP_ANY       , NULL },
	{ "DSINH", 1, 1, IP_SAME, IP_DEF_DOUBLE, NULL },
	{ "QSINH", 1, 1, IP_SAME, IP_REAL_16   , NULL },

	{ "COSH" , 1, 1, IP_SAME, IP_ANY       , NULL },
	{ "DCOSH", 1, 1, IP_SAME, IP_DEF_DOUBLE, NULL },
	{ "QCOSH", 1, 1, IP_SAME, IP_REAL_16   , NULL },

	{ "TANH" , 1, 1, IP_SAME, IP_ANY       , NULL },
	{ "DTANH", 1, 1, IP_SAME, IP_DEF_DOUBLE, NULL },
	{ "QTANH", 1, 1, IP_SAME, IP_REAL_16   , NULL },

	{ "ASINH" , 1, 1, IP_SAME, IP_ANY       , NULL },
	{ "DASINH", 1, 1, IP_SAME, IP_DEF_DOUBLE, NULL },
	{ "QASINH", 1, 1, IP_SAME, IP_REAL_16   , NULL },

	{ "ACOSH" , 1, 1, IP_SAME, IP_ANY       , NULL },
	{ "DACOSH", 1, 1, IP_SAME, IP_DEF_DOUBLE, NULL },
	{ "QACOSH", 1, 1, IP_SAME, IP_REAL_16   , NULL },

	{ "ATANH" , 1, 1, IP_SAME, IP_ANY       , NULL },
	{ "DATANH", 1, 1, IP_SAME, IP_DEF_DOUBLE, NULL },
	{ "QATANH", 1, 1, IP_SAME, IP_REAL_16   , NULL },

	{ "IAND", 2, 2, IP_SAME, IP_INTEGER, ofc_sema_intrinsic_op__constant_iand },
	{ "IEOR", 2, 2, IP_SAME, IP_INTEGER, ofc_sema_intrinsic_op__constant_ieor },
	{ "IOR" , 2, 2, IP_SAME, IP_INTEGER, ofc_sema_intrinsic_op__constant_ior },
	{ "NOT" , 1, 1, IP_SAME, IP_INTEGER, ofc_sema_intrinsic_op__constant_not },

	{ NULL, 0, 0, 0, 0, NULL }
};

/* For the specific functions that are also generic */
static const ofc_sema_intrinsic_op_t ofc_sema_intrinsic__op_list_override[] =
{
	{ "AINT" , 1, 1, IP_SAME,        IP_DEF_REAL    , NULL },
	{ "ANINT", 1, 1, IP_SAME,        IP_DEF_REAL    , NULL },
	{ "NINT" , 1, 1, IP_DEF_INTEGER, IP_DEF_REAL    , NULL },
	{ "ABS"  , 1, 1, IP_SAME,        IP_DEF_REAL    , NULL },
	{ "MOD"  , 2, 2, IP_SAME,        IP_DEF_INTEGER , NULL },
	{ "SIGN" , 2, 2, IP_SAME,        IP_DEF_REAL    , NULL },
	{ "DIM"  , 2, 2, IP_SAME,        IP_DEF_REAL    , NULL },
	{ "SQRT" , 1, 1, IP_SAME,        IP_DEF_REAL    , NULL },
	{ "EXP"  , 1, 1, IP_SAME,        IP_DEF_REAL    , NULL },
	{ "SIN"  , 1, 1, IP_SAME,        IP_DEF_REAL    , NULL },
	{ "COS"  , 1, 1, IP_SAME,        IP_DEF_REAL    , NULL },
	{ "TAN"  , 1, 1, IP_SAME,        IP_DEF_REAL    , NULL },
	{ "ASIN" , 1, 1, IP_SAME,        IP_DEF_REAL    , NULL },
	{ "ACOS" , 1, 1, IP_SAME,        IP_DEF_REAL    , NULL },
	{ "ATAN" , 1, 2, IP_SAME,        IP_DEF_REAL    , NULL },
	{ "ATAN2", 2, 2, IP_SAME,        IP_DEF_REAL    , NULL },
	{ "SINH" , 1, 1, IP_SAME,        IP_DEF_REAL    , NULL },
	{ "COSH" , 1, 1, IP_SAME,        IP_DEF_REAL    , NULL },
	{ "TANH" , 1, 1, IP_SAME,        IP_DEF_REAL    , NULL },
	{ "ASINH", 1, 1, IP_SAME,        IP_DEF_REAL    , NULL },
	{ "ACOSH", 1, 1, IP_SAME,        IP_DEF_REAL    , NULL },
	{ "ATANH", 1, 1, IP_SAME,        IP_DEF_REAL    , NULL },
	{ "DBLE" , 1, 1, IP_DEF_DOUBLE,  IP_INTEGER     , ofc_sema_intrinsic_op__constant_cast },
	{ "QEXT" , 1, 1, IP_REAL_16   ,  IP_DEF_INTEGER , ofc_sema_intrinsic_op__constant_cast },

	{ NULL, 0, 0, 0, 0, NULL }

};



static const ofc_sema_type_t* ofc_sema_intrinsic__len_rt(
	const ofc_sema_dummy_arg_list_t* args)
{
	ofc_sema_kind_e kind = OFC_SEMA_KIND_DEFAULT;
	if (args->count >= 2)
	{
		const ofc_sema_type_t* type
			= ofc_sema_dummy_arg_type(args->dummy_arg[1]);
		if (!type) return NULL;
		kind = type->kind;
	}

	return ofc_sema_type_create_primitive(
		OFC_SEMA_TYPE_INTEGER, kind);
}

static ofc_sema_typeval_t* ofc_sema_intrinsic__len_tv(
	const ofc_sema_intrinsic_t* intrinsic,
	const ofc_sema_dummy_arg_list_t* args)
{
	(void)intrinsic;

	if (args->count > 2)
		return NULL;

	ofc_sema_kind_e kind = OFC_SEMA_KIND_DEFAULT;
	if (args->count > 1)
	{
		const ofc_sema_type_t* kt
			= ofc_sema_dummy_arg_type(args->dummy_arg[1]);
		if (!kt) return NULL;
		kind = kt->kind;
	}

	ofc_sema_expr_t* expr
		= ofc_sema_dummy_arg_get_expr(
			args->dummy_arg[0]);
	const ofc_sema_typeval_t* ctv
		= ofc_sema_expr_constant(expr);
	if (!ctv || !ofc_sema_type_is_character(ctv->type)
		|| ctv->type->len_var)
		return NULL;

	int cl = (int)ctv->type->len;
	if ((unsigned)cl != ctv->type->len)
		return NULL;

	return ofc_sema_typeval_create_integer(
		cl, kind, expr->src);
}

static const ofc_sema_type_t* ofc_sema_intrinsic__char_rt(
	const ofc_sema_dummy_arg_list_t* args)
{
	ofc_sema_kind_e kind = OFC_SEMA_KIND_DEFAULT;
	if (args->count >= 2)
	{
		const ofc_sema_type_t* type
			= ofc_sema_dummy_arg_type(args->dummy_arg[1]);
		if (!type) return NULL;
		kind = type->kind;
	}

	return ofc_sema_type_create_character(
		kind, 1, false);
}

static ofc_sema_typeval_t* ofc_sema_intrinsic__char_tv(
	const ofc_sema_intrinsic_t* intrinsic,
	const ofc_sema_dummy_arg_list_t* args)
{
	(void)intrinsic;

	if (args->count > 2)
		return NULL;

	ofc_sema_kind_e kind = OFC_SEMA_KIND_DEFAULT;
	if (args->count > 1)
	{
		const ofc_sema_type_t* kt
			= ofc_sema_dummy_arg_type(args->dummy_arg[1]);
		if (!kt) return NULL;
		kind = kt->kind;
	}

	ofc_sema_expr_t* expr
		= ofc_sema_dummy_arg_get_expr(
			args->dummy_arg[0]);
	const ofc_sema_typeval_t* ctv
		= ofc_sema_expr_constant(expr);
	if (!ctv) return NULL;

	const ofc_sema_type_t* nt = ofc_sema_type_create_character(kind, 1, false);

	int64_t ic;
	if (!ofc_sema_typeval_get_integer(ctv, &ic))
		return NULL;

	if (ic < 0)
	{
		ofc_sparse_ref_warning(expr->src,
			"Can't convert negative INTEGER to CHARACTER");
		return NULL;
	}

	unsigned nts;
	if (!ofc_sema_type_size(nt, &nts))
		return NULL;

	if ((nts < 8) && ((uint64_t)ic >= (1ULL << (nts * 8U))))
	{
		ofc_sparse_ref_warning(expr->src,
			"INTEGER too large to convert to CHARACTER of kind %u", kind);
		return NULL;
	}

	return ofc_sema_typeval_create_character(
		(char*)&ic, kind, 1, expr->src);
}

static const ofc_sema_type_t* ofc_sema_intrinsic__ichar_rt(
	const ofc_sema_dummy_arg_list_t* args)
{
	ofc_sema_kind_e kind = OFC_SEMA_KIND_DEFAULT;
	if (args->count >= 2)
	{
		const ofc_sema_type_t* type
			= ofc_sema_dummy_arg_type(args->dummy_arg[1]);
		if (!type) return NULL;
		kind = type->kind;
	}

	return ofc_sema_type_create_primitive(
		OFC_SEMA_TYPE_INTEGER, kind);
}

static ofc_sema_typeval_t* ofc_sema_intrinsic__ichar_tv(
	const ofc_sema_intrinsic_t* intrinsic,
	const ofc_sema_dummy_arg_list_t* args)
{
	(void)intrinsic;

	if (args->count > 2)
		return NULL;

	ofc_sema_kind_e kind = OFC_SEMA_KIND_DEFAULT;
	if (args->count > 1)
	{
		const ofc_sema_type_t* kt
			= ofc_sema_dummy_arg_type(args->dummy_arg[1]);
		if (!kt) return NULL;
		kind = kt->kind;
	}

	ofc_sema_expr_t* expr
		= ofc_sema_dummy_arg_get_expr(
			args->dummy_arg[0]);
	const ofc_sema_typeval_t* ctv
		= ofc_sema_expr_constant(expr);
	if (!ctv || !ofc_sema_type_is_character(ctv->type))
		return NULL;

	const ofc_sema_type_t* nt
		= ofc_sema_type_create_primitive(OFC_SEMA_TYPE_INTEGER, kind);

	unsigned nts;
	if (!ofc_sema_type_size(nt, &nts))
		return NULL;

	const ofc_sema_type_t* ct
		= ofc_sema_type_create_character(ctv->type->kind, 1, false);

	unsigned cts;
	if (!ofc_sema_type_size(ct, &cts))
		return NULL;

	if (cts > nts)
	{
		ofc_sparse_ref_warning(expr->src,
			"CHARACTER too large to fit in INTEGER of kind %u", kind);
		return NULL;
	}

	if (cts > 4) return NULL;

	int ic = ' ';
	if (ctv->type->len >= 1)
		memcpy(&ic, ctv->character, cts);
	if (ic < 0) return NULL;

	return ofc_sema_typeval_create_integer(
		ic, kind, expr->src);
}

static const ofc_sema_type_t* ofc_sema_intrinsic__transfer_rt(
	const ofc_sema_dummy_arg_list_t* args)
{
	if (args->count < 2)
		return NULL;

	if (args->count >= 3)
	{
		/* TODO - INTRINSIC - Use 3rd parameter as array size if present. */
		ofc_sparse_ref_error(args->dummy_arg[2]->src,
			"TRANSFER SIZE argument not yet supported");
		return NULL;
	}

	return ofc_sema_dummy_arg_type(args->dummy_arg[1]);
}

static ofc_sema_typeval_t* ofc_sema_intrinsic__transfer_tv(
	const ofc_sema_intrinsic_t* intrinsic,
	const ofc_sema_dummy_arg_list_t* args)
{
	(void)intrinsic;

	/* TODO - INTRINSIC - Use 3rd parameter as array size if present. */
	if ((args->count < 2)
		|| (args->count >= 3))
		return NULL;

	ofc_sema_expr_t* expr_atv
		= ofc_sema_dummy_arg_get_expr(
			args->dummy_arg[1]);
	if (!expr_atv) return NULL;

	const ofc_sema_typeval_t* atv
		= ofc_sema_expr_constant(expr_atv);
	if (!atv) return NULL;

	const ofc_sema_type_t* atype = atv->type;
	if (!atype) return NULL;

	const ofc_sema_type_t* rtype
		= ofc_sema_intrinsic__transfer_rt(args);

	unsigned asize, rsize;
	if (!ofc_sema_type_size(atype, &asize)
		|| !ofc_sema_type_size(rtype, &rsize))
		return NULL;

	uint8_t buff[asize];
	memset(buff, 0x00, asize);

	const void* src = buff;
	switch (atv->type->type)
	{
		case OFC_SEMA_TYPE_LOGICAL:
			buff[0] = (atv->logical ? 1 : 0);
			break;

		case OFC_SEMA_TYPE_INTEGER:
		case OFC_SEMA_TYPE_BYTE:
			if (asize > 8)
				return NULL;
			src = &atv->integer;
			break;

		case OFC_SEMA_TYPE_REAL:
		{
			if (asize == 4)
			{
				float f32 = atv->real;
				memcpy(buff, &f32, 4);
			}
			else if (asize == 8)
			{
				double f64 = atv->real;
				memcpy(buff, &f64, 8);
			}
			else if (asize == 10)
			{
				long double f80 = atv->real;
				memcpy(buff, &f80, 10);
			}
			else
			{
				return NULL;
			}
			break;
		}

		case OFC_SEMA_TYPE_COMPLEX:
		{
			if (asize == 8)
			{
				float f32[2] =
				{
					atv->complex.real,
					atv->complex.imaginary
				};
				memcpy(buff, f32, 8);
			}
			else if (asize == 16)
			{
				double f64[2] =
				{
					atv->complex.real,
					atv->complex.imaginary
				};
				memcpy(buff, f64, 16);
			}
			else if (asize == 20)
			{
				long double f80[2] =
				{
					atv->complex.real,
					atv->complex.imaginary
				};
				memcpy(buff, f80, 20);
			}
			else
			{
				return NULL;
			}
			break;
		}

		case OFC_SEMA_TYPE_CHARACTER:
			src = atv->character;
			if (!src) return NULL;
			break;

		default:
			return NULL;
	}

	/* TODO - INTRINSIC - Retain location in typeval. */
	ofc_sema_typeval_t* rtv
		= ofc_sema_typeval_create_integer(
			0, OFC_SEMA_KIND_DEFAULT,
			OFC_SPARSE_REF_EMPTY);
	if (!rtv) return NULL;
	rtv->type = rtype;

	if (ofc_sema_type_is_character(rtype))
	{
		rtv->character = (char*)malloc(rsize);
		if (!rtv->character)
		{
			ofc_sema_typeval_delete(rtv);
			return NULL;
		}

		if (asize > rsize)
			asize = rsize;

		memset(rtv->character, 0x00, rsize);
		memcpy(rtv->character, src, asize);
	}
	else
	{
		switch (rtype->type)
		{
			case OFC_SEMA_TYPE_LOGICAL:
			{
				uint8_t zero[asize];
				memset(zero, 0x00, asize);
				rtv->logical = (memcmp(src, zero, asize) != 0);
				break;
			}

			case OFC_SEMA_TYPE_INTEGER:
			case OFC_SEMA_TYPE_BYTE:
				/* We can't fold constants this large. */
				if (rsize > 8)
				{
					ofc_sema_typeval_delete(rtv);
					return NULL;
				}

				if (asize < rsize)
					rsize = asize;

				rtv->integer = 0;
				memcpy(&rtv->integer, src, rsize);
				break;

			case OFC_SEMA_TYPE_REAL:
			{
				if (asize > rsize)
					asize = rsize;

				if (rsize == 4)
				{
					float f32 = 0.0f;
					memcpy(&f32, src, asize);
					rtv->real = f32;
				}
				else if (rsize == 8)
				{
					double f64 = 0.0;
					memcpy(&f64, src, asize);
					rtv->real = f64;
				}
				else if (rsize == 10)
				{
					long double f80 = 0.0;
					memcpy(&f80, src, asize);
					rtv->real = f80;
				}
				else
				{
					/* We can't fold obscure float constants. */
					ofc_sema_typeval_delete(rtv);
					return NULL;
				}

				break;
			}

			case OFC_SEMA_TYPE_COMPLEX:
			{
				if (asize > rsize)
					asize = rsize;

				if (rsize == 8)
				{
					float f32[2] = { 0.0f, 0.0f };
					memcpy(f32, src, asize);
					rtv->complex.real = f32[0];
					rtv->complex.imaginary = f32[1];
				}
				else if (rsize == 16)
				{
					double f64[2] = { 0.0, 0.0 };
					memcpy(f64, src, asize);
					rtv->complex.real = f64[0];
					rtv->complex.imaginary = f64[1];
				}
				else if (rsize == 20)
				{
					long double f80[2] = { 0.0, 0.0 };
					memcpy(f80, src, asize);
					rtv->complex.real = f80[0];
					rtv->complex.imaginary = f80[1];
				}
				else
				{
					/* We can't fold obscure float constants. */
					ofc_sema_typeval_delete(rtv);
					return NULL;
				}

				break;
			}

			default:
				ofc_sema_typeval_delete(rtv);
				return NULL;
		}
	}

	return rtv;
}



typedef struct
{
	const char*                 name;
	unsigned                    arg_min, arg_max;
	ofc_sema_intrinsic__param_e return_type;
	ofc_sema_intrinsic__param_e arg_type[4];

	const ofc_sema_type_t* (*return_type_callback)(const ofc_sema_dummy_arg_list_t*);

	ofc_sema_typeval_t* (*constant)(
		const ofc_sema_intrinsic_t*,
		const ofc_sema_dummy_arg_list_t*);
} ofc_sema_intrinsic_func_t;

static const ofc_sema_intrinsic_func_t ofc_sema_intrinsic__func_list[] =
{
	{ "MClock",  0, 0, IP_INTEGER_1, { 0 }, NULL, NULL },
	{ "MClock8", 0, 0, IP_INTEGER_2, { 0 }, NULL, NULL },
	{ "FDate",   0, 0, IP_CHARACTER, { 0 }, NULL, NULL },
	{ "Second",  0, 0, IP_DEF_REAL,  { 0 }, NULL, NULL },

	{ "Loc",      1, 1, IP_DEF_INTEGER, { IP_ANY           }, NULL, NULL },
	{ "IRand",    0, 1, IP_DEF_INTEGER, { IP_INTEGER       }, NULL, NULL },
	{ "LnBlnk",   1, 1, IP_DEF_INTEGER, { IP_CHARACTER     }, NULL, NULL },
	{ "IsaTty",   1, 1, IP_DEF_LOGICAL, { IP_INTEGER       }, NULL, NULL },
	{ "AImag",    1, 1, IP_REAL,        { IP_DEF_COMPLEX   }, NULL, NULL },
	{ "Len_Trim", 1, 1, IP_DEF_INTEGER, { IP_CHARACTER     }, NULL, NULL },
	{ "BesJ0",    1, 1, IP_REAL,        { IP_REAL          }, NULL, NULL },
	{ "BesJ1",    1, 1, IP_REAL,        { IP_REAL          }, NULL, NULL },
	{ "BesJN",    1, 1, IP_DEF_INTEGER, { IP_REAL          }, NULL, NULL },
	{ "BesY0",    1, 1, IP_REAL,        { IP_REAL          }, NULL, NULL },
	{ "BesY1",    1, 1, IP_REAL,        { IP_REAL          }, NULL, NULL },
	{ "CTime",    1, 1, IP_CHARACTER,   { IP_INTEGER       }, NULL, NULL },
	{ "DErF",     1, 1, IP_DEF_DOUBLE,  { IP_DEF_DOUBLE    }, NULL, NULL },
	{ "DErFC",    1, 1, IP_DEF_DOUBLE,  { IP_DEF_DOUBLE    }, NULL, NULL },
	{ "ErF",      1, 1, IP_REAL,        { IP_REAL          }, NULL, NULL },
	{ "ErFC",     1, 1, IP_REAL,        { IP_REAL          }, NULL, NULL },
	{ "ETime",    1, 1, IP_DEF_REAL,    { IP_DEF_REAL_A2   }, NULL, NULL },
	{ "FTell",    1, 1, IP_DEF_INTEGER, { IP_INTEGER       }, NULL, NULL },
	{ "GetCWD",   1, 1, IP_DEF_INTEGER, { IP_CHARACTER_OUT }, NULL, NULL },
	{ "HostNm",   1, 1, IP_DEF_INTEGER, { IP_CHARACTER_OUT }, NULL, NULL },
	{ "TtyNam",   1, 1, IP_CHARACTER,   { IP_INTEGER       }, NULL, NULL },

	{ "Stat",   2, 2, IP_DEF_INTEGER, { IP_CHARACTER, IP_INTEGER_A13_OUT }, NULL, NULL },
	{ "LStat",  2, 2, IP_DEF_INTEGER, { IP_CHARACTER, IP_INTEGER_A13_OUT }, NULL, NULL },
	{ "FStat",  2, 2, IP_DEF_INTEGER, { IP_INTEGER  , IP_INTEGER_A13_OUT }, NULL, NULL },
	{ "Access", 2, 2, IP_DEF_INTEGER, { IP_CHARACTER, IP_CHARACTER       }, NULL, NULL },
	{ "LGe",    2, 2, IP_DEF_LOGICAL, { IP_CHARACTER, IP_CHARACTER       }, NULL, NULL },
	{ "LGt",    2, 2, IP_DEF_LOGICAL, { IP_CHARACTER, IP_CHARACTER       }, NULL, NULL },
	{ "LLe",    2, 2, IP_DEF_LOGICAL, { IP_CHARACTER, IP_CHARACTER       }, NULL, NULL },
	{ "LLt",    2, 2, IP_DEF_LOGICAL, { IP_CHARACTER, IP_CHARACTER       }, NULL, NULL },
	{ "LShift", 2, 2, IP_DEF_INTEGER, { IP_INTEGER  , IP_INTEGER         }, NULL, NULL },
	{ "IShft",  2, 2, IP_INTEGER,     { IP_INTEGER  , IP_INTEGER         }, NULL, NULL },
	{ "BesYN",  2, 2, IP_REAL,        { IP_INTEGER  , IP_REAL            }, NULL, NULL },
	{ "BTest",  2, 2, IP_DEF_LOGICAL, { IP_INTEGER  , IP_INTEGER         }, NULL, NULL },

	{ "IShftC", 3, 3, IP_INTEGER, { IP_INTEGER, IP_INTEGER, IP_INTEGER }, NULL, NULL },

	{ "Len", 1, 2, IP_CALLBACK, { IP_CHARACTER, IP_INTEGER },
		ofc_sema_intrinsic__len_rt, ofc_sema_intrinsic__len_tv },

	{ "Char" , 1, 2, IP_CALLBACK, { IP_INTEGER    , IP_INTEGER },
		ofc_sema_intrinsic__char_rt, ofc_sema_intrinsic__char_tv },
	{ "AChar", 1, 2, IP_CALLBACK, { IP_INTEGER    , IP_INTEGER },
		ofc_sema_intrinsic__char_rt, ofc_sema_intrinsic__char_tv },
	{ "IChar", 1, 2, IP_CALLBACK, { IP_CHARACTER_1, IP_INTEGER },
		ofc_sema_intrinsic__ichar_rt, ofc_sema_intrinsic__ichar_tv },
	{ "IAChar", 1, 2, IP_CALLBACK, { IP_CHARACTER_1, IP_INTEGER },
		ofc_sema_intrinsic__ichar_rt, ofc_sema_intrinsic__ichar_tv },

	{ "Index", 2, 4, IP_INTEGER, { IP_CHARACTER, IP_CHARACTER, IP_LOGICAL, IP_INTEGER },
		NULL, NULL },

	{ "Transfer", 2, 3, IP_CALLBACK, { IP_ANY, IP_ANY, IP_INTEGER },
		ofc_sema_intrinsic__transfer_rt,
		ofc_sema_intrinsic__transfer_tv },

	{ NULL, 0, 0, 0, { 0 }, NULL, NULL }
};

static const ofc_sema_intrinsic_func_t ofc_sema_intrinsic__subr_list[] =
{
	{ "ITime",  1, 1, 0, { IP_INTEGER_A3_OUT }, NULL, NULL },
	{ "FDate",  1, 1, 0, { IP_CHARACTER_OUT  }, NULL, NULL },
	{ "Second", 1, 1, 0, { IP_REAL_OUT       }, NULL, NULL },

	{ "ChDir",  1, 2, 0, { IP_CHARACTER      , IP_INTEGER_OUT   }, NULL, NULL },
	{ "LTime",  2, 2, 0, { IP_INTEGER        , IP_CHARACTER_OUT }, NULL, NULL },
	{ "CTime",  2, 2, 0, { IP_INTEGER        , IP_CHARACTER_OUT }, NULL, NULL },
	{ "DTime",  2, 2, 0, { IP_DEF_REAL_A2_OUT, IP_REAL_OUT      }, NULL, NULL },
	{ "ETime",  2, 2, 0, { IP_DEF_REAL_A2_OUT, IP_REAL_OUT      }, NULL, NULL },
	{ "FGet",   1, 2, 0, { IP_CHARACTER_OUT  , IP_INTEGER_OUT   }, NULL, NULL },
	{ "FPut",   1, 2, 0, { IP_CHARACTER      , IP_INTEGER_OUT   }, NULL, NULL },
	{ "FTell",  2, 2, 0, { IP_INTEGER        , IP_INTEGER_OUT   }, NULL, NULL },
	{ "GetCWD", 1, 2, 0, { IP_CHARACTER_OUT  , IP_INTEGER_OUT   }, NULL, NULL },
	{ "HostNm", 1, 2, 0, { IP_CHARACTER_OUT  , IP_INTEGER_OUT   }, NULL, NULL },
	{ "System", 1, 2, 0, { IP_CHARACTER      , IP_INTEGER_OUT   }, NULL, NULL },
	{ "TtyNam", 2, 2, 0, { IP_INTEGER        , IP_CHARACTER_OUT }, NULL, NULL },
	{ "UMask",  1, 2, 0, { IP_INTEGER        , IP_INTEGER_OUT   }, NULL, NULL },
	{ "Unlink", 1, 2, 0, { IP_CHARACTER      , IP_INTEGER_OUT   }, NULL, NULL },

	{ "ChMod",  2, 3, 0, { IP_CHARACTER, IP_CHARACTER      , IP_INTEGER_OUT }, NULL, NULL },
	{ "SymLnk", 2, 3, 0, { IP_CHARACTER, IP_CHARACTER      , IP_INTEGER_OUT }, NULL, NULL },
	{ "Kill",   2, 3, 0, { IP_INTEGER  , IP_INTEGER        , IP_INTEGER_OUT }, NULL, NULL },
	{ "Stat",   2, 3, 0, { IP_CHARACTER, IP_INTEGER_A13_OUT, IP_INTEGER_OUT }, NULL, NULL },
	{ "FStat",  2, 3, 0, { IP_INTEGER  , IP_INTEGER_A13_OUT, IP_INTEGER_OUT }, NULL, NULL },
	{ "LStat",  2, 3, 0, { IP_CHARACTER, IP_INTEGER_A13_OUT, IP_INTEGER_OUT }, NULL, NULL },
	{ "Alarm",  2, 3, 0, { IP_INTEGER  , IP_INTEGER_A13    , IP_INTEGER_OUT }, NULL, NULL },
	{ "FGetC",  2, 3, 0, { IP_INTEGER  , IP_CHARACTER_OUT  , IP_INTEGER_OUT }, NULL, NULL },
	{ "FPutC",  2, 3, 0, { IP_INTEGER  , IP_CHARACTER      , IP_INTEGER_OUT }, NULL, NULL },
	{ "Link",   2, 3, 0, { IP_CHARACTER, IP_CHARACTER      , IP_INTEGER_OUT }, NULL, NULL },
	{ "Rename", 2, 3, 0, { IP_CHARACTER, IP_CHARACTER      , IP_INTEGER_OUT }, NULL, NULL },

	{ NULL, 0, 0, 0, { 0 }, NULL, NULL }
};


static const ofc_sema_type_t* ofc_sema_intrinsic__param_rtype(
	ofc_sema_intrinsic__param_e param,
	const ofc_sema_dummy_arg_list_t* args,
	const ofc_sema_type_t* (*callback)(const ofc_sema_dummy_arg_list_t*))
{
	if (param >= IP_COUNT)
		return NULL;

	const ofc_sema_intrinsic__param_t p
		= ofc_sema_intrinsic__param[param];

	const ofc_sema_type_t* stype = NULL;
	if (args && (args->count > 0))
		stype = ofc_sema_dummy_arg_type(args->dummy_arg[0]);

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
			if (!callback) return NULL;
			return callback(args);

		default:
			return NULL;
	}

	/* Return value can never be an array. */
	if ((p.type != OFC_SEMA_TYPE_CHARACTER)
		&& (p.size != 0))
		return NULL;

	if (!rtype && stype
		&& (p.kind == OFC_SEMA_KIND_NONE))
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
			if ((kind == OFC_SEMA_KIND_NONE)
				&& ofc_sema_type_is_character(stype))
				kind = stype->kind;
			if (kind == OFC_SEMA_KIND_NONE)
				kind = OFC_SEMA_KIND_DEFAULT;

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
			if (kind == OFC_SEMA_KIND_NONE)
				kind = OFC_SEMA_KIND_DEFAULT;

			rtype = ofc_sema_type_create_primitive(
				p.type, kind);

			if (p.kind == OFC_SEMA_KIND_NONE)
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

static ofc_hashmap_t* ofc_sema_intrinsic__op_map          = NULL;
static ofc_hashmap_t* ofc_sema_intrinsic__op_override_map = NULL;
static ofc_hashmap_t* ofc_sema_intrinsic__func_map        = NULL;
static ofc_hashmap_t* ofc_sema_intrinsic__subr_map        = NULL;

static void ofc_sema_intrinsic__term(void)
{
	ofc_hashmap_delete(ofc_sema_intrinsic__op_map);
	ofc_hashmap_delete(ofc_sema_intrinsic__op_override_map);
	ofc_hashmap_delete(ofc_sema_intrinsic__func_map);
	ofc_hashmap_delete(ofc_sema_intrinsic__subr_map);
}

static bool ofc_sema_intrinsic__op_map_init(void)
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

static bool ofc_sema_intrinsic__op_override_map_init(void)
{
	ofc_sema_intrinsic__op_override_map = ofc_hashmap_create(
		(void*)ofc_str_ref_ptr_hash_ci,
		(void*)ofc_str_ref_ptr_equal_ci,
		(void*)ofc_sema_intrinsic__key,
		(void*)ofc_sema_intrinsic__delete);
	if (!ofc_sema_intrinsic__op_override_map)
		return false;

	unsigned i;
	for (i = 0; ofc_sema_intrinsic__op_list_override[i].name; i++)
	{
		ofc_sema_intrinsic_t* intrinsic
			= ofc_sema_intrinsic__create_op(
				&ofc_sema_intrinsic__op_list_override[i]);
		if (!intrinsic)
		{
			ofc_hashmap_delete(
				ofc_sema_intrinsic__op_override_map);
			ofc_sema_intrinsic__op_override_map = NULL;
			return false;
		}

		if (!ofc_hashmap_add(
			ofc_sema_intrinsic__op_override_map,
			intrinsic))
		{
			ofc_sema_intrinsic__delete(intrinsic);
			ofc_hashmap_delete(
				ofc_sema_intrinsic__op_override_map);
			ofc_sema_intrinsic__op_override_map = NULL;
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
			= ofc_sema_intrinsic__create_func(
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
		&& ofc_sema_intrinsic__op_override_map
		&& ofc_sema_intrinsic__func_map
		&& ofc_sema_intrinsic__subr_map)
		return true;

	/* TODO - Set case sensitivity based on lang_opts? */
	if (!ofc_sema_intrinsic__op_map_init()
		|| !ofc_sema_intrinsic__op_override_map_init()
		|| !ofc_sema_intrinsic__func_map_init()
		|| !ofc_sema_intrinsic__subr_map_init())
		return false;

	atexit(ofc_sema_intrinsic__term);
	return true;
}


const ofc_sema_intrinsic_t* ofc_sema_intrinsic(
	ofc_str_ref_t name, bool case_sensitive)
{
	if (!ofc_sema_intrinsic__init())
		return NULL;

	const ofc_sema_intrinsic_t* func = ofc_hashmap_find(
		ofc_sema_intrinsic__op_map, &name);
	if (!func)
	{
		func = ofc_hashmap_find(
			ofc_sema_intrinsic__func_map, &name);
		if (!func) return NULL;
	}

	if (case_sensitive
		&& !ofc_str_ref_equal(
			name, func->name))
		return NULL;

	return func;
}

bool ofc_sema_intrinsic_is_specific(
	const ofc_sema_intrinsic_t* func)
{
	if (!func)
		return false;

	if (func->type == OFC_SEMA_INTRINSIC_OP)
	{
		return (func->op->arg_type != IP_ANY
				&& func->op->arg_type != IP_SCALAR);
	}

	return false;
}

bool ofc_sema_stmt_intrinsic(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt || !scope
		|| (stmt->type != OFC_PARSE_STMT_DECL_ATTR_INTRINSIC))
		return false;

	if (!ofc_sema_intrinsic__init())
		return NULL;

	unsigned i;
	for (i = 0; i < stmt->decl_attr.count; i++)
	{
		ofc_sparse_ref_t decl_name = *stmt->decl_attr.name[i];

		const ofc_sema_intrinsic_t* func = ofc_hashmap_find(
			ofc_sema_intrinsic__op_override_map, &decl_name.string);
		if (!func)
		{
			func = ofc_hashmap_find(
				ofc_sema_intrinsic__op_map, &decl_name.string);
		}

		if (!func)
		{
			func = ofc_hashmap_find(
				ofc_sema_intrinsic__func_map, &decl_name.string);
		}

		if (!func)
		{
			ofc_sparse_ref_error(decl_name,
				"Function '%.*s' can't be INTRINSIC",
				decl_name.string.size, decl_name.string.base);
			return false;
		}

		if (!ofc_sema_intrinsic_is_specific(func))
		{
			ofc_sparse_ref_warning(decl_name,
				"Generic function '%.*s' shouldn't be INTRINSIC",
				decl_name.string.size, decl_name.string.base);
		}

		ofc_sema_decl_t* decl
			= ofc_sema_scope_decl_find_create(
				scope, decl_name, true);
		if (!decl) return false;

		if (decl->type_implicit
			&& ofc_sema_intrinsic_is_specific(func))
		{
			ofc_sema_intrinsic__param_t p;
			if (func->op->return_type == IP_SAME)
			{
				p = ofc_sema_intrinsic__param[func->op->arg_type];
			}
			else
			{
				p = ofc_sema_intrinsic__param[func->op->return_type];
			}

			const ofc_sema_type_t* rtype
				= ofc_sema_type_create_function(
					ofc_sema_type_create_primitive(p.type, p.kind));
			if (!rtype) return false;

			if (!ofc_sema_decl_type_set(
				decl, rtype, decl_name))
				return false;
		}
		else if (decl->type_implicit)
		{
			/* Generic functions can't have a type */
			if (!ofc_sema_decl_type_set(
				decl, NULL, decl_name))
				return false;
		}

		if (decl->is_external)
		{
			ofc_sparse_ref_error(decl_name,
				"Specifying '%.*s' as EXTERNAL and INTRINSIC",
				decl_name.string.size, decl_name.string.base);
			return false;
		}
		if (decl->is_intrinsic)
		{
			ofc_sparse_ref_error(decl_name,
				"Re-declaring '%.*s' as INTRINSIC",
				decl_name.string.size, decl_name.string.base);
			return false;
		}

		decl->is_intrinsic = true;
		decl->intrinsic    = func;
	}

	return true;
}

static const ofc_sema_type_t* ofc_sema_intrinsic__param_type(
	const ofc_sema_dummy_arg_t* dummy_arg,
	ofc_sema_intrinsic__param_e param,
	bool* valid)
{
	if (!dummy_arg || (param >= IP_COUNT))
		return NULL;

	const ofc_sema_type_t* type
		= ofc_sema_dummy_arg_type(dummy_arg);

	ofc_sema_intrinsic__param_t p
		= ofc_sema_intrinsic__param[param];
	switch (p.type_type)
	{
		case OFC_SEMA_INTRINSIC__TYPE_NORMAL:
			break;

		case OFC_SEMA_INTRINSIC__TYPE_ANY:
			if (valid) *valid = true;
			return type;

		case OFC_SEMA_INTRINSIC__TYPE_SCALAR:
			if (valid) *valid = ofc_sema_type_is_scalar(type);
			return ofc_sema_type_scalar(type);

		/* Arguments can't be SAME or CALLBACK. */
		default:
			return NULL;
	}

	if (!type) return NULL;

	if ((p.type == OFC_SEMA_TYPE_CHARACTER)
		&& (type->type == OFC_SEMA_TYPE_CHARACTER)
		&& ((p.kind == OFC_SEMA_KIND_NONE) || (type->kind == p.kind))
		&& ((p.size == 0) || type->len_var || (p.size == type->len)))
	{
		if (valid) *valid = true;
		return type;
	}

	if ((p.type == type->type)
		&& (p.kind == 0))
	{
		switch (p.type)
		{
			case OFC_SEMA_TYPE_LOGICAL:
			case OFC_SEMA_TYPE_BYTE:
			case OFC_SEMA_TYPE_INTEGER:
			case OFC_SEMA_TYPE_REAL:
			case OFC_SEMA_TYPE_COMPLEX:
				if (valid) *valid = true;
				return type;
			default:
				break;
		}
	}

	const ofc_sema_type_t* ctype = NULL;
	switch (p.type)
	{
		case OFC_SEMA_TYPE_CHARACTER:
			ctype = ofc_sema_type_create_character(
				(p.kind != OFC_SEMA_KIND_NONE
					? p.kind : OFC_SEMA_KIND_DEFAULT),
				p.size, (p.size == 0));
			break;

		case OFC_SEMA_TYPE_LOGICAL:
		case OFC_SEMA_TYPE_BYTE:
		case OFC_SEMA_TYPE_INTEGER:
		case OFC_SEMA_TYPE_REAL:
		case OFC_SEMA_TYPE_COMPLEX:
			ctype = ofc_sema_type_create_primitive(
				p.type, (p.kind != OFC_SEMA_KIND_NONE
					? p.kind : OFC_SEMA_KIND_DEFAULT));
			break;

		default:
			return NULL;
	}

	/* TODO - INTRINSIC - Promote when KIND isn't specified and types differ. */

	if (valid) *valid = ofc_sema_type_compatible(ctype, type);
	return ctype;
}

static ofc_sema_dummy_arg_t* ofc_sema_intrinsic__param_cast(
	const ofc_sema_dummy_arg_t* dummy_arg,
	ofc_sema_intrinsic__param_e param,
	bool* valid)
{
	if (!dummy_arg || (param >= IP_COUNT))
		return NULL;

	const ofc_sema_type_t* type
		= ofc_sema_dummy_arg_type(dummy_arg);
	if (!type) return NULL;

	const ofc_sema_type_t* ctype
		= ofc_sema_intrinsic__param_type(
			dummy_arg, param, valid);
	if (!ctype) return NULL;

	ofc_sema_dummy_arg_t* copy
		= ofc_sema_dummy_arg_copy(dummy_arg);
	if (ofc_sema_type_compatible(type, ctype))
		return copy;

	ofc_sema_dummy_arg_t* cast
		= ofc_sema_dummy_arg_cast(copy, ctype);
	ofc_sema_dummy_arg_delete(copy);
	if (!cast) return NULL;

	return cast;
}

static ofc_sema_dummy_arg_list_t* ofc_sema_intrinsic_cast__op(
	ofc_sparse_ref_t src,
	const ofc_sema_intrinsic_t* intrinsic,
	const ofc_sema_dummy_arg_list_t* args)
{
	if (!intrinsic || !args
		|| (intrinsic->type != OFC_SEMA_INTRINSIC_OP)
		|| !intrinsic->op)
		return NULL;

	if (args->count < intrinsic->op->arg_min)
	{
		ofc_sparse_ref_error(src,
			"Not enough arguments for intrinsic function.");
		return NULL;
	}
	if ((intrinsic->op->arg_max != 0)
		&& (args->count > intrinsic->op->arg_max))
	{
		ofc_sparse_ref_error(src,
			"Too many arguments for intrinsic function.");
		return NULL;
	}

	const ofc_sema_type_t* ctype = NULL;
	unsigned i;
	for (i = 0; i < args->count; i++)
	{
		bool valid = true;
		const ofc_sema_type_t* atype
			= ofc_sema_intrinsic__param_type(
				args->dummy_arg[i], intrinsic->op->arg_type, &valid);

		if (!valid)
		{
			ofc_sparse_ref_warning(args->dummy_arg[i]->src,
				"Incorrect argument type for intrinsic.");
		}

		ctype = (ctype
			? ofc_sema_type_promote(ctype, atype)
			: atype);
		if (!ctype) return NULL;
	}

	ofc_sema_dummy_arg_list_t* cargs
		= ofc_sema_dummy_arg_list_create();
	if (!cargs) return NULL;

	for (i = 0; i < args->count; i++)
	{
		ofc_sema_dummy_arg_t* carg
			= ofc_sema_dummy_arg_copy(args->dummy_arg[i]);
		if (!carg)
		{
			ofc_sema_dummy_arg_list_delete(cargs);
			return NULL;
		}

		const ofc_sema_type_t* atype
			= ofc_sema_dummy_arg_type(carg);

		if (!ofc_sema_type_compatible(
			atype, ctype))
		{
			ofc_sema_dummy_arg_t* cast
				= ofc_sema_dummy_arg_cast(carg, ctype);
			if (!cast)
			{
				ofc_sparse_ref_error(carg->src,
					"Incompatible argument type for intrinsic.");
				ofc_sema_dummy_arg_delete(carg);
				ofc_sema_dummy_arg_list_delete(cargs);
				return NULL;
			}
			ofc_sema_dummy_arg_delete(carg);
			carg = cast;
		}

		if (!ofc_sema_dummy_arg_list_add(cargs, carg))
		{
			ofc_sema_dummy_arg_delete(carg);
			ofc_sema_dummy_arg_list_delete(cargs);
			return NULL;
		}
	}

	return cargs;
}

static ofc_sema_dummy_arg_list_t* ofc_sema_intrinsic_cast__func(
	ofc_sparse_ref_t src,
	const ofc_sema_intrinsic_t* intrinsic,
	const ofc_sema_dummy_arg_list_t* args)
{
	if (!intrinsic || !args)
		return NULL;

	if ((intrinsic->type != OFC_SEMA_INTRINSIC_FUNC)
		&& (intrinsic->type != OFC_SEMA_INTRINSIC_SUBR))
		return NULL;

	if (!intrinsic->func)
		return NULL;

	if (args->count < intrinsic->func->arg_min)
	{
		ofc_sparse_ref_error(src,
			"Not enough arguments for intrinsic function.");
		return NULL;
	}
	if ((intrinsic->func->arg_max != 0)
		&& (args->count > intrinsic->func->arg_max))
	{
		ofc_sparse_ref_error(src,
			"Too many arguments for intrinsic function.");
		return NULL;
	}

	/* TODO - Handle array arguments. */

	ofc_sema_dummy_arg_list_t* cargs
		= ofc_sema_dummy_arg_list_create();
	if (!cargs) return NULL;

	unsigned i;
	for (i = 0; i < args->count; i++)
	{
		bool valid = true;
		ofc_sema_dummy_arg_t* carg
			= ofc_sema_intrinsic__param_cast(
				args->dummy_arg[i], intrinsic->func->arg_type[i], &valid);
		if (!carg)
		{
			ofc_sema_dummy_arg_list_delete(cargs);
			return NULL;
		}

		if (!valid)
		{
			ofc_sparse_ref_warning(carg->src,
				"Incorrect argument type for intrinsic.");
		}

		if (!ofc_sema_dummy_arg_list_add(cargs, carg))
		{
			ofc_sema_dummy_arg_delete(carg);
			ofc_sema_dummy_arg_list_delete(cargs);
			return NULL;
		}
	}

	return cargs;
}

ofc_sema_dummy_arg_list_t* ofc_sema_intrinsic_cast(
	ofc_sparse_ref_t src,
	const ofc_sema_intrinsic_t* intrinsic,
	const ofc_sema_dummy_arg_list_t* args)
{
	switch (intrinsic->type)
	{
		case OFC_SEMA_INTRINSIC_OP:
			return ofc_sema_intrinsic_cast__op(src, intrinsic, args);

		case OFC_SEMA_INTRINSIC_FUNC:
		case OFC_SEMA_INTRINSIC_SUBR:
			return ofc_sema_intrinsic_cast__func(src, intrinsic, args);

		default:
			break;
	}

	return NULL;
}

ofc_sema_typeval_t* ofc_sema_intrinsic_constant(
	const ofc_sema_intrinsic_t* intrinsic,
	const ofc_sema_dummy_arg_list_t* args)
{
	if (!intrinsic)
		return NULL;

	if (intrinsic->type == OFC_SEMA_INTRINSIC_OP)
	{
		if (!intrinsic->op
			|| !intrinsic->op->constant)
			return NULL;
		return intrinsic->op->constant(
			intrinsic, args);

	}
	else if (intrinsic->type == OFC_SEMA_INTRINSIC_FUNC)
	{
		if (!intrinsic->func
			|| !intrinsic->func->constant)
			return NULL;
		return intrinsic->func->constant(
			intrinsic, args);
	}

	return NULL;
}


const ofc_sema_type_t* ofc_sema_intrinsic_type(
	const ofc_sema_intrinsic_t* intrinsic,
	const ofc_sema_dummy_arg_list_t* args)
{
	switch (intrinsic->type)
	{
		case OFC_SEMA_INTRINSIC_OP:
			if (!intrinsic->op) return NULL;
			return ofc_sema_intrinsic__param_rtype(
				intrinsic->func->return_type, args, NULL);

		case OFC_SEMA_INTRINSIC_FUNC:
			if (!intrinsic->func) return NULL;
			return ofc_sema_intrinsic__param_rtype(
				intrinsic->func->return_type, args,
				intrinsic->func->return_type_callback);

		/* Intrinsic subroutines have no return type*/
		case OFC_SEMA_INTRINSIC_SUBR:
			return NULL;

		default:
			break;
	}

	return NULL;
}

const ofc_sema_intrinsic_t* ofc_sema_intrinsic_cast_func(
	const ofc_sema_type_t* type)
{
	if (!type)
		return NULL;

	unsigned i;
	for (i = 0; ofc_sema_intrinsic__op_list[i].name; i++)
	{
		if (ofc_sema_intrinsic__op_list[i].constant
			!= ofc_sema_intrinsic_op__constant_cast)
			continue;

		const ofc_sema_type_t* ctype
			= ofc_sema_intrinsic__param_rtype(
				ofc_sema_intrinsic__op_list[i].return_type, NULL, NULL);

		if (ofc_sema_type_compare(ctype, type))
		{
			/* TODO - INTRINSIC - Find a neater way to do this lookup. */
			const ofc_sema_intrinsic_t* intrinsic
				= ofc_sema_intrinsic(ofc_str_ref_from_strz(
					ofc_sema_intrinsic__op_list[i].name), false);
			if (intrinsic) return intrinsic;
		}
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
