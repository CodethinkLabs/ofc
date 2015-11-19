#include <ofc/sema.h>

typedef enum
{
	IT_ANY = 0, /* Any type */
	IT_SAME,    /* Same as argument */
	IT_SCALAR,  /* Any scalar type */
	IT_LOGICAL,
	IT_INTEGER,
	IT_REAL,
	IT_COMPLEX,

	IT_DEF_LOGICAL,
	IT_DEF_INTEGER,
	IT_DEF_REAL,
	IT_DEF_DOUBLE,
	IT_DEF_COMPLEX,

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
	{ "INT"  , 1, 1, IT_DEF_INTEGER, IT_ANY         },
	{ "IFIX" , 1, 1, IT_DEF_INTEGER, IT_DEF_REAL    },
	{ "IDINT", 1, 1, IT_DEF_INTEGER, IT_DEF_DOUBLE  },
	{ "REAL" , 1, 1, IT_DEF_REAL   , IT_ANY         },
	{ "FLOAT", 1, 1, IT_DEF_REAL   , IT_DEF_INTEGER },
	{ "SNGL" , 1, 1, IT_DEF_REAL   , IT_DEF_DOUBLE  },
	{ "DBLE" , 1, 1, IT_DEF_DOUBLE , IT_ANY         },
	{ "CMPLX", 1, 2, IT_DEF_COMPLEX, IT_ANY         },

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


struct ofc_sema_intrinsic_s
{
	ofc_str_ref_t name;

	bool is_op;

	union
	{
		const ofc_sema_intrinsic_op_t* op;
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
	intrinsic->is_op = true;
	intrinsic->op = op;

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

static ofc_hashmap_t* ofc_sema_intrinsic__map = NULL;

static void ofc_sema_intrinsic__term(void)
{
	ofc_hashmap_delete(ofc_sema_intrinsic__map);
}

static bool ofc_sema_intrinsic__init(void)
{
	if (ofc_sema_intrinsic__map)
		return true;

	/* TODO - Set case sensitivity based on lang_opts? */
	ofc_sema_intrinsic__map = ofc_hashmap_create(
		(void*)ofc_str_ref_ptr_hash_ci,
		(void*)ofc_str_ref_ptr_equal_ci,
		(void*)ofc_sema_intrinsic__key,
		(void*)ofc_sema_intrinsic__delete);
	if (!ofc_sema_intrinsic__map)
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
				ofc_sema_intrinsic__map);
			ofc_sema_intrinsic__map = NULL;
			return false;
		}

		if (!ofc_hashmap_add(
			ofc_sema_intrinsic__map,
			intrinsic))
		{
			ofc_sema_intrinsic__delete(intrinsic);
			ofc_hashmap_delete(
				ofc_sema_intrinsic__map);
			ofc_sema_intrinsic__map = NULL;
			return false;
		}
	}

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

	return ofc_hashmap_find(
		ofc_sema_intrinsic__map, &name);
}

ofc_sema_expr_list_t* ofc_sema_intrinsic_cast(
	const ofc_sema_scope_t* scope,
	ofc_str_ref_t src,
	const ofc_sema_intrinsic_t* intrinsic,
	ofc_sema_expr_list_t* args)
{
	if (!intrinsic || !args
		|| (args->count == 0))
	{
		ofc_sema_expr_list_delete(args);
		return NULL;
	}

	if (!intrinsic->is_op
		|| !intrinsic->op)
		return NULL;

	if (args->count < intrinsic->op->arg_min)
	{
		ofc_sema_scope_error(scope, src,
			"Not enough arguments for intrinsic function.");
		ofc_sema_expr_list_delete(args);
		return NULL;
	}
	if ((intrinsic->op->arg_max != 0)
		&& (args->count > intrinsic->op->arg_max))
	{
		ofc_sema_scope_error(scope, src,
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

		case IT_DEF_DOUBLE:
			stype = ofc_sema_type_real_default();
			break;

		case IT_DEF_COMPLEX:
			stype = ofc_sema_type_complex_default();
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
			case IT_DEF_DOUBLE:
			case IT_DEF_COMPLEX:
				valid = ofc_sema_type_compare(atype, stype);
				break;

			default:
				break;
		}

		if (!valid)
		{
			ofc_sema_scope_warning(scope, args->expr[i]->src,
				"Incorrect argument type for intrinsic.");
		}

		if (!stype)
			ptype = (ptype ? ofc_sema_type_promote(ptype, atype) : atype);
	}

	for (i = 0; i < args->count; i++)
	{
		const ofc_sema_type_t* atype = at[i];

		if (!ofc_sema_type_compare(atype, ptype))
		{
			ofc_sema_expr_t* cast
				= ofc_sema_expr_cast(
					scope, args->expr[i], ptype);
			if (!cast)
			{
				ofc_sema_scope_error(scope, args->expr[i]->src,
					"Incompatible argument type for intrinsic.");
				ofc_sema_expr_list_delete(args);
				return NULL;
			}

			args->expr[i] = cast;
		}
	}

	return args;
}

const ofc_sema_type_t* ofc_sema_intrinsic_type(
	const ofc_sema_intrinsic_t* intrinsic,
	ofc_sema_expr_list_t* args)
{
	if (!intrinsic)
		return NULL;

	if (!intrinsic->is_op
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

		case IT_DEF_DOUBLE:
			return ofc_sema_type_double_default();

		case IT_DEF_COMPLEX:
			return ofc_sema_type_complex_default();

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
					OFC_SEMA_TYPE_REAL, atype->kind,
					false, false, false);

			default:
				return NULL;
		}
	}

	/* TODO - Handle other conversions if used. */
	return NULL;
}
