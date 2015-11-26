#include <ofc/sema.h>


ofc_sema_decl_t* ofc_sema_decl_create(
	const ofc_sema_type_t* type,
	ofc_str_ref_t name)
{
	ofc_sema_decl_t* decl
		= (ofc_sema_decl_t*)malloc(
			sizeof(ofc_sema_decl_t));
	if (!decl) return NULL;

	decl->type = type;
	decl->name = name;
	decl->func = NULL;

	if (ofc_sema_type_is_composite(type))
	{
		decl->init_array = NULL;
	}
	else
	{
		decl->init = NULL;
	}

	decl->equiv = NULL;

	decl->is_static    = false;
	decl->is_volatile  = false;
	decl->is_automatic = false;
	decl->is_target    = false;

	decl->used = false;
	return decl;
}

static ofc_sema_decl_t* ofc_sema_decl__spec(
	const ofc_sema_scope_t* scope,
	ofc_str_ref_t           name,
	const ofc_sema_spec_t*  spec,
	const ofc_sema_array_t* array,
	bool                    is_procedure,
	bool                    is_function)
{
	if (!spec)
		return NULL;

	if (is_procedure)
	{
		if (spec->is_static)
		{
			ofc_sema_scope_error(scope, name,
				"Procedures cannot be STATIC");
			return NULL;
		}

		if (spec->is_automatic)
		{
			ofc_sema_scope_error(scope, name,
				"Procedures cannot be AUTOMATIC");
			return NULL;
		}

		if (spec->is_volatile)
		{
			ofc_sema_scope_error(scope, name,
				"Procedures cannot be VOLATILE");
			return NULL;
		}

		if (spec->is_intrinsic
			&& spec->is_external)
		{
			ofc_sema_scope_error(scope, name,
				"A procedure cannot be INTRINSIC and EXTERNAL");
			return NULL;
		}

		if (!is_function)
		{
			if (!spec->type_implicit
				|| (spec->kind > 0)
				|| (spec->len > 0)
				|| spec->len_var)
			{
				ofc_sema_scope_error(scope, name,
					"A SUBROUTINE cannot have a specified type");
				return NULL;
			}
		}
	}
	else
	{
		if (spec->is_intrinsic)
		{
			ofc_sema_scope_error(scope, name,
				"Only procedures may be INTRINSIC");
			return NULL;
		}

		if (spec->is_external)
		{
			ofc_sema_scope_error(scope, name,
				"Only procedures may be EXTERNAL");
			return NULL;
		}

		if (spec->is_static
			&& spec->is_automatic)
		{
			ofc_sema_scope_error(scope, name,
				"Declaration cannot be AUTOMATIC and STATIC");
			return NULL;
		}
	}

	if (spec->array)
	{
		if (array)
		{
			if (!ofc_sema_array_compare(
				array, spec->array))
			{
				ofc_sema_scope_error(scope, name,
					"Conflicting array dimensions in declaration");
				return NULL;
			}

			ofc_sema_scope_warning(scope, name,
				"Repetition of array dimensions in declaration");
		}

		array = NULL;
	}

	const ofc_sema_type_t* type
		= ofc_sema_type_spec(spec);
	if (!type) return NULL;

	if (array)
	{
		type = ofc_sema_type_create_array(type, array);
		if (!type) return NULL;
	}

	if (is_procedure)
	{
		type = (is_function
			? ofc_sema_type_create_function(type)
			: ofc_sema_type_subroutine());
		if (!type) return NULL;
	}

	ofc_sema_decl_t* decl
		= ofc_sema_decl_create(type, name);
	if (!decl) return NULL;

	decl->is_static    = spec->is_static;
	decl->is_automatic = spec->is_automatic;
	decl->is_volatile  = spec->is_volatile;
	decl->is_intrinsic = spec->is_intrinsic;
	decl->is_external  = spec->is_external;


	if (spec->common)
	{
		/* TODO - Do this after the decl is in a list. */
		if (!ofc_sema_common_define(
			spec->common, spec->common_offset, decl))
		{
			ofc_sema_decl_delete(decl);
			return NULL;
		}

		decl->is_static |= spec->common->save;
	}

	return decl;
}

ofc_sema_decl_t* ofc_sema_decl_spec(
	const ofc_sema_scope_t* scope,
	ofc_str_ref_t           name,
	const ofc_sema_spec_t*  spec,
	const ofc_sema_array_t* array)
{
	return ofc_sema_decl__spec(
		scope, name, spec, array, false, false);
}

ofc_sema_decl_t* ofc_sema_decl_implicit(
	const ofc_sema_scope_t* scope,
	ofc_str_ref_t           name,
	const ofc_sema_array_t* array)
{
	ofc_sema_spec_t* spec
		= ofc_sema_scope_spec_find_final(scope, name);
	if (!spec) return NULL;

	ofc_sema_decl_t* decl;
	if (spec->is_external
		|| spec->is_intrinsic)
	{
		if (spec->type_implicit)
		{
			decl = ofc_sema_decl_subroutine(
				scope, name);
		}
		else
		{
			decl = ofc_sema_decl_function(
				scope, name, spec);
		}
	}
	else
	{
		decl = ofc_sema_decl_spec(
			scope, name, spec, array);
	}

	ofc_sema_spec_delete(spec);
	return decl;
}

ofc_sema_decl_t* ofc_sema_decl_implicit_lhs(
	ofc_sema_scope_t*       scope,
	const ofc_parse_lhs_t*  lhs)
{
	if (!scope || !lhs)
		return NULL;

	ofc_sema_array_t* array = NULL;
	ofc_str_ref_t base_name;
	if (!ofc_parse_lhs_base_name(
		*lhs, &base_name))
		return NULL;

    switch (lhs->type)
	{
		case OFC_PARSE_LHS_VARIABLE:
			break;
		case OFC_PARSE_LHS_ARRAY:
			array = ofc_sema_array(
				scope, lhs->array.index);
			if (!array) return NULL;
			break;
		default:
			break;
	}

	ofc_sema_decl_t* decl
		= ofc_sema_decl_implicit(
			scope, base_name, array);
	ofc_sema_array_delete(array);
	return decl;
}


ofc_sema_decl_t* ofc_sema_decl_function(
	const ofc_sema_scope_t* scope,
	ofc_str_ref_t           name,
	const ofc_sema_spec_t*  spec)
{
	return ofc_sema_decl__spec(
		scope, name, spec, NULL, true, true);
}

ofc_sema_decl_t* ofc_sema_decl_implicit_function(
	const ofc_sema_scope_t* scope,
	ofc_str_ref_t           name)
{
	ofc_sema_spec_t* spec
		= ofc_sema_scope_spec_find_final(
			scope, name);
	ofc_sema_decl_t* decl
		= ofc_sema_decl_function(
			scope, name, spec);
	ofc_sema_spec_delete(spec);
	return decl;
}

ofc_sema_decl_t* ofc_sema_decl_subroutine(
	const ofc_sema_scope_t* scope,
	ofc_str_ref_t           name)
{
	ofc_sema_spec_t* spec
		= ofc_sema_scope_spec_find_final(
			scope, name);
	ofc_sema_decl_t* decl = ofc_sema_decl__spec(
		scope, name, spec, NULL, true, false);
	ofc_sema_spec_delete(spec);
	return decl;
}


static bool ofc_sema_decl__decl(
	ofc_sema_scope_t*       scope,
	ofc_sema_spec_t         spec,
	const ofc_parse_decl_t* pdecl)
{
	if (!pdecl || !pdecl->lhs)
		return false;

	bool is_decl = (pdecl->init_clist || pdecl->init_expr);

	const ofc_parse_lhs_t* lhs = pdecl->lhs;
	if (lhs->type == OFC_PARSE_LHS_STAR_LEN)
	{
		if (lhs->star_len.var)
		{
			if (spec.len > 0)
			{
				ofc_sema_scope_warning(scope, lhs->src,
					"Overriding specified star length in %s list",
					(is_decl ? "decl" : "specifier"));
			}

			spec.len     = 0;
			spec.len_var = true;
		}
		else
		{
			ofc_sema_expr_t* expr
				= ofc_sema_expr(scope, lhs->star_len.len);
			if (!expr) return false;

			unsigned star_len;
			bool resolved = ofc_sema_expr_resolve_uint(expr, &star_len);
			ofc_sema_expr_delete(expr);

			if (!resolved)
			{
				ofc_sema_scope_error(scope, lhs->src,
					"Star length must be a positive whole integer");
				return false;
			}

			if (spec.len_var
				|| ((spec.len > 0) && (spec.len != star_len)))
			{
				ofc_sema_scope_warning(scope, lhs->src,
					"Overriding specified star length in %s list",
					(is_decl ? "decl" : "specifier"));
			}

			spec.len     = star_len;
			spec.len_var = false;
		}

		lhs = lhs->parent;
		if (!lhs) return false;
	}

	if (lhs->type == OFC_PARSE_LHS_ARRAY)
	{
		if (spec.array)
		{
			/* This shouldn't ever happen since arrays
			   can't be specified on the LHS of a declaration. */
			return false;
		}

		spec.array = ofc_sema_array(
			scope, lhs->array.index);
		if (!spec.array) return false;

		lhs = lhs->parent;
		if (!lhs)
		{
			ofc_sema_array_delete(spec.array);
			return false;
		}
	}
	else if (spec.array)
	{
		spec.array = ofc_sema_array_copy(spec.array);
		if (!spec.array) return false;
	}

	if (lhs->type != OFC_PARSE_LHS_VARIABLE)
	{
		ofc_sema_array_delete(spec.array);
		return false;
	}

	if (!ofc_parse_lhs_base_name(
		*lhs, &spec.name))
	{
		ofc_sema_array_delete(spec.array);
		return false;
	}

	ofc_sema_decl_t* decl
		= ofc_sema_scope_decl_find_modify(
			scope, spec.name, true);
	bool exist = (decl != NULL);

	if (is_decl)
	{
		if (exist)
		{
			/* TODO - Handle redeclarations which match original. */

			ofc_sema_scope_error(scope, lhs->src,
				"Redeclaration of declared variable");
			ofc_sema_array_delete(spec.array);
			return false;
		}

		decl = ofc_sema_decl__spec(
			scope, spec.name, &spec,
			NULL, false, false);
		ofc_sema_array_delete(spec.array);
		if (!decl) return false;

		if (pdecl->init_expr)
		{
			ofc_sema_expr_t* init_expr
				= ofc_sema_expr(scope, pdecl->init_expr);
			if (!init_expr)
			{
				ofc_sema_decl_delete(decl);
				return false;
			}

			bool initialized = ofc_sema_decl_init(
				scope, decl, init_expr);
			ofc_sema_expr_delete(init_expr);

			if (!initialized)
			{
				ofc_sema_decl_delete(decl);
				return false;
			}
		}
		else if (pdecl->init_clist)
		{
			/* TODO - CList initializer resolution. */
			ofc_sema_decl_delete(decl);
			return false;
		}

		if (!ofc_sema_decl_list_add(
			scope->decl, decl))
		{
			ofc_sema_decl_delete(decl);
			return false;
		}
	}
	else
	{
		if (exist)
		{
			/* TODO - Handle specifications which are compatible. */

			ofc_sema_scope_error(scope, lhs->src,
				"Specification of declared variable");
			ofc_sema_array_delete(spec.array);
			return false;
		}

		ofc_sema_spec_t* nspec
			= ofc_sema_scope_spec_modify(
				scope, spec.name);
		if (!nspec)
		{
			ofc_sema_array_delete(spec.array);
			return false;
		}

		/* Overlay the spec on the existing one. */
		if (!spec.type_implicit)
		{
			if (!nspec->type_implicit
				&& (nspec->type != spec.type))
			{
				ofc_sema_array_delete(spec.array);
				return false;
			}

			nspec->type_implicit = spec.type_implicit;
			nspec->type          = spec.type;

			if (spec.kind != 0)
				nspec->kind = spec.kind;
		}

		if (spec.array)
		{
			if (nspec->array)
			{
				bool conflict = !ofc_sema_array_compare(
					nspec->array, spec.array);
				ofc_sema_array_delete(spec.array);
				if (conflict) return false;
			}
			else
			{
				nspec->array = spec.array;
			}
		}

		if ((spec.len != 0)
			|| spec.len_var)
		{
			nspec->len     = spec.len;
			nspec->len_var = spec.len_var;
		}

		nspec->is_static    |= spec.is_static;
		nspec->is_automatic |= spec.is_automatic;
		nspec->is_volatile  |= spec.is_volatile;
		nspec->is_intrinsic |= spec.is_intrinsic;
		nspec->is_external  |= spec.is_external;
	}

	return true;
}

bool ofc_sema_decl(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt || !scope || !scope->decl
		|| !stmt->decl.type || !stmt->decl.decl
		|| (stmt->type != OFC_PARSE_STMT_DECL))
		return false;

	ofc_sema_spec_t* spec = ofc_sema_spec(
		scope, stmt->decl.type);
	if (!spec) return false;

	unsigned count = stmt->decl.decl->count;
	if (count == 0) return false;

	unsigned i;
	for (i = 0; i < count; i++)
	{
		if (!ofc_sema_decl__decl(
			scope, *spec, stmt->decl.decl->decl[i]))
		{
			ofc_sema_spec_delete(spec);
			return false;
		}
	}

	ofc_sema_spec_delete(spec);
	return true;
}

void ofc_sema_decl_delete(
	ofc_sema_decl_t* decl)
{
	if (!decl)
		return;

	if (ofc_sema_decl_is_composite(decl)
		&& decl->init_array)
	{
		unsigned count
			= ofc_sema_decl_elem_count(decl);

		unsigned i;
		for (i = 0; i < count; i++)
			ofc_sema_typeval_delete(decl->init_array[i]);

		free(decl->init_array);
	}
	else
	{
		ofc_sema_typeval_delete(decl->init);
	}

	ofc_sema_scope_delete(decl->func);
	ofc_sema_equiv_delete(decl->equiv);
	free(decl);
}


bool ofc_sema_decl_init(
	const ofc_sema_scope_t* scope,
	ofc_sema_decl_t* decl,
	const ofc_sema_expr_t* init)
{
	if (!decl || !init || !decl->type
		|| ofc_sema_decl_is_procedure(decl))
		return false;

	if (decl->used)
	{
		ofc_sema_scope_error(scope, init->src,
			"Can't initialize declaration after use");
		return false;
	}

    const ofc_sema_typeval_t* ctv
		= ofc_sema_expr_constant(init);
	if (!ctv)
	{
		ofc_sema_scope_error(scope, init->src,
			"Initializer element not constant");
		return false;
	}

	if (ofc_sema_decl_is_composite(decl))
	{
		/* TODO - Support F90 "(/ 0, 1 /)" array initializers. */
		ofc_sema_scope_error(scope, init->src,
			"Can't initialize non-scalar declaration with expression");
		return false;
	}

	ofc_sema_typeval_t* tv
		= ofc_sema_typeval_cast(
			scope, ctv, decl->type);
	if (!tv) return false;

	if (decl->init)
	{
		bool redecl = ofc_sema_typeval_compare(
			tv, decl->init);
		ofc_sema_typeval_delete(tv);

		if (redecl)
		{
			ofc_sema_scope_warning(scope, init->src,
				"Duplicate initialization");
		}
		else
		{
			/* TODO - Convert to assignment. */
			ofc_sema_scope_error(scope, init->src,
				"Conflicting initializaters");
			return false;
		}
	}

	decl->init = tv;
	return true;
}

bool ofc_sema_decl_init_offset(
	const ofc_sema_scope_t* scope,
	ofc_sema_decl_t* decl,
	unsigned offset,
	const ofc_sema_expr_t* init)
{
	if (!decl || !init || !decl->type
		|| ofc_sema_decl_is_procedure(decl))
		return false;

	if (decl->used)
	{
		ofc_sema_scope_error(scope, init->src,
			"Can't initialize declaration after use");
		return false;
	}

	if (!decl->type)
		return NULL;

	if (!ofc_sema_type_is_array(decl->type))
	{
		if (offset == 0)
			return ofc_sema_decl_init(
				scope, decl, init);
		return false;
	}

	if (decl->init_array)
	{
		ofc_sema_scope_warning(scope, init->src,
			"Initializing array in multiple statements");
	}

	unsigned elem_count
		= ofc_sema_decl_elem_count(decl);
	if (offset >= elem_count)
	{
		ofc_sema_scope_warning(scope, init->src,
			"Initializer destination out-of-bounds");
		return false;
	}

	if (!decl->init_array)
	{
		decl->init_array = (ofc_sema_typeval_t**)malloc(
			sizeof(ofc_sema_typeval_t*) * elem_count);
		if (!decl->init_array) return false;

		unsigned i;
		for (i = 0; i < elem_count; i++)
			decl->init_array[i] = NULL;
	}

	const ofc_sema_typeval_t* ctv
		= ofc_sema_expr_constant(init);
	if (!ctv)
	{
		ofc_sema_scope_error(scope, init->src,
			"Array initializer element not constant.");
		return false;
	}

	ofc_sema_typeval_t* tv = ofc_sema_typeval_cast(
		scope, ctv, ofc_sema_type_base(decl->type));
	if (!tv) return false;

	if (decl->init_array[offset])
	{
		bool equal = ofc_sema_typeval_compare(
			decl->init_array[offset], tv);
		ofc_sema_typeval_delete(tv);

		if (!equal)
		{
			ofc_sema_scope_error(scope, init->src,
				"Re-initialization of array element"
				" with different value");
			return false;
		}

		ofc_sema_scope_warning(scope, init->src,
			"Re-initialization of array element");
	}
	else
	{
		decl->init_array[offset] = tv;
	}

	return true;
}

bool ofc_sema_decl_init_array(
	const ofc_sema_scope_t* scope,
	ofc_sema_decl_t* decl,
	const ofc_sema_array_t* array,
	unsigned count,
	const ofc_sema_expr_t** init)
{
	if (!decl || !init
		|| ofc_sema_decl_is_procedure(decl))
		return false;

	if (count == 0)
		return true;

	if (decl->used)
	{
		ofc_sema_scope_error(scope, init[0]->src,
			"Can't initialize declaration after use");
		return false;
	}

	if (!decl->type)
		return false;

	if (!ofc_sema_type_is_array(decl->type))
	{
		if (!array && (count == 1))
			return ofc_sema_decl_init(
				scope, decl, init[0]);
		return false;
	}

	if (decl->init_array)
	{
		ofc_sema_scope_warning(scope, init[0]->src,
			"Initializing arrays in multiple statements.");
	}

	unsigned elem_count
		= ofc_sema_type_elem_count(decl->type);
	if (elem_count == 0) return true;

	if (!decl->init_array)
	{
		decl->init_array = (ofc_sema_typeval_t**)malloc(
			sizeof(ofc_sema_typeval_t*) * elem_count);
		if (!decl->init_array) return false;

		unsigned i;
		for (i = 0; i < elem_count; i++)
			decl->init_array[i] = NULL;
	}

	if (!array)
	{
		if (count > elem_count)
		{
			ofc_sema_scope_warning(scope, init[0]->src,
				"Array initializer too large, truncating.");
			count = elem_count;
		}

		unsigned i;
		for (i = 0; i < count; i++)
		{
			const ofc_sema_typeval_t* ctv
				= ofc_sema_expr_constant(init[i]);
			if (!ctv)
			{
				ofc_sema_scope_error(scope, init[i]->src,
					"Array initializer element not constant.");
				return false;
			}

			ofc_sema_typeval_t* tv = ofc_sema_typeval_cast(
				scope, ctv, ofc_sema_type_base(decl->type));
			if (!tv) return false;

			if (decl->init_array[i])
			{
				bool equal = ofc_sema_typeval_compare(
					decl->init_array[i], tv);
				ofc_sema_typeval_delete(tv);

				if (!equal)
				{
					ofc_sema_scope_error(scope, init[i]->src,
						"Re-initialization of array element"
						" with different value");
					return false;
				}

				ofc_sema_scope_warning(scope, init[i]->src,
					"Re-initialization of array element");
			}
			else
			{
				decl->init_array[i] = tv;
			}
		}
	}
	else
	{
		/* TODO - Initialize array slice. */
		return false;
	}

	return true;
}

bool ofc_sema_decl_init_func(
	ofc_sema_decl_t* decl,
	ofc_sema_scope_t* func)
{
	if (!ofc_sema_decl_is_procedure(decl))
		return false;

	if (decl->func)
		return (decl->func == func);

	decl->func = func;
	return true;
}


unsigned ofc_sema_decl_size(
	const ofc_sema_decl_t* decl)
{
	if (!decl)
		return 0;
	return ofc_sema_type_size(
		decl->type);
}

unsigned ofc_sema_decl_elem_count(
	const ofc_sema_decl_t* decl)
{
	if (!decl)
		return 0;
	return ofc_sema_type_elem_count(
		decl->type);
}

bool ofc_sema_decl_is_array(
	const ofc_sema_decl_t* decl)
{
	return (decl && ofc_sema_type_is_array(decl->type));
}

bool ofc_sema_decl_is_composite(
	const ofc_sema_decl_t* decl)
{
	if (!decl)
		return false;
	return ofc_sema_type_is_composite(decl->type);
}


bool ofc_sema_decl_is_subroutine(
	const ofc_sema_decl_t* decl)
{
	return (decl && ofc_sema_type_is_subroutine(decl->type));
}

bool ofc_sema_decl_is_function(
	const ofc_sema_decl_t* decl)
{
	return (decl && ofc_sema_type_is_function(decl->type));
}

bool ofc_sema_decl_is_procedure(
	const ofc_sema_decl_t* decl)
{
	return (ofc_sema_decl_is_subroutine(decl)
		|| ofc_sema_decl_is_function(decl));
}


bool ofc_sema_decl_has_initializer(
	const ofc_sema_decl_t* decl)
{
	if (!decl)
		return false;
	return (ofc_sema_decl_is_composite(decl)
		? (decl->init_array != NULL)
		: (decl->init != NULL));
}


const ofc_sema_type_t* ofc_sema_decl_type(
	const ofc_sema_decl_t* decl)
{
	return (decl ? decl->type : NULL);
}

const ofc_sema_type_t* ofc_sema_decl_base_type(
	const ofc_sema_decl_t* decl)
{
	if (!decl)
		return NULL;

	return ofc_sema_type_base(decl->type);
}



static const ofc_str_ref_t* ofc_sema_decl__key(
	const ofc_sema_decl_t* decl)
{
	return (decl ? &decl->name : NULL);
}

bool ofc_sema_decl_list__remap(
	ofc_sema_decl_list_t* list)
{
    if (!list)
		return false;

	if (list->map)
		ofc_hashmap_delete(list->map);



	return (list->map != NULL);
}

ofc_sema_decl_list_t* ofc_sema_decl_list__create(
	bool case_sensitive, bool is_ref)
{
	ofc_sema_decl_list_t* list
		= (ofc_sema_decl_list_t*)malloc(
			sizeof(ofc_sema_decl_list_t));
	if (!list) return NULL;

	list->case_sensitive = case_sensitive;

	list->count  = 0;
	list->decl   = NULL;
	list->is_ref = is_ref;

	list->map = ofc_hashmap_create(
		(void*)(list->case_sensitive
			? ofc_str_ref_ptr_hash
			: ofc_str_ref_ptr_hash_ci),
		(void*)(list->case_sensitive
			? ofc_str_ref_ptr_equal
			: ofc_str_ref_ptr_equal_ci),
		(void*)ofc_sema_decl__key, NULL);
	if (!list->map)
	{
		free(list);
		return NULL;
	}

	return list;
}

ofc_sema_decl_list_t* ofc_sema_decl_list_create(
	bool case_sensitive)
{
	return ofc_sema_decl_list__create(
		case_sensitive, false);
}

ofc_sema_decl_list_t* ofc_sema_decl_list_create_ref(
	bool case_sensitive)
{
	return ofc_sema_decl_list__create(
		case_sensitive, true);
}

void ofc_sema_decl_list_delete(
	ofc_sema_decl_list_t* list)
{
	if (!list)
		return;

	ofc_hashmap_delete(list->map);

	if (!list->is_ref)
	{
		unsigned i;
		for (i = 0; i < list->count; i++)
			ofc_sema_decl_delete(list->decl[i]);
	}

	free(list->decl);

	free(list);
}

bool ofc_sema_decl_list_add(
	ofc_sema_decl_list_t* list,
	ofc_sema_decl_t* decl)
{
	if (!list || !decl
		|| list->is_ref)
		return false;

	/* Check for duplicate definitions. */
	if (ofc_sema_decl_list_find(
		list, decl->name))
		return false;

    ofc_sema_decl_t** ndecl
		= (ofc_sema_decl_t**)realloc(list->decl,
			(sizeof(ofc_sema_decl_t*) * (list->count + 1)));
	if (!ndecl) return false;
	list->decl = ndecl;

	if (!ofc_hashmap_add(
		list->map, decl))
		return false;

	list->decl[list->count++] = decl;
	return true;
}

bool ofc_sema_decl_list_add_ref(
	ofc_sema_decl_list_t* list,
	const ofc_sema_decl_t* decl)
{
	if (!list || !decl
		|| !list->is_ref)
		return false;

	/* Check for duplicate definitions. */
	if (ofc_sema_decl_list_find(
		list, decl->name))
		return false;

    const ofc_sema_decl_t** ndecl
		= (const ofc_sema_decl_t**)realloc(list->decl_ref,
			(sizeof(const ofc_sema_decl_t*) * (list->count + 1)));
	if (!ndecl) return false;
	list->decl_ref = ndecl;

	if (!ofc_hashmap_add(
		list->map, (void*)decl))
		return false;

	list->decl_ref[list->count++] = decl;
	return true;
}

const ofc_sema_decl_t* ofc_sema_decl_list_find(
	const ofc_sema_decl_list_t* list, ofc_str_ref_t name)
{
	if (!list)
		return NULL;

	return ofc_hashmap_find(
		list->map, &name);
}

ofc_sema_decl_t* ofc_sema_decl_list_find_modify(
	ofc_sema_decl_list_t* list, ofc_str_ref_t name)
{
	if (!list)
		return NULL;

	return ofc_hashmap_find_modify(
		list->map, &name);
}

const ofc_hashmap_t* ofc_sema_decl_list_map(
	const ofc_sema_decl_list_t* list)
{
	return (list ? list->map : NULL);
}
