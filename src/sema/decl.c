#include <ofc/sema.h>


static ofc_sema_decl_t* ofc_sema_decl__type_name(
	const ofc_sema_type_t* type,
	ofc_str_ref_t name)
{
	if (!type)
		return NULL;

	ofc_sema_decl_t* decl
		= (ofc_sema_decl_t*)malloc(
			sizeof(ofc_sema_decl_t));
	if (!decl) return NULL;

	decl->type = type;
	decl->name = name;

	decl->init = NULL;

	decl->equiv = NULL;

	decl->is_static    = type->is_static;
	decl->is_volatile  = type->is_volatile;
	decl->is_automatic = type->is_automatic;
	decl->is_target    = false;

	return decl;
}

static ofc_sema_decl_t* ofc_sema_decl_implicit__name(
	const ofc_sema_implicit_t* implicit,
	ofc_str_ref_t name)
{
	if (!implicit)
		return NULL;

	if (ofc_str_ref_empty(name))
		return NULL;

	const ofc_sema_type_t* type
		= ofc_sema_implicit_get(
			implicit, name.base[0]);

	return ofc_sema_decl__type_name(
		type, name);
}

ofc_sema_decl_t* ofc_sema_decl_implicit_lhs(
	const ofc_sema_scope_t* scope,
	const ofc_parse_lhs_t* lhs)
{
	if (!scope || !lhs)
		return NULL;

	/* Can't implicitly declare an array or struct. */
	if (lhs->type
		!= OFC_PARSE_LHS_VARIABLE)
		return NULL;

	return ofc_sema_decl_implicit__name(
		scope->implicit, lhs->variable);
}

static ofc_sema_decl_t* ofc_sema_decl__decl(
	const ofc_sema_scope_t* scope,
	const ofc_sema_type_t*  type,
	const ofc_parse_decl_t* decl)
{
	if (!decl || !type)
		return NULL;

	/* TODO - Support ARRAY declarations. */
	if (decl->lhs->type
		!= OFC_PARSE_LHS_VARIABLE)
		return NULL;

	ofc_sema_decl_t* sdecl
		= ofc_sema_decl__type_name(
			type, decl->lhs->variable);

	if (decl->init_expr)
	{
		ofc_sema_expr_t* init_expr
			= ofc_sema_expr(scope, decl->init_expr);
		if (!init_expr)
		{
			ofc_sema_decl_delete(sdecl);
			return NULL;
		}

		const ofc_sema_typeval_t* ctv
			= ofc_sema_expr_constant(init_expr);

		sdecl->init = ofc_sema_typeval_copy(ctv);
		ofc_sema_expr_delete(init_expr);
		if (!sdecl->init)
		{
			ofc_sema_decl_delete(sdecl);
			return NULL;
		}
	}
	else if (decl->init_clist)
	{
		/* TODO - CList initializer resolution. */
		ofc_sema_decl_delete(sdecl);
		return NULL;
	}

	return sdecl;
}

bool ofc_sema_decl(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt || !scope || !scope->decl
		|| !stmt->decl.type || !stmt->decl.decl
		|| (stmt->type != OFC_PARSE_STMT_DECL))
		return false;

	const ofc_sema_type_t* type = ofc_sema_type(
		scope, stmt->decl.type);
	if (!type) return false;

	unsigned count = stmt->decl.decl->count;
	if (count == 0) return false;

	ofc_sema_decl_t* decl[count];

	unsigned i;
	for (i = 0; i < count; i++)
	{
		decl[i] = ofc_sema_decl__decl(
			scope, type, stmt->decl.decl->decl[i]);

		if (decl[i] && ofc_sema_decl_list_find(
			scope->decl, decl[i]->name))
		{
			/* TODO - Allow redeclaration as long as it matches original. */
			ofc_sema_decl_delete(decl[i]);
			decl[i] = NULL;
		}

		if (!decl[i])
		{
			unsigned j;
			for (j = 0; j < i; j++)
				ofc_sema_decl_delete(decl[j]);
			return false;
		}
	}

	for (i = 0; i < count; i++)
	{
		if (!ofc_sema_decl_list_add(
			scope->decl, decl[i]))
		{
			/* This should never happen. */
			abort();
			return false;
		}
	}

	return true;
}

void ofc_sema_decl_delete(
	ofc_sema_decl_t* decl)
{
	if (!decl)
		return;

	if (decl->equiv)
	{
		free(decl->equiv->decl);
		decl->equiv->decl = NULL;

		if (decl->equiv->count > 0)
			decl->equiv->count -= 1;

		if (decl->equiv->count == 0)
			free(decl->equiv);
	}

	free(decl->init);
	free(decl);
}


unsigned ofc_sema_decl_size(
	const ofc_sema_decl_t* decl)
{
	if (!decl)
		return 0;
	return ofc_sema_type_size(
		decl->type);
}

const ofc_sema_type_t* ofc_sema_decl_type(
	const ofc_sema_decl_t* decl)
{
	return (decl ? decl->type : NULL);
}


bool ofc_sema_decl_equiv(
	ofc_sema_decl_t* a,
	ofc_sema_decl_t* b)
{
	if (!a || !b)
		return false;

	if (a->equiv == b->equiv)
		return true;

	if (a->equiv && b->equiv)
	{
		ofc_sema_equiv_t* equiv = a->equiv;
		ofc_sema_equiv_t* bequiv = b->equiv;

		ofc_sema_decl_t** ndecl
			= (ofc_sema_decl_t**)realloc(equiv->decl,
				sizeof(ofc_sema_decl_t*) * (equiv->count + bequiv->count));
		if (!ndecl) return false;
		equiv->decl = ndecl;

		unsigned i;
		for (i = 0; i < bequiv->count; i++)
		{
			equiv->decl[equiv->count++] = bequiv->decl[i];
			bequiv->decl[i]->equiv = equiv;
		}

		free(bequiv);
	}
	else if (!a->equiv && !b->equiv)
	{
		ofc_sema_equiv_t* equiv
			= (ofc_sema_equiv_t*)malloc(
				sizeof(ofc_sema_equiv_t));
		if (!equiv) return false;

		equiv->count = 2;
		equiv->decl = (ofc_sema_decl_t**)malloc(
			sizeof(ofc_sema_decl_t*) * equiv->count);
		if (!equiv->decl)
		{
			free(equiv);
			return false;
		}

		equiv->decl[0] = a;
		equiv->decl[1] = b;
		a->equiv = equiv;
		b->equiv = equiv;
	}
	else if (!b->equiv)
	{
		ofc_sema_equiv_t* equiv = a->equiv;

		ofc_sema_decl_t** ndecl
			= (ofc_sema_decl_t**)realloc(equiv->decl,
				sizeof(ofc_sema_decl_t*) * (equiv->count + 1));
		if (!ndecl) return false;
		equiv->decl = ndecl;

		equiv->decl[equiv->count++] = b;
		b->equiv = equiv;
	}
	else
	{
		ofc_sema_equiv_t* equiv = b->equiv;

		ofc_sema_decl_t** ndecl
			= (ofc_sema_decl_t**)realloc(equiv->decl,
				sizeof(ofc_sema_decl_t*) * (equiv->count + 1));
		if (!ndecl) return false;
		equiv->decl = ndecl;

		unsigned i;
		for (i = 0; i < equiv->count; i++)
			equiv->decl[equiv->count - i] = equiv->decl[equiv->count - (i + 1)];
		equiv->decl[0] = a;
		equiv->count += 1;

		a->equiv = equiv;
	}

	return true;
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

ofc_sema_decl_list_t* ofc_sema_decl_list_create(bool ignore_case)
{
	ofc_sema_decl_list_t* list
		= (ofc_sema_decl_list_t*)malloc(
			sizeof(ofc_sema_decl_list_t));
	if (!list) return NULL;

	list->ignore_case = ignore_case;

	list->count = 0;
	list->decl = NULL;

	list->map = ofc_hashmap_create(
		(void*)(list->ignore_case
			? ofc_str_ref_ptr_hash_ci
			: ofc_str_ref_ptr_hash),
		(void*)(list->ignore_case
			? ofc_str_ref_ptr_equal_ci
			: ofc_str_ref_ptr_equal),
		(void*)ofc_sema_decl__key, NULL);
	if (!list->map)
	{
		free(list);
		return NULL;
	}

	return list;
}

void ofc_sema_decl_list_delete(
	ofc_sema_decl_list_t* list)
{
	if (!list)
		return;

	ofc_hashmap_delete(list->map);

	unsigned i;
	for (i = 0; i < list->count; i++)
		ofc_sema_decl_delete(list->decl[i]);
	free(list->decl);

	free(list);
}

bool ofc_sema_decl_list_add(
	ofc_sema_decl_list_t* list,
	ofc_sema_decl_t* decl)
{
	if (!list || !decl)
		return false;

	/* Check for duplicate definitions. */
	if (ofc_sema_decl_list_find(
		list, decl->name))
	{
		/* TODO - Ignore duplicate so long as it matches. */
		return false;
	}

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

ofc_sema_decl_t* ofc_sema_decl_list_find(
	ofc_sema_decl_list_t* list, ofc_str_ref_t name)
{
	if (!list)
		return NULL;

	return ofc_hashmap_find_modify(
		list->map, &name);
}

const ofc_hashmap_t* ofc_decl_list_map(
	const ofc_sema_decl_list_t* list)
{
	return (list ? list->map : NULL);
}
