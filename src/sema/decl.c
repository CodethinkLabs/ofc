#include <ofc/sema.h>


ofc_sema_decl_t* ofc_sema_decl_implicit_name(
	const ofc_sema_implicit_t* implicit,
	ofc_str_ref_t* name)
{
	return NULL;
}

ofc_sema_decl_t* ofc_sema_decl_implicit_lhs(
	const ofc_sema_implicit_t* implicit,
	const ofc_parse_lhs_t* lhs)
{
	return NULL;
}

ofc_sema_decl_t* ofc_sema_decl(
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt || (stmt->type != OFC_PARSE_STMT_DECL))
		return NULL;

	if (!stmt->decl.type
		|| !stmt->decl.decl)
		return NULL;

	ofc_sema_decl_t* decl
		= (ofc_sema_decl_t*)malloc(
			sizeof(ofc_sema_decl_t));
	if (!decl) return NULL;

	decl->type = NULL;
	decl->name = OFC_STR_REF_EMPTY;
	decl->init = NULL;

	decl->equiv = NULL;

	/* TODO - Initialize these to types attributes. */
	decl->is_static    = false;
	decl->is_volatile  = false;
	decl->is_automatic = false;
	decl->is_target    = false;

	return decl;
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
			? ofc_str_ref_hash_ci
			: ofc_str_ref_hash),
		(void*)(list->ignore_case
			? ofc_str_ref_equal_ci
			: ofc_str_ref_equal),
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
