#include <ofc/sema.h>


static uint8_t ofc_sema_equiv__hash(const ofc_sema_decl_t* decl)
{
	uintptr_t hash = (uintptr_t)decl;
	hash ^= (hash >> 16);
	hash ^= (hash >>  8);
	return (uint8_t)(hash & 0xFF);
}

static bool ofc_sema_equiv__equal(
	const ofc_sema_decl_t* a,
	const ofc_sema_decl_t* b)
{
	return (a == b);
}

static void* ofc_sema_equiv__key(
	const ofc_sema_lhs_t* lhs)
{
	return (lhs ? lhs->decl : NULL);
}

static ofc_sema_equiv_t* ofc_sema_equiv__create(void)
{
	ofc_sema_equiv_t* equiv
		= (ofc_sema_equiv_t*)malloc(
			sizeof(ofc_sema_equiv_t));
	if (!equiv) return NULL;

	equiv->count = 0;
	equiv->lhs = NULL;
	if (!equiv->lhs)
	{
		free(equiv);
		return NULL;
	}

	equiv->map = ofc_hashmap_create(
		(void*)ofc_sema_equiv__hash,
		(void*)ofc_sema_equiv__equal,
		(void*)ofc_sema_equiv__key, NULL);
	if (!equiv->map)
	{
		free(equiv);
		return NULL;
	}

	equiv->refcnt = 0;
	return equiv;
}

static bool ofc_sema_equiv__add(
	ofc_sema_equiv_t* equiv,
	ofc_sema_lhs_t* lhs,
	ofc_sema_decl_t* decl)
{
	if (!equiv)
		return false;

	if (decl->func)
		return false;

	const ofc_sema_lhs_t* exist
		= ofc_hashmap_find(
			equiv->map, decl);
	if (exist)
		return ofc_sema_lhs_compare(
			lhs, exist);

	if (!ofc_sema_lhs_reference(lhs))
		return false;

	ofc_sema_lhs_t** nlhs
		= (ofc_sema_lhs_t**)realloc(equiv->lhs,
			(sizeof(ofc_sema_lhs_t*) * (equiv->count + 1)));
	if (!nlhs)
	{
		ofc_sema_lhs_delete(lhs);
		return false;
	}
	equiv->lhs = nlhs;

	if (!ofc_hashmap_add(
		equiv->map, lhs))
	{
		ofc_sema_lhs_delete(lhs);
		return false;
	}

	equiv->lhs[equiv->count++] = lhs;
	equiv->refcnt++;
	decl->equiv = equiv;
	return true;
}

static bool ofc_sema_equiv__create_pair(
	ofc_sema_lhs_t* a,
	ofc_sema_decl_t* ad,
	ofc_sema_lhs_t* b,
	ofc_sema_decl_t* bd)
{
	if (!ad || !bd)
		return false;

	ofc_sema_equiv_t* equiv
		= ofc_sema_equiv__create();
	if (!equiv) return false;

	if (!ofc_sema_equiv__add(equiv, a, ad)
		|| !ofc_sema_equiv__add(equiv, b, bd))
	{
		ad->equiv = NULL;
		bd->equiv = NULL;
		equiv->refcnt = 0;
		ofc_sema_equiv_delete(equiv);
		return false;
	}

	equiv->refcnt = (equiv->count - 1);
	return true;
}

static bool ofc_sema_equiv__merge(
	ofc_sema_equiv_t* a,
	ofc_sema_equiv_t* b)
{
	if (!a || !b)
		return false;

	if ((a->count == 0)
		|| (b->count == 0))
		return true;

	ofc_sema_equiv_t* equiv
		= ofc_sema_equiv__create();
	if (!equiv) return false;

	unsigned i;
	for (i = 0; i < a->count; i++)
	{
		ofc_sema_decl_t* decl
			= ofc_sema_lhs_decl(a->lhs[i]);
		if (!decl)
		{
			equiv->refcnt = 0;
			ofc_sema_equiv_delete(equiv);
			return false;
		}

		if (!ofc_sema_equiv__add(
			equiv, a->lhs[i], decl))
		{
			equiv->refcnt = 0;
			ofc_sema_equiv_delete(equiv);
			return false;
		}

		/* Don't change the equiv until complete. */
		decl->equiv = a;
	}
	for (i = 0; i < b->count; i++)
	{
		ofc_sema_decl_t* decl
			= ofc_sema_lhs_decl(b->lhs[i]);
		if (!decl)
		{
			equiv->refcnt = 0;
			ofc_sema_equiv_delete(equiv);
			return false;
		}

		if (!ofc_sema_equiv__add(
			equiv, b->lhs[i], decl))
		{
			equiv->refcnt = 0;
			ofc_sema_equiv_delete(equiv);
			return false;
		}

		/* Don't change the equiv until complete. */
		decl->equiv = b;
	}

	/* Point the decl's to the merged equiv. */
	for (i = 0; i < equiv->count; i++)
	{
		ofc_sema_decl_t* decl
			= ofc_sema_lhs_decl(equiv->lhs[i]);
		if (decl) decl->equiv = equiv;
	}
	equiv->refcnt = (equiv->count - 1);

	a->refcnt = 0;
	ofc_sema_equiv_delete(a);
	b->refcnt = 0;
	ofc_sema_equiv_delete(b);

	return true;
}


bool ofc_sema_equiv(
	ofc_sema_lhs_t* a,
	ofc_sema_lhs_t* b)
{
	/* TODO - Print better errors for this stuff. */

	if (!a || !b)
		return false;

	if (ofc_sema_lhs_compare(a, b))
		return true;

	ofc_sema_decl_t* ad
		= ofc_sema_lhs_decl(a);
	ofc_sema_decl_t* bd
		= ofc_sema_lhs_decl(b);

	if (!ad || !bd)
		return false;

	if (ad->equiv == bd->equiv)
		return true;

	if (ad == bd)
		return false;

	if (!ad->equiv && !bd->equiv)
	{
		return ofc_sema_equiv__create_pair(
			a, ad, b, bd);
	}
	else if (ad->equiv && bd->equiv)
	{
		return ofc_sema_equiv__merge(
			ad->equiv, bd->equiv);
	}
	else if (bd->equiv)
	{
		return ofc_sema_equiv__add(
			bd->equiv, a, ad);
	}
	else
	{
		return ofc_sema_equiv__add(
			ad->equiv, b, bd);
	}

	return true;
}

void ofc_sema_equiv_delete(
	ofc_sema_equiv_t* equiv)
{
	if (!equiv)
		return;

	if (equiv->refcnt > 0)
	{
		equiv->refcnt--;
		return;
	}

	ofc_hashmap_delete(equiv->map);

	unsigned i;
	for (i = 0; i < equiv->count; i++)
		ofc_sema_lhs_delete(equiv->lhs[i]);
	free(equiv->lhs);

	free(equiv);
}
