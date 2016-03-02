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

#include "ofc/parse.h"


unsigned ofc_parse_decl_attr(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_decl_attr_t* attr)
{
	if (!attr)
		return 0;

	static const ofc_parse_keyword_e kw[] =
	{
		OFC_PARSE_KEYWORD_STATIC,
		OFC_PARSE_KEYWORD_AUTOMATIC,
		OFC_PARSE_KEYWORD_VOLATILE,
		OFC_PARSE_KEYWORD_INTRINSIC,
		OFC_PARSE_KEYWORD_EXTERNAL,
	};

	bool* param[] =
	{
		&attr->is_static,
		&attr->is_automatic,
		&attr->is_volatile,
		&attr->is_intrinsic,
		&attr->is_external,
		NULL
	};

	unsigned i;
	for (i = 0; param[i]; i++)
	{
		unsigned l = ofc_parse_keyword(
			src, ptr, debug, kw[i]);
		if (l != 0)
		{
			if (!(param[i]))
			{
				ofc_parse_debug_warning(debug,
					ofc_sparse_ref(src, ptr, l),
					"Duplicate definition of %s decl attribute",
					ofc_parse_keyword_name(kw[i]));
			}

			*(param[i]) = true;
			return l;
		}
	}

	return 0;
}

bool ofc_parse_decl_attr_print(
	ofc_colstr_t* cs, const ofc_parse_decl_attr_t* attr)
{
	if (!cs || !attr)
		return false;

	bool first = true;

	static const ofc_parse_keyword_e kw[] =
	{
		OFC_PARSE_KEYWORD_STATIC,
		OFC_PARSE_KEYWORD_AUTOMATIC,
		OFC_PARSE_KEYWORD_VOLATILE,
		OFC_PARSE_KEYWORD_INTRINSIC,
		OFC_PARSE_KEYWORD_EXTERNAL,
	};

	bool param[] =
	{
		attr->is_static,
		attr->is_automatic,
		attr->is_volatile,
		attr->is_intrinsic,
		attr->is_external,
	};

	unsigned i;
	for (i = 0; i < 5; i++)
	{
		if (!param[i])
			continue;

		if (!first && !ofc_colstr_writef(cs, " "))
			return false;

		if (!ofc_colstr_atomic_writef(cs,
			"%s", ofc_parse_keyword_name(kw[i])))
			return false;

		first = false;
	}

	return true;
}



static ofc_parse_decl_t* ofc_parse__decl(
	const ofc_sparse_t* src, const char* ptr,
	bool is_f90,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	ofc_parse_decl_t* decl
		= (ofc_parse_decl_t*)malloc(
			sizeof(ofc_parse_decl_t));
	if (!decl) return NULL;

	decl->record = OFC_SPARSE_REF_EMPTY;

	unsigned i;
	decl->lhs = ofc_parse_lhs_star_len(
		src, ptr, debug, &i);
	if (!decl->lhs)
	{
		free(decl);
		return NULL;
	}

	decl->init_expr  = NULL;
	decl->init_clist = NULL;

	if (is_f90 && (ptr[i] == '='))
	{
		unsigned l;
		decl->init_expr = ofc_parse_expr(
			src, &ptr[i + 1], debug, &l);
		if (decl->init_expr) i += (l + 1);
	}
	else if (!is_f90 && (ptr[i] == '/'))
	{
		unsigned l;
		decl->init_clist = ofc_parse_expr_clist(
			src, &ptr[i], debug, &l);
		if (decl->init_clist) i += l;
	}

	if (len) *len = i;
	return decl;
}

ofc_parse_decl_t* ofc_parse_decl(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	return ofc_parse__decl(
		src, ptr, false, debug, len);
}

ofc_parse_decl_t* ofc_parse_decl_f90(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	return ofc_parse__decl(
		src, ptr, true, debug, len);
}

ofc_parse_decl_t* ofc_parse_decl_record(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	ofc_parse_decl_t* decl
		= (ofc_parse_decl_t*)malloc(
			sizeof(ofc_parse_decl_t));
	if (!decl) return NULL;

	decl->record = OFC_SPARSE_REF_EMPTY;

	unsigned i = 0, l;
	if (ptr[i] == '/')
	{
		i++;

		l = ofc_parse_ident(src, &ptr[i],
			debug, &decl->record);
		i += l;
		if ((l == 0)
			|| (ptr[i++] != '/'))
		{
			free(decl);
			return NULL;
		}
	}

	decl->lhs = ofc_parse_lhs(
		src, &ptr[i], debug, &l);
	if (!decl->lhs)
	{
		free(decl);
		return NULL;
	}
	i += l;

	decl->init_expr  = NULL;
	decl->init_clist = NULL;

	if (len) *len = i;
	return decl;
}


void ofc_parse_decl_delete(
	ofc_parse_decl_t* decl)
{
	if (!decl)
		return;

	ofc_parse_expr_delete(decl->init_expr);
	ofc_parse_expr_list_delete(decl->init_clist);

	ofc_parse_lhs_delete(decl->lhs);
	free(decl);
}

bool ofc_parse_decl_print(
	ofc_colstr_t* cs, const ofc_parse_decl_t* decl)
{
	if (!decl) return false;

	if (!ofc_sparse_ref_empty(decl->record))
	{
		if (!ofc_colstr_atomic_writef(cs, "/")
			|| !ofc_sparse_ref_print(cs, decl->record)
			|| !ofc_colstr_atomic_writef(cs, "/")
			|| !ofc_colstr_atomic_writef(cs, " "))
			return false;
	}

	if (!ofc_parse_lhs_print(
		cs, decl->lhs, true))
		return false;

	if (decl->init_expr)
	{
		if (!ofc_colstr_atomic_writef(cs, " = ")
			|| !ofc_parse_expr_print(cs, decl->init_expr))
			return false;
	}
	else if (decl->init_clist)
	{
		if (!ofc_colstr_atomic_writef(cs, "/")
			|| !ofc_parse_expr_list_print(
				cs, decl->init_clist)
			|| !ofc_colstr_atomic_writef(cs, "/"))
			return false;
	}

	return true;
}


ofc_parse_decl_list_t* ofc_parse_decl_list(
	const ofc_sparse_t* src, const char* ptr,
	bool is_f90,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	ofc_parse_decl_list_t* list
		= (ofc_parse_decl_list_t*)malloc(
			sizeof(ofc_parse_decl_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->decl = NULL;

	unsigned i = ofc_parse_list(
		src, ptr, debug, ',',
		&list->count, (void***)&list->decl,
		(void*)(is_f90 ? ofc_parse_decl_f90 : ofc_parse_decl),
		(void*)ofc_parse_decl_delete);
	if (i == 0)
	{
		free(list);
		return NULL;
	}

	if (len) *len = i;
	return list;
}

ofc_parse_decl_list_t* ofc_parse_decl_list_record(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	ofc_parse_decl_list_t* list
		= (ofc_parse_decl_list_t*)malloc(
			sizeof(ofc_parse_decl_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->decl = NULL;

	unsigned i = ofc_parse_list(
		src, ptr, debug, ',',
		&list->count, (void***)&list->decl,
		(void*)ofc_parse_decl_record,
		(void*)ofc_parse_decl_delete);
	if (i == 0)
	{
		free(list);
		return NULL;
	}

	if (ofc_sparse_ref_empty(
		list->decl[0]->record))
	{
		ofc_parse_decl_list_delete(list);
		return NULL;
	}

	if (len) *len = i;
	return list;
}

void ofc_parse_decl_list_delete(
	ofc_parse_decl_list_t* list)
{
	if (!list)
		return;

	ofc_parse_list_delete(
		list->count, (void**)list->decl,
		(void*)ofc_parse_decl_delete);
	free(list);
}

bool ofc_parse_decl_list_print(
	ofc_colstr_t* cs, const ofc_parse_decl_list_t* list)
{
	return ofc_parse_list_print(
		cs, list->count, (const void**)list->decl,
		(void*)ofc_parse_decl_print);
}
