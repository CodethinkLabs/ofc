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


static const ofc_str_ref_t* ofc_structure__member_name(
	const ofc_sema_structure_member_t* member)
{
	if (!member)
		return NULL;
	return (member->is_structure
		? &member->structure->name.string
		: &member->decl->name.string);
}


static ofc_sema_structure_t* ofc_sema__structure(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt)
		return NULL;

	const ofc_lang_opts_t* opts
		= ofc_sparse_lang_opts(stmt->src.sparse);
	if (!opts) return NULL;

	ofc_sema_structure_e type;
	switch (stmt->type)
	{
		case OFC_PARSE_STMT_STRUCTURE:
			type = OFC_SEMA_STRUCTURE_VAX_STRUCTURE;
			break;

		case OFC_PARSE_STMT_UNION:
			type = OFC_SEMA_STRUCTURE_VAX_UNION;
			break;

		case OFC_PARSE_STMT_MAP:
			type = OFC_SEMA_STRUCTURE_VAX_MAP;
			break;

		case OFC_PARSE_STMT_TYPE:
			type = OFC_SEMA_STRUCTURE_F90_TYPE;
			break;

		default:
			return NULL;
	}

	ofc_sema_structure_t* structure
		= (ofc_sema_structure_t*)malloc(
			sizeof(ofc_sema_structure_t));
	if (!structure) return NULL;

	structure->map = ofc_hashmap_create(
		(void*)(global_opts.case_sensitive
			? ofc_str_ref_ptr_hash
			: ofc_str_ref_ptr_hash_ci),
		(void*)(global_opts.case_sensitive
			? ofc_str_ref_ptr_equal
			: ofc_str_ref_ptr_equal_ci),
		(void*)ofc_structure__member_name,
		NULL);
	if (!structure->map)
	{
		free(structure);
		return NULL;
	}

	structure->name = stmt->structure.name;
	structure->type = type;

	const ofc_sema_implicit_t* simplicit
		= ofc_sema_scope_implicit(scope);

	structure->implicit
		= ofc_sema_implicit_copy(simplicit);
	if (simplicit && !structure->implicit)
	{
		free(structure);
		return NULL;
	}

	structure->count  = 0;
	structure->member = NULL;

	structure->refcnt = 0;

	if (stmt->structure.block)
	{
		unsigned i;
		for (i = 0; i < stmt->structure.block->count; i++)
		{
			/* TODO - Validate sub-structures. */

			ofc_sema_structure_t* smember
				= ofc_sema__structure(scope,
					stmt->structure.block->stmt[i]);
			if (smember)
			{
				if (!ofc_sema_structure_member_add_structure(
					structure, smember))
				{
					ofc_sema_structure_delete(smember);
					ofc_sema_structure_delete(structure);
					return NULL;
				}
			}
			else if (stmt->structure.block->stmt[i]
				&& (stmt->structure.block->stmt[i]->type
					== OFC_PARSE_STMT_SEQUENCE))
			{
				if (structure->type
					== OFC_SEMA_STRUCTURE_F90_TYPE_SEQUENCE)
				{
					ofc_sparse_ref_warning(
						stmt->structure.block->stmt[i]->src,
						"Redundant SEQUENCE statement in TYPE");
				}
				else if (structure->type
					!= OFC_SEMA_STRUCTURE_F90_TYPE)
				{
					ofc_sparse_ref_warning(
						stmt->structure.block->stmt[i]->src,
						"SEQUENCE statement invalid"
						" for VAX structures, ignoring");
				}
				else
				{
					structure->type
						= OFC_SEMA_STRUCTURE_F90_TYPE_SEQUENCE;
				}
			}
			else if (!ofc_sema_decl_member(scope, structure,
				stmt->structure.block->stmt[i]))
			{
				ofc_sema_structure_delete(structure);
				return NULL;
			}
		}

		for (i = 0; i < structure->count; i++)
		{
			if (!structure->member[i]
				|| structure->member[i]->is_structure)
				continue;

			if (!ofc_sema_decl_type_finalize(
				structure->member[i]->decl))
			{
				ofc_sema_structure_delete(structure);
				return NULL;
			}
		}
	}

	return structure;
}

ofc_sema_structure_t* ofc_sema_structure(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt)
		return NULL;

	ofc_sema_structure_t* structure
		= ofc_sema__structure(scope, stmt);
	if (!structure) return NULL;

	ofc_sema_scope__check_namespace_collision(
		scope, "Structure", stmt->structure.name);

	bool added = false;
	switch (stmt->type)
	{
		case OFC_PARSE_STMT_STRUCTURE:
			added = ofc_sema_scope_structure_add(scope, structure);
			break;
		case OFC_PARSE_STMT_TYPE:
			added = ofc_sema_scope_derived_type_add(scope, structure);
			break;
		default:
			break;
	}
	if (!added)
	{
		ofc_sema_structure_delete(structure);
		return NULL;
	}

	return structure;
}

bool ofc_sema_structure_reference(
	ofc_sema_structure_t* structure)
{
	if (!structure)
		return false;

	if ((structure->refcnt + 1) == 0)
		return false;

	structure->refcnt++;
	return true;
}

static void ofc_sema_structure__member_delete(
	ofc_sema_structure_member_t* member)
{
	if (!member)
		return;

	if (member->is_structure)
		ofc_sema_structure_delete(
			member->structure);
	else
		ofc_sema_decl_delete(member->decl);

	free(member);
}

void ofc_sema_structure_delete(
	ofc_sema_structure_t* structure)
{
	if (!structure)
		return;

	if (structure->refcnt > 0)
	{
		structure->refcnt--;
		return;
	}

	ofc_sema_implicit_delete(
		structure->implicit);

	ofc_hashmap_delete(structure->map);

	unsigned i;
	for (i = 0; i < structure->count; i++)
	{
		ofc_sema_structure__member_delete(
			structure->member[i]);
	}
	free(structure->member);

	free(structure);
}


static bool ofc_sema_structure__member_anon(
	ofc_sema_structure_member_t* member)
{
	if (!member || !member->is_structure)
		return false;
	return ofc_sparse_ref_empty(
		member->structure->name);
}

static bool ofc_sema_structure__member_add(
	ofc_sema_structure_t*        structure,
	ofc_sema_structure_member_t* member)
{
	if (!structure || !member)
		return false;

	bool anon = ofc_sema_structure__member_anon(member);
	if (!anon)
	{
		const ofc_sema_structure_member_t* emember
			= ofc_hashmap_find(structure->map,
				ofc_structure__member_name(member));
		if (emember)
		{
			ofc_sparse_ref_t ref = (member->is_structure
				? member->structure->name
				: member->decl->name);
			ofc_sparse_ref_error(ref,
				"Duplicate structure member declaration");
			return false;
		}
	}

	ofc_sema_structure_member_t** nmember
		= (ofc_sema_structure_member_t**)realloc(structure->member,
			(sizeof(ofc_sema_structure_member_t*) * (structure->count + 1)));
	if (!nmember) return false;
	structure->member = nmember;

	if (!anon && !ofc_hashmap_add(
		structure->map, member))
		return false;

	structure->member[structure->count++] = member;
	return true;
}

static bool ofc_sema_structure__member_add_decl(
	ofc_sema_structure_t* structure,
	ofc_sema_decl_t*      member)
{
	if (!member)
		return false;

	ofc_sema_structure_member_t* m
		= (ofc_sema_structure_member_t*)malloc(
			sizeof(ofc_sema_structure_member_t));
	if (!m) return false;

	m->is_structure = false;
	m->decl = member;

	if (!ofc_sema_structure__member_add(
		structure, m))
	{
		free(m);
		return false;
	}

	return true;
}

ofc_sema_decl_t* ofc_sema_structure_decl_find_create(
	ofc_sema_structure_t* structure,
	ofc_sparse_ref_t name)
{
	if (!structure)
		return NULL;

	ofc_sema_decl_t* decl
		= ofc_sema_structure_member_get_decl_name(
			structure, name.string);
	if (decl) return decl;

	decl = ofc_sema_decl_create(
		structure->implicit, name);
	if (!decl) return NULL;

	if (!ofc_sema_structure__member_add_decl(
		structure, decl))
	{
		ofc_sema_decl_delete(decl);
		return NULL;
	}

	return decl;
}

bool ofc_sema_structure_member_add_structure(
	ofc_sema_structure_t* structure,
	ofc_sema_structure_t* member)
{
	if (!member)
		return false;

	ofc_sema_structure_member_t* m
		= (ofc_sema_structure_member_t*)malloc(
			sizeof(ofc_sema_structure_member_t));
	if (!m) return false;

	m->is_structure = true;
	m->structure = member;

	if (!ofc_sema_structure__member_add(
		structure, m))
	{
		free(m);
		return false;
	}

	return true;
}


bool ofc_sema_structure_member_count(
	const ofc_sema_structure_t* structure,
	unsigned* count)
{
	if (!structure)
		return false;

	unsigned i, c;
	for (i = 0, c = 0; i < structure->count; i++)
	{
		unsigned rc = 1;
		if (ofc_sema_structure__member_anon(
			structure->member[i]))
		{
			if (!ofc_sema_structure_member_count(
				structure->member[i]->structure, &rc))
				return false;
		}
		c += rc;
	}

	if (count) *count = c;
	return true;
}

ofc_sema_decl_t* ofc_sema_structure_member_get_decl_offset(
	ofc_sema_structure_t* structure,
	unsigned offset)
{
	if (!structure
		|| !structure->member)
		return NULL;

	unsigned i, o;
	for (i = 0, o = offset; i < structure->count; i++)
	{
		if (ofc_sema_structure__member_anon(
			structure->member[i]))
		{
			unsigned count;
			if (!ofc_sema_structure_member_count(
				structure->member[i]->structure, &count))
				return NULL;

			if (o < count)
				return ofc_sema_structure_member_get_decl_offset(
					structure, o);
			o -= count;
		}
		else
		{
			if (o == 0)
			{
				if (structure->member[i]->is_structure)
					return NULL;
				return structure->member[i]->decl;
			}
			o -= 1;
		}
	}

	return NULL;
}

ofc_sema_decl_t* ofc_sema_structure_member_get_decl_name(
	ofc_sema_structure_t* structure,
	ofc_str_ref_t name)
{
	if (!structure)
		return NULL;

	ofc_sema_structure_member_t* member
		= ofc_hashmap_find_modify(
			structure->map, &name);
	if (member)
	{
		if (member->is_structure)
			return NULL;
		return member->decl;
	}

	unsigned i;
	for (i = 0; i < structure->count; i++)
	{
		member = structure->member[i];
		if (!member->is_structure
			|| !ofc_sema_structure__member_anon(member))
			continue;

		ofc_sema_decl_t* decl
			= ofc_sema_structure_member_get_decl_name(
				member->structure, name);
		if (decl) return decl;
	}

	return NULL;
}

bool ofc_sema_structure_member_offset(
	const ofc_sema_structure_t* structure,
	const ofc_sema_decl_t* member,
	unsigned* offset)
{
	if (!structure || !member)
		return false;

	unsigned i, o;
	for (i = 0, o = 0; i < structure->count; i++)
	{
		if (!structure->member[i]->is_structure)
		{
            if (structure->member[i]->decl == member)
			{
				if (offset) *offset = o;
				return true;
			}
			o++;
		}
		else if (ofc_sema_structure__member_anon(
			structure->member[i]))
		{
			unsigned ao;
			if (ofc_sema_structure_member_offset(
				structure->member[i]->structure, member, &ao))
			{
				if (offset) *offset = (o + ao);
				return true;
			}

			unsigned ac;
			if (!ofc_sema_structure_member_count(
				structure->member[i]->structure, &ac))
				return false;
			o += ac;
		}
		else
		{
			o++;
		}
	}

	return false;
}


bool ofc_sema_structure_is_union(
	const ofc_sema_structure_t* structure)
{
	if (!structure)
		return false;

	switch (structure->type)
	{
		case OFC_SEMA_STRUCTURE_VAX_UNION:
			return true;

		default:
			break;
	}

	return false;
}

bool ofc_sema_structure_is_nested(
	const ofc_sema_structure_t* structure)
{
	if (!structure)
		return false;

	unsigned i;
	for (i = 0; i < structure->count; i++)
	{
		if (structure->member[i]->is_structure)
			return true;
	}

	return false;
}

bool ofc_sema_structure_is_derived_type(
	const ofc_sema_structure_t* structure)
{
	if (!structure)
		return false;

	switch (structure->type)
	{
		case OFC_SEMA_STRUCTURE_F90_TYPE:
		case OFC_SEMA_STRUCTURE_F90_TYPE_SEQUENCE:
			return true;

		default:
			break;
	}

	return false;
}


bool ofc_sema_structure_size(
	const ofc_sema_structure_t* structure,
	unsigned* size)
{
	if (!structure)
		return false;

	unsigned usize = 0;
	unsigned ssize = 0;
	unsigned i;
	for (i = 0; i < structure->count; i++)
	{
		unsigned msize;
		if (structure->member[i]->is_structure)
		{
			if (!ofc_sema_structure_size(
				structure->member[i]->structure, &msize))
				return false;
		}
		else
		{
			if (!ofc_sema_decl_size(
				structure->member[i]->decl, &msize))
				return false;
		}

		if (msize > usize)
			usize = msize;
		ssize += msize;
	}

	if (size)
	{
		if (ofc_sema_structure_is_union(structure))
			*size = usize;
		else
			*size = ssize;
	}

	return true;
}

bool ofc_sema_structure_elem_count(
	const ofc_sema_structure_t* structure,
	unsigned* count)
{
	if (!structure)
		return false;

	unsigned ucount = 0;
	unsigned scount = 0;
	unsigned i;
	for (i = 0; i < structure->count; i++)
	{
		unsigned mcount;
		if (structure->member[i]->is_structure)
		{
			if (!ofc_sema_structure_elem_count(
				structure->member[i]->structure, &mcount))
				return false;
		}
		else
		{
			if (!ofc_sema_decl_elem_count(
				structure->member[i]->decl, &mcount))
				return false;
		}

		if (mcount > ucount)
			ucount = mcount;
		scount += mcount;
	}

	if (count)
	{
		if (ofc_sema_structure_is_union(structure))
			*count = ucount;
		else
			*count = scount;
	}

	return true;
}

ofc_sema_decl_t* ofc_sema_structure_elem_get(
	ofc_sema_structure_t* structure,
	unsigned offset)
{
	if (!structure)
		return NULL;

	unsigned i, o;
	for (i = 0, o = 0; i < structure->count; i++)
	{
		if (structure->member[i]->is_structure)
		{
			unsigned sc;
			if (!ofc_sema_structure_elem_count(
				structure->member[i]->structure, &sc))
				return NULL;

			if ((offset - o) < sc)
				return ofc_sema_structure_elem_get(
					structure->member[i]->structure, (offset - o));

			o += sc;
		}
		else
		{
			if (o == offset)
				return structure->member[i]->decl;
			o++;
		}
	}

	return NULL;
}

bool ofc_sema_structure_elem_print(
	ofc_colstr_t* cs,
	const ofc_sema_structure_t* structure,
	unsigned offset)
{
	if (!structure)
		return false;

	char msym = '%';

	if (!ofc_sema_structure_is_derived_type(structure))
		msym = '.';

	if (!ofc_colstr_atomic_writef(cs, "%c", msym))
		return false;

	unsigned i, o;
	for (i = 0, o = 0; i < structure->count; i++)
	{
		if (structure->member[i]->is_structure)
		{
			unsigned sc;
			if (!ofc_sema_structure_elem_count(
				structure->member[i]->structure, &sc))
				return NULL;

			if ((offset - o) < sc)
			{
				if (!ofc_sema_structure_print_name(
					cs, structure->member[i]->structure))
					return false;
				return ofc_sema_structure_elem_print(
					cs, structure->member[i]->structure, (offset - o));
			}

			o += sc;
		}
		else
		{
			if (o == offset)
				return ofc_sema_decl_print_name(
					cs, structure->member[i]->decl);
			o++;
		}
	}

	return false;
}


bool ofc_sema_structure_print_name(
	ofc_colstr_t* cs,
	const ofc_sema_structure_t* structure)
{
	if (!structure)
		return false;
	return ofc_sparse_ref_print(
		cs, structure->name);
}

bool ofc_sema_structure_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_sema_structure_t* structure)
{
	if (!structure)
		return false;

	if (!ofc_colstr_newline(
		cs, indent, NULL))
		return false;

	const char* kwstr;
	switch (structure->type)
	{
		case OFC_SEMA_STRUCTURE_F90_TYPE:
		case OFC_SEMA_STRUCTURE_F90_TYPE_SEQUENCE:
			kwstr = "TYPE";
			break;

		case OFC_SEMA_STRUCTURE_VAX_STRUCTURE:
			kwstr = "STRUCTURE";
			break;

		case OFC_SEMA_STRUCTURE_VAX_UNION:
			kwstr = "UNION";
			break;

		case OFC_SEMA_STRUCTURE_VAX_MAP:
			kwstr = "MAP";
			break;

		default:
			return false;
	}

	if (!ofc_colstr_atomic_writef(cs, "%s", kwstr))
		return false;

	bool has_name = !ofc_sparse_ref_empty(structure->name);
	if (has_name)
	{
		if (structure->type == OFC_SEMA_STRUCTURE_VAX_STRUCTURE)
		{
			if (!ofc_colstr_atomic_writef(cs, " ")
				|| !ofc_colstr_atomic_writef(cs, "/")
				|| !ofc_sparse_ref_print(cs, structure->name)
				|| !ofc_colstr_atomic_writef(cs, "/"))
				return false;
		}
		else
		{
			if (!ofc_colstr_atomic_writef(cs, " ")
				|| !ofc_sparse_ref_print(cs, structure->name))
				return false;
		}
	}

	if (structure->type == OFC_SEMA_STRUCTURE_F90_TYPE_SEQUENCE)
	{
		if (!ofc_colstr_newline(cs, (indent + 1), NULL)
			|| !ofc_colstr_atomic_writef(cs, "SEQUENCE"))
		return false;
	}

	unsigned i;
	for (i = 0; i < structure->count; i++)
	{
		if (structure->member[i]->is_structure)
		{
			if (!ofc_sema_structure_print(cs, (indent + 1),
				structure->member[i]->structure))
				return false;
		}
		else
		{
			if (!ofc_sema_decl_print(cs, (indent + 1),
				structure->member[i]->decl))
				return false;
		}
	}

	if (!ofc_colstr_newline(cs, indent, NULL)
		|| !ofc_colstr_atomic_writef(cs, "END")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_colstr_atomic_writef(cs, "%s", kwstr))
		return false;

	if (has_name)
	{
		if (!ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_sparse_ref_print(cs, structure->name))
			return false;
	}

	return true;
}


bool ofc_sema_structure_foreach_expr(
	ofc_sema_structure_t* structure, void* param,
	bool (*func)(ofc_sema_expr_t* expr, void* param))
{
	if (!structure)
		return false;

	unsigned i;
	for (i = 0; i < structure->count; i++)
	{
		if (structure->member[i]
			&& !structure->member[i]->is_structure
			&& structure->member[i]->decl
			&& !ofc_sema_decl_foreach_expr(
				structure->member[i]->decl, param, func))
			return false;
	}

	return true;
}



static const ofc_str_ref_t* ofc_structure__name(
	const ofc_sema_structure_t* structure)
{
	if (!structure)
		return NULL;
	return &structure->name.string;
}

ofc_sema_structure_list_t* ofc_sema_structure_list_create(
	bool case_sensitive)
{
	ofc_sema_structure_list_t* list
		= (ofc_sema_structure_list_t*)malloc(
			sizeof(ofc_sema_structure_list_t));
	if (!list) return NULL;

	list->map = ofc_hashmap_create(
		(void*)(case_sensitive
			? ofc_str_ref_ptr_hash
			: ofc_str_ref_ptr_hash_ci),
		(void*)(case_sensitive
			? ofc_str_ref_ptr_equal
			: ofc_str_ref_ptr_equal_ci),
		(void*)ofc_structure__name,
		NULL);
	if (!list->map)
	{
		free(list);
		return NULL;
	}

	list->count = 0;
	list->size  = 0;
	list->structure = NULL;

	return list;
}

void ofc_sema_structure_list_delete(
	ofc_sema_structure_list_t* list)
{
	if (!list)
		return;

	ofc_hashmap_delete(list->map);

	unsigned i;
	for (i = 0; i < list->size; i++)
	{
		ofc_sema_structure_delete(
			list->structure[i]);
	}
	free(list->structure);

	free(list);
}


bool ofc_sema_structure_list_add(
	ofc_sema_structure_list_t* list,
	ofc_sema_structure_t* structure)
{
	if (!list || !structure)
		return false;

	if (ofc_sema_structure_list_find(
		list, structure->name.string))
		return false;

	unsigned slot;
	if (list->count >= list->size)
	{
		unsigned nsize = (list->count + 1);
		ofc_sema_structure_t** nstructure
			= (ofc_sema_structure_t**)realloc(list->structure,
				(sizeof(ofc_sema_structure_t*) * nsize));
		if (!nstructure) return false;
		list->structure = nstructure;

		slot = list->count;
		list->structure[slot] = NULL;
		list->size = nsize;
	}
	else
	{
		unsigned i;
		for (i = 0; i < list->size; i++)
		{
			if (!list->structure[i])
			{
				slot = i;
				break;
			}
		}
		if (i >= list->size)
			return false;
	}

	if (!ofc_hashmap_add(
		list->map, structure))
		return false;

	list->structure[slot] = structure;
	list->count++;
	return true;
}

void ofc_sema_structure_list_remove(
	ofc_sema_structure_list_t* list,
	ofc_sema_structure_t* structure)
{
	if (!list || !structure)
		return;

	ofc_hashmap_remove(
		list->map, structure);

	unsigned i;
	for (i = 0; i < list->size; i++)
	{
		if (list->structure[i] == structure)
		{
			list->structure[i] = NULL;
			list->count--;
		}
	}
}


const ofc_sema_structure_t* ofc_sema_structure_list_find(
	const ofc_sema_structure_list_t* list, ofc_str_ref_t name)
{
	if (!list) return NULL;
	return ofc_hashmap_find(list->map, &name);
}

ofc_sema_structure_t* ofc_sema_structure_list_find_modify(
	ofc_sema_structure_list_t* list, ofc_str_ref_t name)
{
	if (!list) return false;
	return ofc_hashmap_find_modify(list->map, &name);
}


bool ofc_sema_structure_list_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_sema_structure_list_t* list)
{
	if (!list)
		return false;

	unsigned i;
	for (i = 0; i < list->size; i++)
	{
		if (list->structure[i]
			&& !ofc_sema_structure_print(
			cs, indent, list->structure[i]))
			return false;
	}

	return true;
}

bool ofc_sema_structure_list_foreach(
	ofc_sema_structure_list_t* list, void* param,
	bool (*func)(ofc_sema_structure_t* structure, void* param))
{
	if (!list || !func)
		return false;

	unsigned i;
	for (i = 0; i < list->size; i++)
	{
		if (list->structure[i]
			&& !func(list->structure[i], param))
			return false;
	}

	return true;
}

bool ofc_sema_structure_list_foreach_expr(
	ofc_sema_structure_list_t* list, void* param,
	bool (*func)(ofc_sema_expr_t* expr, void* param))
{
	if (!list)
		return false;

	unsigned i;
	for (i = 0; i < list->size; i++)
	{
		if (list->structure[i]
			&& !ofc_sema_structure_foreach_expr(
				list->structure[i], param, func))
			return false;
	}

	return true;
}
