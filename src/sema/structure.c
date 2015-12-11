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

#include <ofc/hashmap.h>
#include <ofc/sema.h>


void ofc_sema_structure__delete_locked(ofc_sema_structure_t* structure)
{
	if (!structure)
		return;

	free(structure->member.type);
	free(structure->member.name);
	free(structure);
}

void ofc_sema_structure_delete(
	ofc_sema_structure_t* structure)
{
	if (structure && structure->locked)
		ofc_sema_structure__delete_locked(structure);
}


ofc_sema_structure_t* ofc_sema_structure_create(bool is_vax)
{
	ofc_sema_structure_t* structure
		= (ofc_sema_structure_t*)malloc(
			sizeof(ofc_sema_structure_t));
	if (!structure) return NULL;

	structure->is_vax   = is_vax;
	structure->is_union = false;

	structure->member.count = 0;
	structure->member.type  = NULL;
	structure->member.name  = NULL;

	structure->locked = false;

	return structure;
}

ofc_sema_structure_t* ofc_sema_structure_create_union(void)
{
	ofc_sema_structure_t* structure
		= ofc_sema_structure_create(true);
	if (!structure) return NULL;

	structure->is_union = true;
	return structure;
}


bool ofc_sema_structure_append(
	ofc_sema_structure_t* structure,
	const ofc_sema_type_t* type, ofc_str_ref_t name)
{
	if (!structure || !type
		|| structure->locked
		|| ofc_str_ref_empty(name))
		return false;

	const ofc_sema_type_t** ntype
		= (const ofc_sema_type_t**)realloc(structure->member.type,
			(sizeof(const ofc_sema_type_t*) * (structure->member.count + 1)));
	if (!ntype) return false;
	structure->member.type = ntype;

	ofc_str_ref_t* nname
		= (ofc_str_ref_t*)realloc(structure->member.name,
			(sizeof(ofc_str_ref_t) * (structure->member.count + 1)));
	if (!nname) return false;
	structure->member.name = nname;

	structure->member.type[structure->member.count] = type;
	structure->member.name[structure->member.count] = name;
	structure->member.count++;
	return true;
}


const ofc_sema_structure_t* ofc_sema_structure__key(
	const ofc_sema_structure_t* structure)
{
	return structure;
}

ofc_hashmap_t* ofc_sema_structure__map = NULL;

bool ofc_sema_structure_complete(
	ofc_sema_structure_t* structure)
{
	if (!structure)
		return false;

	if (structure->locked)
		return true;

	if (!ofc_sema_structure__map)
	{
		ofc_sema_structure__map = ofc_hashmap_create(
			(void*)ofc_sema_structure_hash,
			(void*)ofc_sema_structure_compare,
			(void*)ofc_sema_structure__key,
			(void*)ofc_sema_structure__delete_locked);
		if (!ofc_sema_structure__map)
			return false;
	}

	if (!ofc_hashmap_add(
		ofc_sema_structure__map,
		structure))
		return false;

	structure->locked = true;
	return true;
}


uint8_t ofc_sema_structure_hash(
	const ofc_sema_structure_t* structure)
{
	if (!structure)
		return 0;

	uint8_t hash = (structure->is_vax
		+ structure->is_union);

	unsigned i;
	for (i = 0; i < structure->member.count; i++)
	{
		hash += ofc_str_ref_hash(structure->member.name[i]);
		hash += ofc_sema_type_hash(structure->member.type[i]);
	}

	return hash;
}

bool ofc_sema_structure_compare(
	const ofc_sema_structure_t* a,
	const ofc_sema_structure_t* b)
{
	if (!a || !b)
		return false;

	if (a == b)
		return true;

	if ((a->is_vax != b->is_vax)
		|| (a->is_union != b->is_union)
		|| (a->member.count != b->member.count))
		return false;

	unsigned i;
	for (i = 0; i < a->member.count; i++)
	{
		/* TODO - Compare case insensitively dependent on lang_opts */
		if (!ofc_str_ref_equal(
			a->member.name[i],
			b->member.name[i]))
			return false;

		if (!ofc_sema_type_compare(
			a->member.type[i],
			b->member.type[i]))
			return false;
	}

	return true;
}


bool ofc_sema_structure_size(
	const ofc_sema_structure_t* structure,
	unsigned* size)
{
	if (!structure)
		return false;

	unsigned total = 0, max = 0;
	unsigned i;
	for (i = 0; i < structure->member.count; i++)
	{
		unsigned msize;
		if (!ofc_sema_type_size(
			structure->member.type[i], &msize))
			return false;
		total += msize;

		if (msize > max)
			max = msize;
	}

	if (size)
		*size = (structure->is_union ? max : total);
	return true;
}

bool ofc_sema_structure_elem_count(
	const ofc_sema_structure_t* structure,
	unsigned* count)
{
	if (!structure
		|| (structure->member.count == 0))
		return false;

	unsigned total = 0, max = 0;
	unsigned i;
	for (i = 0; i < structure->member.count; i++)
	{
		unsigned melem;
		if (!ofc_sema_type_elem_count(
			structure->member.type[i], &melem))
			return false;
		total += melem;

		if (melem > max)
			max = melem;
	}

	if (count)
		*count = (structure->is_union ? max : total);
	return true;
}
