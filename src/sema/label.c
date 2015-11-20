#include <ofc/sema.h>

static void ofc_sema_label__delete(
	ofc_sema_label_t* label)
{
	if (!label)
		return;

	if (label->type == OFC_SEMA_LABEL_FORMAT)
		ofc_sema_format_delete(label->format);

	free(label);
}

static ofc_sema_label_t* ofc_sema_label__stmt(
	unsigned number, unsigned offset)
{
	ofc_sema_label_t* label
		= (ofc_sema_label_t*)malloc(
			sizeof(ofc_sema_label_t));
	if (!label) return NULL;

	label->type   = OFC_SEMA_LABEL_STMT;
	label->number = number;
	label->offset = offset;

	return label;
}

static ofc_sema_label_t* ofc_sema_label__format(
	unsigned number, ofc_sema_format_t* format)
{
	if (!format)
		return NULL;

	ofc_sema_label_t* label
		= (ofc_sema_label_t*)malloc(
			sizeof(ofc_sema_label_t));
	if (!label) return NULL;

	label->type   = OFC_SEMA_LABEL_FORMAT;
	label->number = number;
	label->format = format;

	return label;
}

static const unsigned* ofc_sema_label__number(
	const ofc_sema_label_t* label)
{
	return (label ? &label->number : NULL);
}

static const unsigned* ofc_sema_label__offset(
	const ofc_sema_label_t* label)
{
	return (label ? &label->offset : NULL);
}

static bool ofc_sema_label__compare(
	const unsigned* a, const unsigned* b)
{
	if (!a || !b)
		return false;
	return (*a == *b);
}

static uint8_t ofc_sema_label__hash(const unsigned* label)
{
	if (!label)
		return 0;

	unsigned h = *label;
	h ^= (h >> (sizeof(h) * 4));
	h ^= (h >> (sizeof(h) * 2));

	return (h & 0xFF);
}

static uint8_t ofc_sema_label_offset__hash(const unsigned* offset)
{
	if (!offset)
		return 0;

	return (*offset & 0xFF);
}

ofc_sema_label_map_t* ofc_sema_label_map_create()
{
	ofc_sema_label_map_t* map
		= (ofc_sema_label_map_t*)malloc(
			sizeof(ofc_sema_label_map_t));
	if (!map) return NULL;

	map->label = ofc_hashmap_create(
		(void*)ofc_sema_label__hash,
		(void*)ofc_sema_label__compare,
		(void*)ofc_sema_label__number,
		NULL);

	map->offset = ofc_hashmap_create(
		(void*)ofc_sema_label_offset__hash,
		(void*)ofc_sema_label__compare,
		(void*)ofc_sema_label__offset,
		(void*)ofc_sema_label__delete);

	map->format = ofc_sema_format_label_list_create();

	if (!map->label
		|| !map->offset
		|| !map->format)
	{
		ofc_sema_label_map_delete(map);
		return NULL;
	}

	return map;
}

void ofc_sema_label_map_delete(
	ofc_sema_label_map_t* map)
{
	if (!map) return;

	ofc_sema_format_label_list_delete(map->format);
	ofc_hashmap_delete(map->offset);
	ofc_hashmap_delete(map->label);

	free(map);
}

bool ofc_sema_label_map_add_stmt(
	const ofc_sema_scope_t* scope, const ofc_parse_stmt_t* stmt,
	ofc_sema_label_map_t* map, unsigned label, unsigned offset)
{
	if (!map || !map->label || !map->offset)
		return false;

	const ofc_sema_label_t* duplicate
		= ofc_hashmap_find(map->label, &label);
	if (duplicate)
	{
		ofc_sema_scope_error(scope, stmt->src,
			"Re-definition of label %d", label);
		return false;
	}

	if (label == 0)
	{
		ofc_sema_scope_warning(scope, stmt->src,
			"Label zero isn't supported in standard Fortran");
	}

	ofc_sema_label_t* l
		= ofc_sema_label__stmt(label, offset);
	if (!l) return false;

	if (!ofc_hashmap_add(
		map->offset, l))
	{
		ofc_sema_label__delete(l);
		return false;
	}

	if (!ofc_hashmap_add(
		map->label, l))
	{
		/* This should never happen. */
		abort();
	}

	return true;
}

bool ofc_sema_label_map_add_format(
	const ofc_sema_scope_t* scope, const ofc_parse_stmt_t* stmt,
	ofc_sema_label_map_t* map, unsigned label,
	ofc_sema_format_t* format)
{
	if (!map || !map->label
		|| !map->format|| !format)
		return false;

	const ofc_sema_label_t* duplicate
		= ofc_hashmap_find(map->label, &label);
	if (duplicate)
	{
		ofc_sema_scope_error(scope, stmt->src,
			"Re-definition of label %d", label);
		return false;
	}

	if (label == 0)
	{
		ofc_sema_scope_warning(scope, stmt->src,
			"Label zero isn't supported in standard Fortran");
	}

	ofc_sema_label_t* l
		= ofc_sema_label__format(
			label, format);
	if (!l) return false;

	if (!ofc_sema_format_label_list_add(
		map->format, l))
	{
		/* Don't delete because we don't yet own format. */
		free(l);
		return false;
	}

	if (!ofc_hashmap_add(map->label, l))
	{
		/* This should never happen. */
		abort();
	}

	return true;
}

const ofc_sema_label_t* ofc_sema_label_map_find(
	const ofc_hashmap_t* map, unsigned label)
{
	return ofc_hashmap_find(map, &label);
}

ofc_sema_format_label_list_t*
	ofc_sema_format_label_list_create(void)
{
	ofc_sema_format_label_list_t* list
		= (ofc_sema_format_label_list_t*)malloc(
			sizeof(ofc_sema_format_label_list_t));
	if (!list) return NULL;

	list->count  = 0;
	list->format = NULL;

	return list;
}

void ofc_sema_format_label_list_delete(
	ofc_sema_format_label_list_t* list)
{
	if (!list) return;

	if (list->format)
	{
		unsigned i;
		for (i = 0; i < list->count; i++)
			ofc_sema_label__delete(list->format[i]);
		free(list->format);
	}

	free(list);
}

bool ofc_sema_format_label_list_add(
	ofc_sema_format_label_list_t* list,
	ofc_sema_label_t* format)
{
	if (!list || !format) return false;

    ofc_sema_label_t** nformat
		= (ofc_sema_label_t**)realloc(list->format,
			(sizeof(ofc_sema_label_t*) * (list->count + 1)));
	if (!nformat) return false;
	list->format = nformat;

	list->format[list->count++] = format;
	return true;
}

bool ofc_sema_format_label_list_print(ofc_colstr_t* cs,
	ofc_sema_format_label_list_t* list)
{
	if (!cs || !list)
		return false;

	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		if (!list->format[i]->number) return false;

		unsigned label_num = list->format[i]->number;

		if (!ofc_colstr_newline(cs, &label_num)
			|| !ofc_sema_format_label_print(cs, list->format[i]))
			return false;
	}

	return true;
}

bool ofc_sema_format_label_print(ofc_colstr_t* cs,
	ofc_sema_label_t* label)
{
	if (!cs || (label->type != OFC_SEMA_LABEL_FORMAT))
		return false;

	if (!ofc_sema_format_print(cs, label->format))
		return false;

	return true;
}
