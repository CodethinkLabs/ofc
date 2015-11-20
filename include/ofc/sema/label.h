#ifndef __ofc_sema_label_h__
#define __ofc_sema_label_h__

typedef enum
{
	OFC_SEMA_LABEL_STMT = 0,
	OFC_SEMA_LABEL_FORMAT,

	OFC_SEMA_LABEL_COUNT
} ofc_sema_label_e;

typedef struct
{
	ofc_sema_label_e type;
	unsigned         number;

	union
	{
		unsigned offset;
		ofc_sema_format_t* format;
	};
} ofc_sema_label_t;

struct ofc_sema_format_label_list_s
{
	unsigned count;
	ofc_sema_label_t** format;
};

typedef struct
{
	/* This references statements and formats. */
	ofc_hashmap_t*                label;

	/* This owns the statement labels. */
	ofc_hashmap_t*                offset;

	/* This owns the format labels. */
	ofc_sema_format_label_list_t* format;
} ofc_sema_label_map_t;

ofc_sema_label_map_t* ofc_sema_label_map_create();
void ofc_sema_label_map_delete(
	ofc_sema_label_map_t* label_map);

bool ofc_sema_label_map_add_stmt(
	const ofc_sema_scope_t* scope, const ofc_parse_stmt_t* stmt,
	ofc_sema_label_map_t* map, unsigned label, unsigned offset);

ofc_sema_format_label_list_t* ofc_sema_format_label_list_create(void);
void ofc_sema_format_label_list_delete(
	ofc_sema_format_label_list_t* label_list);

bool ofc_sema_format_label_list_add(
	ofc_sema_format_label_list_t* list,
	ofc_sema_label_t* format);
bool ofc_sema_format_label_list_print(ofc_colstr_t* cs,
	ofc_sema_format_label_list_t* list);
bool ofc_sema_format_label_print(ofc_colstr_t* cs,
	ofc_sema_label_t* label);
bool ofc_sema_label_map_add_format(
	const ofc_sema_scope_t* scope, const ofc_parse_stmt_t* stmt,
	ofc_sema_label_map_t* map, unsigned label,
	ofc_sema_format_t* format);
const ofc_sema_label_t* ofc_sema_label_map_find(
	const ofc_hashmap_t* map, unsigned label);

#endif
