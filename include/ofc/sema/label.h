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

ofc_hashmap_t* ofc_sema_label_map_create(void);
bool ofc_sema_label_map_add_stmt(
	const ofc_sema_scope_t* scope, const ofc_parse_stmt_t* stmt,
	ofc_hashmap_t* map, unsigned label, unsigned offset);
bool ofc_sema_label_map_add_format(
	const ofc_sema_scope_t* scope, const ofc_parse_stmt_t* stmt,
	ofc_hashmap_t* map, unsigned label, ofc_sema_format_t* format);
const ofc_sema_label_t* ofc_sema_label_map_find(
	const ofc_hashmap_t* map, unsigned label);

#endif
