#include "parse.h"

unsigned ofc_parse_label(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_label_t* label)
{
	unsigned len = ofc_parse_unsigned(
		src, ptr, debug, &label->number);
	label->type = (len > 0 ? OFC_PARSE_LABEL_NUMBER : OFC_PARSE_LABEL_VARIABLE);
	if (len > 0) return len;

	len = ofc_parse_name(
		src, ptr, debug,
		&label->variable);
	return len;
}

bool ofc_parse_label_print(
	ofc_colstr_t* cs, const ofc_parse_label_t label)
{
	if (label.type == OFC_PARSE_LABEL_NUMBER)
		return ofc_colstr_atomic_writef(cs, "%u", label.number);
	return ofc_str_ref_print(cs, label.variable);
}
