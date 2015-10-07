#include "parse.h"

unsigned parse_label(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_label_t* label)
{
	unsigned len = parse_unsigned(
		src, ptr, debug, &label->number);
	label->type = (len > 0 ? PARSE_LABEL_NUMBER : PARSE_LABEL_VARIABLE);
	if (len > 0) return len;

	len = parse_name(
		src, ptr, debug,
		&label->variable);
	return len;
}

bool parse_label_print(
	colstr_t* cs, const parse_label_t label)
{
	if (label.type == PARSE_LABEL_NUMBER)
		return colstr_atomic_writef(cs, "%u", label.number);
	return str_ref_print(cs, label.variable);
}
