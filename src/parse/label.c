#include "parse.h"

unsigned parse_label(
	const sparse_t* src, const char* ptr,
	parse_label_t* label)
{
	unsigned len = parse_unsigned(
		src, ptr, &label->number);
	label->type = (len > 0 ? PARSE_LABEL_NUMBER : PARSE_LABEL_VARIABLE);
	if (len > 0) return len;

	len = parse_name(src, ptr,
		&label->variable);
	return len;
}

bool parse_label_print(
	int fd, const parse_label_t label)
{
	if (label.type == PARSE_LABEL_NUMBER)
		return dprintf_bool(fd, "%u", label.number);
	return str_ref_print(fd, label.variable);
}
