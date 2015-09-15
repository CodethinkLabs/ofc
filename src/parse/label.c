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
