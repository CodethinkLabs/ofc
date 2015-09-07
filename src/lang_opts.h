#ifndef __lang_opts_h__
#define __lang_opts_h__

#include <stdbool.h>

typedef enum
{
	LANG_FORM_FIXED = 0,
	LANG_FORM_FREE,
	LANG_FORM_TAB,
} lang_form_e;

typedef struct
{
	lang_form_e form;
	unsigned    tab_width;
	bool        debug;
	unsigned    columns;
	bool        case_sensitive;
} lang_opts_t;

static const lang_opts_t LANG_OPTS_F77 =
{
	.form           = LANG_FORM_FIXED,
	.tab_width      = 8,
	.debug          = false,
	.columns        = 72,
	.case_sensitive = false,
};

static const lang_opts_t LANG_OPTS_F77_TAB =
{
	.form           = LANG_FORM_TAB,
	.tab_width      = 1,
	.debug          = false,
	.columns        = 72,
	.case_sensitive = false,
};

static const lang_opts_t LANG_OPTS_F90 =
{
	.form           = LANG_FORM_FREE,
	.tab_width      = 8,
	.debug          = false,
	.columns        = 132,
	.case_sensitive = false,
};

#endif
