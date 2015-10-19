#include <ofc/sema.h>

static ofc_sema_parameter_t* ofc_sema_parameter__create(
	ofc_str_ref_t name,
	ofc_sema_typeval_t* typeval)
{
	ofc_sema_parameter_t* parameter
		= (ofc_sema_parameter_t*)malloc(
			sizeof(ofc_sema_parameter_t));
	if (!parameter) return NULL;

	parameter->name    = name;
	parameter->typeval = typeval;
	return parameter;
}

static ofc_sema_parameter_t* ofc_sema_parameter__assign(
	const ofc_parse_assign_t* assign,
	const ofc_hashmap_t* map)
{
	if (!assign
		|| !assign->name)
		return NULL;

	if (assign->name->type
		!= OFC_PARSE_LHS_VARIABLE)
	{
		/* TODO - Error: Expected parameter name. */
		return NULL;
	}

	const ofc_sema_parameter_t* exists
		= ofc_hashmap_find(map, &assign->name->variable);
	if (exists)
	{
		/* TODO - Error: Duplicate PARAMETER definition. */
		return NULL;
	}

	ofc_sema_expr_t* expr
		= ofc_sema_expr(assign->init);
	if (!expr)
	{
		/* TODO - Error: Invalid PARAMETER expression. */
		return NULL;
	}

	ofc_sema_typeval_t* typeval
		= ofc_sema_expr_resolve(expr);
	ofc_sema_expr_delete(expr);
	if (!typeval)
	{
		/* TODO - Error: Failed to resolve PARAMETER value. */
		return NULL;
	}

	ofc_sema_parameter_t* param
		= ofc_sema_parameter__create(
			assign->name->variable, typeval);
	if (!param)
	{
		ofc_sema_typeval_delete(typeval);
		return NULL;
	}

	return param;
}

bool ofc_sema_parameter(
	const ofc_parse_stmt_t* stmt,
	ofc_hashmap_t* map)
{
	if (!stmt || !map
		|| (stmt->type != OFC_PARSE_STMT_PARAMETER))
		return false;

	unsigned count = stmt->parameter.list->count;
	if (count == 0)
		return false;

	ofc_sema_parameter_t* param[count];

	unsigned i;
	for (i = 0; i < count; i++)
	{
		param[i] = ofc_sema_parameter__assign(
			stmt->parameter.list->assign[i], map);
		if (!param[i])
		{
			unsigned j;
			for (j = 0; j < i; j++)
				ofc_sema_parameter_delete(param[j]);
			return false;
		}
	}

	for (i = 0; i < count; i++)
	{
		if (!ofc_hashmap_add(map, param[i]))
		{
			/* This should never happen. */
			abort();
		}
	}

	return true;
}

bool ofc_sema_parameter_decl(
	const ofc_parse_stmt_t* stmt,
	ofc_hashmap_t* map)
{
	if (!stmt || !map
		|| (stmt->type != OFC_PARSE_STMT_DECL))
		return false;

	/* TODO - Implement Fortran 90 style typed PARAMETER declarations. */
	return false;
}

void ofc_sema_parameter_delete(
	ofc_sema_parameter_t* parameter)
{
	if (!parameter)
		return;

	ofc_sema_typeval_delete(parameter->typeval);
	free(parameter);
}


bool ofc_sema_parameter_int32(
	const ofc_sema_parameter_t* parameter,
	int32_t* value)
{
	if (!parameter)
		return false;


	static const ofc_sema_type_t* integer = NULL;
	if (!integer)
	{
		integer = ofc_sema_type_create_primitive(
			OFC_SEMA_TYPE_INTEGER, 4, false, false, false);
		if (!integer) return false;
	}

	bool lossy = false;
	bool success = ofc_sema_typeval_get(
		parameter->typeval,
		integer, value, &lossy);

	/* TODO - Allow lossy conversions on parameter read? */
	return (success && !lossy);
}
