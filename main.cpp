/*
 * main.c file
 */

#include "dast.h"
#include "dlang_grm.hpp"
#include "dlang_lex.h"

#include <stdio.h>
#include <iostream>

int yyparse(Expression **expression, yyscan_t scanner);

Expression *getAST(const char *expr)
{
    Expression *expression;
    yyscan_t scanner;
    YY_BUFFER_STATE state;

    if (yylex_init(&scanner)) {
        // couldn't initialize
        return NULL;
    }

    state = yy_scan_string(expr, scanner);

    if (yyparse(&expression, scanner)) {
        // error parsing
        return NULL;
    }

    yy_delete_buffer(state, scanner);

    yylex_destroy(scanner);

    return expression;
}

int main(void)
{
    Expression *e = NULL;
    char test[]="module a.b;";
    int result = 0;

    e = getAST(test);

    std::cout << e->name() << std::endl;

    return 0;
}
