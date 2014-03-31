/*
 * main.c file
 */

#include "dast.h"
#include "dlang_grm.hpp"
#include "dlang_lex.h"

#include <stdio.h>
#include <iostream>

#include <fstream>
#include <string>
#include <cerrno>

void getFileContents(std::string& contents, const char *filename)
{
  std::ifstream in(filename, std::ios::in | std::ios::binary);
  if (in)
  {
    in.seekg(0, std::ios::end);
    contents.resize(in.tellg());
    in.seekg(0, std::ios::beg);
    in.read(&contents[0], contents.size());
    in.close();
    return;
  }
  throw(errno);
}

int yyparse(ModuleNode **expression, yyscan_t scanner);

extern int yydebug;

ModuleNode *getAST(const char *expr)
{
    ModuleNode *expression;
    yyscan_t scanner;
    YY_BUFFER_STATE state;

    if (yylex_init(&scanner)) {
        // couldn't initialize
        return NULL;
    }

    state = yy_scan_string(expr, scanner);

    yydebug = 0;
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
    std::string str;

    getFileContents(str, "test.d");

//    yydebug = 1;

    ModuleNode* e;
    e = getAST(str.c_str());
    if (!e)
    {
        std::cerr << "MainError" << std::endl;
        exit(1);
    }

    std::cout << *e << std::endl;

    return 0;
}
