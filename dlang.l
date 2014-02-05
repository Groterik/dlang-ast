%{

/*
 * Lexer.l file
 * To generate the lexical analyzer run: "flex Lexer.l"
 */

#include "dast.h"
#include "dlang_grm.hpp"

#include <stdio.h>
#include <iostream>

%}

%option warn nodefault
%option header-file="dlang_lex.h"

%option reentrant noyywrap never-interactive nounistd
%option bison-bridge

D			[0-9]
L			[a-zA-Z_]
H			[a-fA-F0-9]
E			[Ee][+-]?{D}+
FS			(f|F|l|L)
IS			(u|U|l|L)*
LP                      "{"
RP                      "}"
SM                      ";"


WS          [ \r\n\t]*

%%

"abstact"               { return(TOK_ABSTRACT); }
"alias"                 { return(TOK_ALIAS); }
"align"                 { return(TOK_ALIGN); }

"&"			{ return(TOK_AMPERSAND); }
"&&"			{ return(TOK_AND); }
"&="			{ return(TOK_AND_BIN_ASSIGN); }

"assert"                { return(TOK_ASSERT); }

"="			{ return(TOK_ASSIGN); }

"@"                     { return(TOK_AT); }
"@disable"              { return(TOK_ATDISABLE); }
"@property"             { return(TOK_ATPROPERTY); }

"auto"			{ return(TOK_AUTO); }
"body"                  { return(TOK_BODY); }
"bool"                  { return(TOK_BOOL); }
"break"			{ return(TOK_BREAK); }
"byte"                  { return(TOK_BYTE); }
"case"			{ return(TOK_CASE); }
"cast"                  { return(TOK_CAST); }
"catch"                 { return(TOK_CATCH); }
"cdouble"               { return(TOK_CDOUBLE); }
"cfloat"                { return(TOK_CFLOAT); }
"char"			{ return(TOK_CHAR); }

\'.\'                   { return(TOK_CHAR_LITERAL); }

"class"                 { return(TOK_CLASS); }

":"			{ return(TOK_COLON); }
","			{ return(TOK_COMMA); }

"const"			{ return(TOK_CONST); }
"continue"		{ return(TOK_CONTINUE); }
"creal"                 { return(TOK_CREAL); }
"dchar"                 { return(TOK_DCHAR); }

"--"			{ return(TOK_DEC); }

"default"		{ return(TOK_DEFAULT); }
"delegate"              { return(TOK_DELEGATE); }
"delete"                { return(TOK_DELETE); }
"deprecated"            { return(TOK_DEPRECATED); }
"disable"               { return(TOK_DISABLE); }

"/"			{ return(TOK_DIV); }
"/="			{ return(TOK_DIV_ASSIGN); }

"do"			{ return(TOK_DO); }

"$"                     { return(TOK_DOLLAR); }
"."			{ return(TOK_DOT); }

"double"		{ return(TOK_DOUBLE); }

".."                    { return(TOK_DOUBLE_DOT); }
"..."			{ return(TOK_ELLIPSIS); }

"else"			{ return(TOK_ELSE); }
"enum"			{ return(TOK_ENUM); }

"=="			{ return(TOK_EQ); }
"!"			{ return(TOK_EXCLAMATION); }

"export"                { return(TOK_EXPORT); }
"extern"		{ return(TOK_EXTERN); }
"false"                 { return(TOK_FALSE); }

"__FILE__"              { return(TOK__FILE); }

"final"                 { return(TOK_FINAL); }
"finally"               { return(TOK_FINALLY); }
"float"                 { return(TOK_FLOAT); }
"for"                   { return(TOK_FOR); }
"foreach"               { return(TOK_FOREACH); }
"foreach_reverse"       { return(TOK_FOREACH_REVERSE); }
"function"              { return(TOK_FUNCTION); }

"__FUNCTION__"          { return(TOK__FUNCTION); }

">="			{ return(TOK_GE); }

"goto"			{ return(TOK_GOTO); }

">"			{ return(TOK_GRT); }

"__gshared"             { return(TOK__GSHARED); }

"idouble"               { return(TOK_IDOUBLE); }
"if"			{ return(TOK_IF); }
"ifloat"                { return(TOK_IFLOAT); }
"immutable"             { return(TOK_IMMUTABLE); }
"import"                { return(TOK_IMPORT); }
"in"                    { return(TOK_IN); }

"++"			{ return(TOK_INC); }

"inout"                 { return(TOK_INOUT); }
"int"			{ return(TOK_INT); }
"interface"             { return(TOK_INTERFACE); }
"invariant"             { return(TOK_INVARIANT); }
"ireal"                 { return(TOK_IREAL); }
"is"                    { return(TOK_IS); }
"lazy"                  { return(TOK_LAZY); }

"<="			{ return(TOK_LE); }
"<<"			{ return(TOK_LEFT); }
"<<="			{ return(TOK_LEFT_ASSIGN); }

"{"                     { return(TOK_LEFT_BRACE); }
"("			{ return(TOK_LEFT_PAR); }
"["		        { return(TOK_LEFT_SQUARE); }

"<>"                    { return(TOK_LG); }
"<>="                   { return(TOK_LGE); }

"__LINE__"              { return(TOK__LINE); }

"C"                     { return(TOK_LINK_C); }
"C++"                   { return(TOK_LINK_CPP); }
"D"                     { return(TOK_LINK_D); }
"Pascal"                { return(TOK_LINK_PASCAL); }
"System"                { return(TOK_LINK_SYSTEM); }
"Windows"               { return(TOK_LINK_WINDOWS); }

"long"			{ return(TOK_LONG); }

"<"			{ return(TOK_LSS); }
"-"			{ return(TOK_MINUS); }
"-="			{ return(TOK_MINUS_ASSIGN); }

"mixin"                 { return(TOK_MIXIN); }

"%"			{ return(TOK_MOD); }
"%="			{ return(TOK_MOD_ASSIGN); }

"module"                { return(TOK_MODULE); }

"__MODULE__"            { return(TOK__MODULE); }

"*"			{ return(TOK_MUL); }
"*="			{ return(TOK_MUL_ASSIGN); }
"!="			{ return(TOK_NE); }

"new"                   { return(TOK_NEW); }
"nothrow"               { return(TOK_NOTHROW); }

"!in"                   { return(TOK_NOTIN); }
"!is"                   { return(TOK_NOTIS); }

"null"                  { return(TOK_NULL); }

"||"			{ return(TOK_OR); }
"|="			{ return(TOK_OR_BIN_ASSIGN); }

"out"                   { return(TOK_OUT); }
"override"              { return(TOK_OVERRIDE); }
"package"               { return(TOK_PACKAGE); }

"__parameters"          { return(TOK__PARAMETERS); }

">>>"                   { return(TOK_PINE); }
">>>="                  { return(TOK_PINE_ASSIGN); }
"+"			{ return(TOK_PLUS); }
"+="			{ return(TOK_PLUS_ASSIGN); }
"^^"                    { return(TOK_POW); }
"^^="                   { return(TOK_POW_ASSIGN); }

"pramga"                { return(TOK_PRAGMA); }

"__PRETY_FUNCTION__"    { return(TOK__PRETTY_FUNCTION); }

"private"               { return(TOK_PRIVATE); }
"property"              { return(TOK_PROPERTY); }
"protected"             { return(TOK_PROTECTED); }
"public"                { return(TOK_PUBLIC); }
"pure"                  { return(TOK_PURE); }

"?"			{ return(TOK_QUESTION); }

"real"                  { return(TOK_REAL); }
"ref"                   { return(TOK_REF); }
"register"		{ return(TOK_REGISTER); }
"return"		{ return(TOK_RETURN); }

">>"			{ return(TOK_RIGHT); }
"=>"                    { return(TOK_RIGHT_ARROW); }
">>="			{ return(TOK_RIGHT_ASSIGN); }

"}"		        { return(TOK_RIGHT_BRACE); }
")"			{ return(TOK_RIGHT_PAR); }
"]"	          	{ return(TOK_RIGHT_SQUARE); }

"safe"                  { return(TOK_SAFE); }
"scope"                 { return(TOK_SCOPE); }
"scope(exit)"           { return(TOK_SCOPE_EXIT); }
"scope(failure)"        { return(TOK_SCOPE_FAILURE); }
"scope(success)"        { return(TOK_SCOPE_SUCCESS); }

";"			{ return(TOK_SEMICOLON); }

"shared"                { return(TOK_SHARED); }
"short"			{ return(TOK_SHORT); }
"signed"		{ return(TOK_SIGNED); }
"sizeof"		{ return(TOK_SIZEOF); }
"static"		{ return(TOK_STATIC); }

L?\"(\\.|[^\\"])*\"	{ return(TOK_STRING_LITERAL); }

"struct"		{ return(TOK_STRUCT); }
"super"                 { return(TOK_SUPER); }
"switch"                { return(TOK_SWITCH); }
"synchronized"          { return(TOK_SYNCHRONIZED); }
"system"                { return(TOK_SYSTEM); }
"template"              { return(TOK_TEMPLATE); }
"this"                  { return(TOK_THIS); }
"throw"                 { return(TOK_THROW); }

"~"			{ return(TOK_TILDA); }
"~="                    { return(TOK_TILDA_ASSIGN); }

"true"                  { return(TOK_TRUE); }
"trusted"               { return(TOK_TRUSTED); }
"try"                   { return(TOK_TRY); }
"typedef"               { return(TOK_TYPEDEF); }
"typeid"                { return(TOK_TYPEID); }
"typeof"                { return(TOK_TYPEOF); }
"ubyte"                 { return(TOK_UBYTE); }
"uint"                  { return(TOK_UINT); }
"ulong"                 { return(TOK_ULONG); }
"union"			{ return(TOK_UNION); }
"unittest"              { return(TOK_UNITTEST); }

"!<>="                  { return(TOK_UNORDERED); }
"!<>"                   { return(TOK_UNORDERED_E); }
"!<="                   { return(TOK_UNORDERED_G); }
"!<"                    { return(TOK_UNORDERED_GE); }
"!>="                   { return(TOK_UNORDERED_L); }
"!>"                    { return(TOK_UNORDERED_LE); }
"^"			{ return(TOK_UP_ARROW); }

"ushort"                { return(TOK_USHORT); }

"|"			{ return(TOK_VERTICAL); }

"void"                  { return(TOK_VOID); }
"wchar"                 { return(TOK_WCHAR); }
"while"                 { return(TOK_WHILE); }
"with"                  { return(TOK_WITH); }

"^="                    { return(TOK_XOR_ASSIGN); }



{L}({L}|{D})*		{ yylval->str = strdup(yytext); return(TOK_IDENTIFIER); }

0[xX]{H}+{IS}?		{ sscanf(yytext, "%d", &yylval->ivalue); return(TOK_INT_CONSTANT); }
0{D}+{IS}?		{ sscanf(yytext, "%d", &yylval->ivalue); return(TOK_INT_CONSTANT); }
{D}+{IS}?		{ sscanf(yytext, "%d", &yylval->ivalue); return(TOK_INT_CONSTANT); }
L?'(\\.|[^\\'])+'	{ sscanf(yytext, "%d", &yylval->ivalue); return(TOK_INT_CONSTANT); }

{D}+{E}{FS}?		{ sscanf(yytext, "%lf", &yylval->rvalue); return(TOK_REAL_CONSTANT); }
{D}*"."{D}+({E})?{FS}?	{ sscanf(yytext, "%lf", &yylval->rvalue); return(TOK_REAL_CONSTANT); }
{D}+"."{D}*({E})?{FS}?	{ sscanf(yytext, "%lf", &yylval->rvalue); return(TOK_REAL_CONSTANT); }


[ \t\v\n\f]		{ }
.			{ /* ignore bad characters */ }


%%

/*
void comment()
{
        char c, c1;

cloop:
        while ((c = input()) != '*' && c != 0)
                putchar(c);

        if ((c1 = input()) != '/' && c != 0)
        {
                unput(c1);
                goto cloop;
        }

        if (c != 0)
                putchar(c1);
}*/

int yyerror(const char *msg) {
    std::cerr << "Error:" << msg; return 0;
}
