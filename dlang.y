%{

#include "dlang_grm.hpp"
#include "dlang_lex.h"
#include "dast.h"

int yyerror(Expression **expression, yyscan_t scanner, const char *msg) {
    std::cout << "Error: " << msg << std::endl;
}

%}

%code requires {

#include "dast.h"
#include <iostream>

#define TRACE std::clog<<__FILE__<<' '<<__LINE__<<'\n'

#define YYDEBUG 1

#ifndef YY_TYPEDEF_YY_SCANNER_T
#define YY_TYPEDEF_YY_SCANNER_T
typedef void* yyscan_t;
#endif

}

%define api.pure
%lex-param   { yyscan_t scanner }
%parse-param { Expression **expression }
%parse-param { yyscan_t scanner }

%union {
    int ivalue;
    double rvalue;
    const char* str;
    Expression *expression;
    ExprList *list;
}

%left '+' TOK_PLUS
%left '*' TOK_MUL

%token TOK_ABSTRACT
%token TOK_ALIAS
%token TOK_ALIGN
%token TOK_AMPERSAND
%token TOK_AND
%token TOK_AND_BIN_ASSIGN
%token TOK_ASSERT
%token TOK_ASSIGN
%token TOK_AT
%token TOK_ATDISABLE
%token TOK_ATPROPERTY
%token TOK_AUTO
%token TOK_BODY
%token TOK_BOOL
%token TOK_BREAK
%token TOK_BYTE
%token TOK_CASE
%token TOK_CAST
%token TOK_CATCH
%token TOK_CDOUBLE
%token TOK_CFLOAT
%token TOK_CHAR
%token TOK_CHAR_LITERAL
%token TOK_CLASS
%token TOK_COLON
%token TOK_COMMA
%token TOK_CONST
%token TOK_CONTINUE
%token TOK_CREAL
%token TOK_DCHAR
%token TOK_DEC
%token TOK_DEFAULT
%token TOK_DELEGATE
%token TOK_DELETE
%token TOK_DEPRECATED
%token TOK_DISABLE
%token TOK_DIV
%token TOK_DIV_ASSIGN
%token TOK_DO
%token TOK_DOLLAR
%token TOK_DOT
%token TOK_DOUBLE
%token TOK_DOUBLE_DOT
%token TOK_ELLIPSIS
%token TOK_ELSE
%token TOK_ENUM
%token TOK_EQ
%token TOK_EXCLAMATION
%token TOK_EXPORT
%token TOK_EXTERN
%token TOK_FALSE
%token TOK__FILE
%token TOK_FINAL
%token TOK_FINALLY
%token TOK_FLOAT
%token TOK_FOR
%token TOK_FOREACH
%token TOK_FOREACH_REVERSE
%token TOK_FUNCTION
%token TOK__FUNCTION
%token TOK_GE
%token TOK_GOTO
%token TOK_GRT
%token TOK__GSHARED
%token TOK_IDOUBLE
%token TOK_IF
%token TOK_IFLOAT
%token TOK_IMMUTABLE
%token TOK_IMPORT
%token TOK_IN
%token TOK_INC
%token TOK_INOUT
%token TOK_INT
%token TOK_INTERFACE
%token TOK_INVARIANT
%token TOK_IREAL
%token TOK_IS
%token TOK_LAZY
%token TOK_LE
%token TOK_LEFT
%token TOK_LEFT_ASSIGN
%token TOK_LEFT_BRACE
%token TOK_LEFT_PAR
%token TOK_LEFT_SQUARE
%token TOK_LG
%token TOK_LGE
%token TOK__LINE
%token TOK_LINK_C
%token TOK_LINK_CPP
%token TOK_LINK_D
%token TOK_LINK_PASCAL
%token TOK_LINK_SYSTEM
%token TOK_LINK_WINDOWS
%token TOK_LONG
%token TOK_LSS
%token TOK_MINUS
%token TOK_MINUS_ASSIGN
%token TOK_MIXIN
%token TOK_MOD
%token TOK_MOD_ASSIGN
%token TOK_MODULE
%token TOK__MODULE
%token TOK_MUL
%token TOK_MUL_ASSIGN
%token TOK_NE
%token TOK_NEW
%token TOK_NOTHROW
%token TOK_NOTIN
%token TOK_NOTIS
%token TOK_NULL
%token TOK_OR
%token TOK_OR_BIN_ASSIGN
%token TOK_OUT
%token TOK_OVERRIDE
%token TOK_PACKAGE
%token TOK__PARAMETERS
%token TOK_PINE
%token TOK_PINE_ASSIGN
%token TOK_PLUS
%token TOK_PLUS_ASSIGN
%token TOK_POW
%token TOK_POW_ASSIGN
%token TOK_PRAGMA
%token TOK__PRETTY_FUNCTION
%token TOK_PRIVATE
%token TOK_PROPERTY
%token TOK_PROTECTED
%token TOK_PUBLIC
%token TOK_PURE
%token TOK_QUESTION
%token TOK_REAL
%token TOK_REF
%token TOK_REGISTER
%token TOK_RETURN
%token TOK_RIGHT
%token TOK_RIGHT_ARROW
%token TOK_RIGHT_ASSIGN
%token TOK_RIGHT_BRACE
%token TOK_RIGHT_PAR
%token TOK_RIGHT_SQUARE
%token TOK_SAFE
%token TOK_SCOPE
%token TOK_SCOPE_EXIT
%token TOK_SCOPE_FAILURE
%token TOK_SCOPE_SUCCESS
%token TOK_SEMICOLON
%token TOK_SHARED
%token TOK_SHORT
%token TOK_SIGNED
%token TOK_SIZEOF
%token TOK_STATIC
%token TOK_STRING_LITERAL
%token TOK_STRUCT
%token TOK_SUPER
%token TOK_SWITCH
%token TOK_SYNCHRONIZED
%token TOK_SYSTEM
%token TOK_TEMPLATE
%token TOK_THIS
%token TOK_THROW
%token TOK_TILDA
%token TOK_TILDA_ASSIGN
%token TOK_TRUE
%token TOK_TRUSTED
%token TOK_TRY
%token TOK_TYPEDEF
%token TOK_TYPEID
%token TOK_TYPEOF
%token TOK_UBYTE
%token TOK_UINT
%token TOK_ULONG
%token TOK_UNION
%token TOK_UNITTEST
%token TOK_UNORDERED
%token TOK_UNORDERED_E
%token TOK_UNORDERED_G
%token TOK_UNORDERED_GE
%token TOK_UNORDERED_L
%token TOK_UNORDERED_LE
%token TOK_UP_ARROW
%token TOK_USHORT
%token TOK_VERTICAL
%token TOK_VOID
%token TOK_WCHAR
%token TOK_WHILE
%token TOK_WITH
%token TOK_XOR_ASSIGN

%token <str> TOK_IDENTIFIER

%token <ivalue> TOK_INT_CONSTANT
%token <rvalue> TOK_REAL_CONSTANT

%type <expression> Module
%type <expression> ModuleDeclaration
%type <expression> ModuleFullyQualifiedName
%type <expression> ModuleName
%type <expression> Packages
%type <expression> PackageName
%type <expression> DeclDef
%type <list> DeclDefsopt
%type <list> DeclDefs
%type <expression> ImportDeclaration
%type <list> ImportList
%type <expression> Import

%%

input
    : Module { *expression = $1; }
    | error { /**expression = $1;*/ yyerrok; }
    ;

/* ***** Modules ***** */

Module
    : ModuleDeclaration DeclDefsopt { $$ = new Module($1->name()); $$->addChild($2); delete $2; }
    | DeclDefs { $$ = new Module(); if ($1) $$->addChild($1); delete $1; }
    ;

DeclDefsopt
    : { $$ = 0; }
    | DeclDefs { $$ = $1; }
    ;

DeclDefs
    : DeclDef { $$ = new ExprList; $$->push_back($1); }
    | DeclDef DeclDefs { $2->push_front($1); $$ = $2; }
    ;

DeclDef
    : AttributeSpecifier {}
    | ImportDeclaration { $$ = $1; }
    | EnumDeclaration {}
    | ClassDeclaration {}
    | InterfaceDeclaration {}
    | AggregateDeclaration {}
    | Declaration {}
    | Constructor {}
    | Destructor {}
    | UnitTest {}
    | StaticConstructor {}
    | StaticDestructor {}
    | SharedStaticConstructor {}
    | SharedStaticDestructor {}
/*    | ConditionalDeclaration {}*/ /* TODO */
/*    | DebugSpecification {}*/ /* TODO */
  /*  | VersionSpecification {}*/ /* TODO */
    /*| StaticAssert {}*/
    | TemplateDeclaration {}
    | TemplateMixinDeclaration {}
    | TemplateMixin {}
    | MixinDeclaration {}
    | TOK_SEMICOLON {}
    ;


ModuleDeclaration
    : TOK_MODULE ModuleFullyQualifiedName TOK_SEMICOLON { $$ = new Expression(MODULE, $2->name()); delete $2; }
    ;
ModuleFullyQualifiedName
    : ModuleName { $$ = $1; }
    | Packages TOK_DOT ModuleName { $$ = new Identifier($1->name() + "." + $3->name()); delete $1; delete $3; }
    ;

ModuleName
    : TOK_IDENTIFIER { $$ = new Identifier($1); delete $1; }
    ;

Packages
    : PackageName { $$ = $1; }
    | Packages TOK_DOT PackageName { $$ = new Identifier($1->name() + "." + $3->name()); delete $1; delete $3; }
    ;
PackageName
    : TOK_IDENTIFIER { $$ = new Identifier($1); delete $1; }
    ;

/* ***** Import Declaration ***** */

ImportDeclaration
    : TOK_IMPORT ImportList TOK_SEMICOLON { $$ = new Import; $$->addChild($2); delete $2; }
    | TOK_STATIC TOK_IMPORT ImportList TOK_SEMICOLON { $$ = new Import; $$->addChild($3); delete $3; }
    ;

ImportList
    : Import { $$ = new ExprList; $$->push_back($1);  }
    | ImportBindings { }
    | Import TOK_COMMA ImportList { $3->push_front($1); $$ = $3; }
    ;

Import
    : ModuleFullyQualifiedName { $$ = new Import($$->name()); }
    | ModuleAliasIdentifier TOK_COLON ModuleFullyQualifiedName { $$ = new Import($$->name()); }
    ;

ImportBindings
    : Import TOK_COLON ImportBindList { printf(" importbindings "); }
    ;

ImportBindList
    : ImportBind { printf(" importbindlist "); }
    | ImportBind TOK_COMMA ImportBindList { printf(" importbindlist2 "); }
    ;

ImportBind
    : TOK_IDENTIFIER { printf(" importbind:%s ", $1); }
    | TOK_IDENTIFIER TOK_ASSIGN TOK_IDENTIFIER { printf(" importbind:%s=%s", $1, $3); }
    ;

ModuleAliasIdentifier
    : TOK_IDENTIFIER { printf(" modalias:%s ", $1); }
    ;

/* ***** Declarations ***** */

Declaration
    : AliasDeclaration { printf(" decl "); }
    | AliasThisDeclaration { printf(" decl "); }
    | Decl { printf(" decl "); }
    ;

AliasDeclaration
    : TOK_ALIAS BasicType Declarator { printf(" aliasdecl "); }
    | TOK_ALIAS AliasInitializerList { printf(" aliasdecl "); }
    ;

AliasInitializerList
    : AliasInitializer { printf(" aliasinitializer "); }
    | AliasInitializer TOK_COMMA AliasInitializerList { printf(" aliasinitialize "); }
    ;

AliasInitializer
    : TOK_IDENTIFIER TOK_ASSIGN Type {}
    ;

AliasThisDeclaration
    : TOK_ALIAS TOK_IDENTIFIER TOK_THIS {}
    ;

Decl
    : StorageClasses Decl {}
    | BasicType Declarators TOK_SEMICOLON {}
    | BasicType Declarator FunctionBody {}
    | AutoDeclaration {}
    ;

Declarators
    : DeclaratorInitializer {}
    | DeclaratorInitializer TOK_COMMA DeclaratorIdentifierList {}
    ;

DeclaratorInitializer
    : Declarator {}
    | Declarator TOK_ASSIGN Initializer {}
    ;

DeclaratorIdentifierList
    : DeclaratorIdentifier {}
    | DeclaratorIdentifier TOK_COMMA DeclaratorIdentifierList {}
    ;

DeclaratorIdentifier
    : TOK_IDENTIFIER {}
    | TOK_IDENTIFIER TOK_ASSIGN Initializer {}
    ;

BasicType
    : BasicTypeX {}
    | TOK_DOT IdentifierList {}
    | IdentifierList {}
    | Typeof {}
    | Typeof TOK_DOT IdentifierList {}
    | TOK_CONST TOK_LEFT_PAR Type TOK_RIGHT_PAR {}
    | TOK_IMMUTABLE TOK_LEFT_PAR Type TOK_RIGHT_PAR {}
    | TOK_SHARED TOK_LEFT_PAR Type TOK_RIGHT_PAR {}
    | TOK_INOUT TOK_LEFT_PAR Type TOK_RIGHT_PAR {}
    ;

BasicTypeX
    : TOK_BOOL {}
    | TOK_BYTE {}
    | TOK_UBYTE {}
    | TOK_SHORT {}
    | TOK_USHORT {}
    | TOK_INT {}
    | TOK_UINT {}
    | TOK_LONG {}
    | TOK_ULONG {}
    | TOK_CHAR {}
    | TOK_WCHAR {}
    | TOK_DCHAR {}
    | TOK_FLOAT {}
    | TOK_DOUBLE {}
    | TOK_REAL {}
    | TOK_IFLOAT {}
    | TOK_IDOUBLE {}
    | TOK_IREAL {}
    | TOK_CFLOAT {}
    | TOK_CDOUBLE {}
    | TOK_CREAL {}
    | TOK_VOID {}
    ;

BasicType2opt
    : {}
    | BasicType2 {}
    ;

BasicType2
    : TOK_MUL {}
    | TOK_LEFT_SQUARE TOK_RIGHT_SQUARE {}
    | TOK_LEFT_SQUARE AssignExpression TOK_RIGHT_SQUARE {}
    | TOK_LEFT_SQUARE AssignExpression TOK_DOUBLE_DOT AssignExpression TOK_RIGHT_SQUARE {}
    | TOK_LEFT_SQUARE Type TOK_RIGHT_SQUARE {}
    | TOK_DELEGATE Parameters MemberFunctionAttributesopt {}
    | TOK_FUNCTION Parameters FunctionAttributesopt {}
    ;

Declarator
    : BasicType2opt TOK_LEFT_PAR Declarator TOK_RIGHT_PAR DeclaratorSuffixesopt {}
    | BasicType2opt Identifier DeclaratorSuffixesopt {}
    ;

DeclaratorSuffixesopt
    : {}
    | DeclaratorSuffixes {}
    ;

DeclaratorSuffixes
    : DeclaratorSuffix {}
    | DeclaratorSuffix DeclaratorSuffixes {}
    ;

Constraintopt
    : {}
    | TOK_INT_CONSTANT {}
    ;

DeclaratorSuffix
    : TOK_LEFT_SQUARE TOK_RIGHT_SQUARE {}
    | TOK_LEFT_SQUARE AssignExpression TOK_RIGHT_SQUARE {}
    | TOK_LEFT_SQUARE Type TOK_RIGHT_SQUARE {}
    | TemplateParametersopt Parameters MemberFunctionAttributesopt Constraintopt {}
    ;

IdentifierList
    : TOK_IDENTIFIER {}
    | TOK_IDENTIFIER TOK_DOT IdentifierList {}
    | TemplateInstance {}
    | TemplateInstance TOK_DOT IdentifierList {}
    ;

StorageClasses
    : StorageClass {}
    | StorageClass StorageClasses {}
    ;

StorageClass
    : TOK_ABSTRACT {}
    | TOK_AUTO {}
    | TypeCtor {}
    | TOK_DEPRECATED {}
    | TOK_ENUM {}
    | TOK_EXTERN {}
    | TOK_FINAL {}
    | TOK_NOTHROW {}
    | TOK_OVERRIDE {}
    | TOK_PURE {}
    | TOK__GSHARED {}
    | Property {}
    | TOK_SCOPE {}
    | TOK_STATIC {}
    | TOK_SYNCHRONIZED {}
    ;

TypeCtors
    : TypeCtor {}
    | TypeCtor TypeCtors {}
    ;

TypeCtorsopt
    : {}
    | TypeCtors {}
    ;

TypeCtor
    : TOK_CONST {}
    | TOK_IMMUTABLE {}
    | TOK_INOUT {}
    | TOK_SHARED {}
    ;

Typeopt
    : {}
    | Type {}
    ;

Type
    : TypeCtorsopt BasicType {}
    | TypeCtorsopt BasicType Declarator2 {}
    ;

Declarator2
    : BasicType2opt DeclaratorSuffixesopt {}
    | BasicType2opt TOK_LEFT_PAR Declarator2 TOK_RIGHT_PAR DeclaratorSuffixesopt {}
    ;

Parameters
    : TOK_LEFT_PAR ParameterList TOK_RIGHT_PAR {}
    | TOK_LEFT_PAR TOK_RIGHT_PAR {}
    ;

ParameterList
    : Parameter {}
    | Parameter TOK_COMMA ParameterList {}
    | TOK_ELLIPSIS {}
    ;

Parameter
    : InOutopt BasicType Declarator {}
    | InOutopt BasicType Declarator TOK_ELLIPSIS {}
    | InOutopt BasicType Declarator TOK_ASSIGN DefaultInitializerExpression {}
    | InOutopt Type {}
    | InOutopt Type TOK_ELLIPSIS {}
    ;

InOutopt
    : {}
    | InOut {}
    ;

InOut
    : InOutX {}
    | InOut InOutX {}
    ;

InOutX
    : TOK_AUTO {}
    | TypeCtor {}
    | TOK_FINAL {}
    | TOK_IN {}
    | TOK_LAZY {}
    | TOK_OUT {}
    | TOK_REF {}
    | TOK_SCOPE {}
    ;

FunctionAttributesopt
    : {}
    | FunctionAttributes {}
    ;

FunctionAttributes
    : FunctionAttribute {}
    | FunctionAttribute FunctionAttributes {}
    ;

FunctionAttribute
    : TOK_NOTHROW {}
    | TOK_PURE {}
    | Property {}
    ;

MemberFunctionAttributesopt
    : {}
    | MemberFunctionAttributes {}
    ;

MemberFunctionAttributes
    : MemberFunctionAttribute {}
    | MemberFunctionAttribute MemberFunctionAttributes {}
    ;

MemberFunctionAttribute
    : TOK_CONST {}
    | TOK_IMMUTABLE {}
    | TOK_INOUT {}
    | TOK_SHARED {}
    | FunctionAttribute {}
    ;

DefaultInitializerExpression
    : AssignExpression {}
    | TOK__FILE {}
    | TOK__MODULE {}
    | TOK__LINE {}
    | TOK__FUNCTION {}
    | TOK__PRETTY_FUNCTION {}
    ;

Initializer
    : VoidInitializer {}
    | NonVoidInitializer {}
    ;

NonVoidInitializer
    : AssignExpression {}
    | ArrayInitializer {}
    | StructInitializer {}
    ;

ArrayInitializer
    : TOK_LEFT_SQUARE TOK_RIGHT_SQUARE {}
    | TOK_LEFT_SQUARE ArrayMemberInitializations TOK_RIGHT_SQUARE {}
    ;

ArrayMemberInitializations
    : ArrayMemberInitialization {}
    | ArrayMemberInitialization TOK_COMMA {}
    | ArrayMemberInitialization TOK_COMMA ArrayMemberInitializations {}
    ;

ArrayMemberInitialization
    : NonVoidInitializer {}
    | AssignExpression TOK_COLON NonVoidInitializer {}
    ;

StructInitializer
    : TOK_LEFT_BRACE TOK_RIGHT_BRACE {}
    | TOK_LEFT_BRACE StructMemberInitializers TOK_RIGHT_BRACE {}
    ;

StructMemberInitializers
    : StructMemberInitializer {}
    | StructMemberInitializer TOK_COMMA {}
    | StructMemberInitializer TOK_COMMA StructMemberInitializers {}
    ;

StructMemberInitializer
    : NonVoidInitializer {}
    | TOK_IDENTIFIER TOK_COLON NonVoidInitializer {}
    ;

AutoDeclaration
    : StorageClasses AutoDeclarationX TOK_SEMICOLON {}
    ;

AutoDeclarationX
    : TOK_IDENTIFIER TOK_ASSIGN Initializer {}
    | AutoDeclarationX TOK_COMMA TOK_IDENTIFIER TOK_ASSIGN Initializer {}
    ;

Typeof
    : TOK_TYPEOF TOK_LEFT_PAR Expression TOK_RIGHT_PAR {}
    | TOK_TYPEOF TOK_LEFT_PAR TOK_RETURN TOK_RIGHT_PAR {}
    ;

VoidInitializer
    : TOK_VOID {}
    ;


/* ***** Attributes ***** */

AttributeSpecifier
    : Attribute TOK_COLON {}
    | Attribute DeclarationBlock {}
    ;

Attribute
    : LinkageAttribute {}
    | AlignAttribute {}
    | Pragma {}
    | DeprecatedAttribute {}
    | ProtectionAttribute {}
    | TOK_STATIC {}
    | TOK_EXTERN {}
    | TOK_FINAL {}
    | TOK_SYNCHRONIZED {}
    | TOK_OVERRIDE {}
    | TOK_ABSTRACT {}
    | TOK_AUTO {}
    | TOK_SCOPE {}
    | TOK_CONST {}
    | TOK_IMMUTABLE {}
    | TOK_INOUT {}
    | TOK_SHARED {}
    | TOK__GSHARED {}
    | TOK_ATDISABLE {}
    | TOK_ATPROPERTY {}
    | Property {}
    ;

Property
    : TOK_AT PropertyIdentifier {}
    | UserDefinedAttribute {}
    ;

PropertyIdentifier
    : TOK_PROPERTY {}
    | TOK_SAFE {}
    | TOK_TRUSTED {}
    | TOK_SYSTEM {}
    | TOK_DISABLE {}
    ;

DeclarationBlock
    : DeclDef {}
    |TOK_LEFT_BRACE DeclDefsopt TOK_RIGHT_BRACE {}
    ;

LinkageAttribute
    : TOK_EXTERN TOK_LEFT_PAR LinkageType TOK_RIGHT_PAR {}
    ;

LinkageType
    : TOK_LINK_C {}
    | TOK_LINK_CPP {}
    | TOK_LINK_D {}
    | TOK_LINK_WINDOWS {}
    | TOK_LINK_PASCAL {}
    | TOK_LINK_SYSTEM {}
    ;

AlignAttribute
    : TOK_ALIGN {}
    | TOK_ALIGN TOK_LEFT_PAR TOK_INT_CONSTANT TOK_RIGHT_PAR {}
    ;

DeprecatedAttribute
    : TOK_DEPRECATED {}
    | TOK_DEPRECATED TOK_LEFT_PAR TOK_STRING_LITERAL TOK_RIGHT_PAR {}
    ;

ProtectionAttribute
    : TOK_PRIVATE {}
    | TOK_PACKAGE {}
    | TOK_PROTECTED {}
    | TOK_PUBLIC {}
    | TOK_EXPORT {}
    ;

UserDefinedAttribute
    : TOK_AT TOK_LEFT_PAR ArgumentList TOK_RIGHT_PAR {}
    | TOK_AT TOK_IDENTIFIER {}
    | TOK_AT TOK_IDENTIFIER TOK_LEFT_PAR ArgumentListopt TOK_RIGHT_PAR {}
    ;

/* ***** Pragmas ***** */

Pragma
    : TOK_PRAGMA TOK_LEFT_PAR TOK_IDENTIFIER TOK_RIGHT_PAR {}
    | TOK_PRAGMA TOK_LEFT_PAR TOK_IDENTIFIER TOK_COMMA ArgumentList TOK_RIGHT_PAR {}
    ;

/* ***** Expressions ***** */

Expressionopt
    : {}
    | Expression {}
    ;

Expression
    : CommaExpression {}
    ;

CommaExpression
    : AssignExpression {}
    | AssignExpression TOK_COMMA CommaExpression {}
    ;


AssignExpression
    : ConditionalExpression {}
    | ConditionalExpression TOK_ASSIGN AssignExpression {}
    | ConditionalExpression TOK_PLUS_ASSIGN AssignExpression {}
    | ConditionalExpression TOK_MINUS_ASSIGN AssignExpression {}
    | ConditionalExpression TOK_MUL_ASSIGN AssignExpression {}
    | ConditionalExpression TOK_DIV_ASSIGN AssignExpression {}
    | ConditionalExpression TOK_MOD_ASSIGN AssignExpression {}
    | ConditionalExpression TOK_AND_BIN_ASSIGN AssignExpression {}
    | ConditionalExpression TOK_OR_BIN_ASSIGN AssignExpression {}
    | ConditionalExpression TOK_XOR_ASSIGN AssignExpression {}
    | ConditionalExpression TOK_TILDA_ASSIGN AssignExpression {}
    | ConditionalExpression TOK_LEFT_ASSIGN AssignExpression {}
    | ConditionalExpression TOK_RIGHT_ASSIGN AssignExpression {}
    | ConditionalExpression TOK_PINE_ASSIGN AssignExpression {}
    | ConditionalExpression TOK_POW_ASSIGN AssignExpression {}
    ;

ConditionalExpression
    : OrOrExpression {}
    | OrOrExpression TOK_QUESTION Expression TOK_COLON ConditionalExpression {}
    ;

OrOrExpression
    : AndAndExpression {}
    | OrOrExpression TOK_OR AndAndExpression {}
    ;

AndAndExpression
    : OrExpression {}
    | AndAndExpression TOK_AND OrExpression {}
    | CmpExpression {}
    | AndAndExpression TOK_AND CmpExpression {}
    ;

OrExpression
    : XorExpression {}
    | OrExpression TOK_VERTICAL XorExpression {}
    ;

XorExpression
    : AndExpression {}
    | XorExpression TOK_UP_ARROW AndExpression {}
    ;

AndExpression
    : ShiftExpression {}
    | AndExpression TOK_AMPERSAND ShiftExpression {}
    ;

CmpExpression
    : ShiftExpression {}
    | EqualExpression {}
    | IdentityExpression {}
    | RelExpression {}
    | InExpression {}
    ;

EqualExpression
    : ShiftExpression TOK_EQ ShiftExpression {}
    | ShiftExpression TOK_NE ShiftExpression {}
    ;

IdentityExpression
    : ShiftExpression TOK_IS ShiftExpression {}
    | ShiftExpression TOK_NOTIS ShiftExpression {}
    ;

RelExpression
    : ShiftExpression TOK_LSS ShiftExpression {}
    | ShiftExpression TOK_LE ShiftExpression {}
    | ShiftExpression TOK_GRT ShiftExpression {}
    | ShiftExpression TOK_GE ShiftExpression {}
    | ShiftExpression TOK_UNORDERED ShiftExpression {}
    | ShiftExpression TOK_UNORDERED_E ShiftExpression {}
    | ShiftExpression TOK_LG ShiftExpression {}
    | ShiftExpression TOK_LGE ShiftExpression {}
    | ShiftExpression TOK_UNORDERED_LE ShiftExpression {}
    | ShiftExpression TOK_UNORDERED_L ShiftExpression {}
    | ShiftExpression TOK_UNORDERED_GE ShiftExpression {}
    | ShiftExpression TOK_UNORDERED_G ShiftExpression {}
    ;

InExpression
    : ShiftExpression TOK_IN ShiftExpression {}
    | ShiftExpression TOK_NOTIN ShiftExpression {}
    ;

ShiftExpression
    : AddExpression {}
    | ShiftExpression TOK_LEFT AddExpression {}
    | ShiftExpression TOK_RIGHT AddExpression {}
    | ShiftExpression TOK_PINE AddExpression {}
    ;

AddExpression
    : MulExpression {}
    | AddExpression TOK_PLUS MulExpression {}
    | AddExpression TOK_MINUS MulExpression {}
    | CatExpression {}
    ;

CatExpression
    : AddExpression TOK_TILDA MulExpression {}
    ;

MulExpression
    : UnaryExpression {}
    | MulExpression TOK_MUL UnaryExpression {}
    | MulExpression TOK_DIV UnaryExpression {}
    | MulExpression TOK_MOD UnaryExpression {}
    ;

UnaryExpression
    : TOK_AMPERSAND UnaryExpression {}
    | TOK_INC UnaryExpression {}
    | TOK_DEC UnaryExpression {}
    | TOK_MUL UnaryExpression {}
    | TOK_DIV UnaryExpression {}
    | TOK_PLUS UnaryExpression {}
    | TOK_EXCLAMATION UnaryExpression {}
    | ComplementExpression {}
    | TOK_LEFT_PAR Type TOK_RIGHT_PAR TOK_DOT Identifier {}
    | TOK_LEFT_PAR Type TOK_RIGHT_PAR TOK_DOT TemplateInstance {}
    | DeleteExpression {}
    | CastExpression {}
    | PowExpression {}
    ;

ComplementExpression
    : TOK_TILDA UnaryExpression {}
    ;

NewExpression
    : TOK_NEW AllocatorArgumentsopt Type {}
    | NewExpressionWithArgs {}
    ;

NewExpressionWithArgs
    : TOK_NEW AllocatorArgumentsopt Type TOK_LEFT_SQUARE AssignExpression TOK_RIGHT_SQUARE {}
    | TOK_NEW AllocatorArgumentsopt Type TOK_LEFT_PAR ArgumentList TOK_RIGHT_PAR {}
    | NewAnonClassExpression {}
    ;

AllocatorArgumentsopt
    : {}
    | AllocatorArguments {}
    ;


AllocatorArguments
    : TOK_LEFT_PAR ArgumentListopt TOK_RIGHT_PAR {}
    ;

ArgumentListopt
    : {}
    | ArgumentList {}
    ;

ArgumentList
    : AssignExpression {}
    | AssignExpression TOK_COMMA {}
    | AssignExpression TOK_COMMA ArgumentList {}
    ;

DeleteExpression
    : TOK_DELETE UnaryExpression {}
    ;

CastExpression
    : TOK_CAST TOK_LEFT_PAR Type TOK_RIGHT_PAR UnaryExpression {}
    | TOK_CAST TOK_LEFT_PAR CastQual TOK_RIGHT_PAR UnaryExpression {}
    | TOK_CAST TOK_LEFT_PAR TOK_RIGHT_PAR UnaryExpression {}
    ;

CastQual
    : TOK_CONST {}
    | TOK_CONST TOK_SHARED {}
    | TOK_SHARED TOK_CONST {}
    | TOK_INOUT {}
    | TOK_INOUT TOK_SHARED {}
    | TOK_SHARED TOK_INOUT {}
    | TOK_IMMUTABLE {}
    | TOK_SHARED {}
    ;

PowExpression
    : PostfixExpression {}
    | PostfixExpression TOK_POW UnaryExpression {}
    ;

PostfixExpression
    : PrimaryExpression {}
    | PostfixExpression TOK_DOT TOK_IDENTIFIER {}
    | PostfixExpression TOK_DOT TemplateInstance {}
    | PostfixExpression TOK_DOT NewExpression {}
    | PostfixExpression TOK_INC {}
    | PostfixExpression TOK_DEC {}
    | PostfixExpression TOK_LEFT_PAR TOK_RIGHT_PAR {}
    | PostfixExpression TOK_LEFT_PAR ArgumentList TOK_RIGHT_PAR {}
    | TypeCtorsopt BasicType TOK_LEFT_PAR TOK_RIGHT_PAR {}
    | TypeCtorsopt BasicType TOK_LEFT_PAR ArgumentList TOK_RIGHT_PAR {}
    | IndexExpression {}
    | SliceExpression {}
    ;

IndexExpression
    : PostfixExpression TOK_RIGHT_SQUARE ArgumentList TOK_LEFT_SQUARE {}
    ;

SliceExpression
    : PostfixExpression TOK_LEFT_SQUARE TOK_RIGHT_SQUARE {}
    | PostfixExpression TOK_LEFT_SQUARE AssignExpression TOK_DOUBLE_DOT AssignExpression TOK_RIGHT_SQUARE {}
    ;

PrimaryExpression
    : TOK_IDENTIFIER {}
    | TOK_DOT TOK_IDENTIFIER {}
    | TemplateInstance {}
    | TOK_DOT TemplateInstance {}
    | TOK_THIS {}
    | TOK_SUPER {}
    | TOK_NULL {}
    | TOK_TRUE {}
    | TOK_FALSE {}
    | TOK_DOLLAR {}
    | TOK__FILE {}
    | TOK__MODULE {}
    | TOK__LINE {}
    | TOK__FUNCTION {}
    | TOK__PRETTY_FUNCTION {}
    | TOK_INT_CONSTANT {}
    | TOK_REAL_CONSTANT {}
    | TOK_CHAR_LITERAL {}
    | TOK_STRING_LITERAL {}
    | ArrayLiteral {}
    | AssocArrayLiteral {}
    | Lambda {}
    | FunctionLiteral {}
    | AssertExpression {}
    | MixinExpression {}
    | ImportExpression {}
    | NewExpressionWithArgs {}
    | BasicType TOK_DOT TOK_IDENTIFIER {}
    | Typeof {}
    | TypeidExpression {}
    | IsExpression {}
    | TOK_LEFT_PAR Expression TOK_RIGHT_PAR {}
    /*| TraitsExpression {}*/ /* TODO */
    ;

StringLiterals
    : TOK_STRING_LITERAL {}
    | StringLiterals TOK_STRING_LITERAL {}
    ;

ArrayLiteral
    : TOK_LEFT_SQUARE ArgumentList TOK_RIGHT_SQUARE {}
    ;

AssocArrayLiteral
    : TOK_LEFT_SQUARE KeyValuePairs TOK_RIGHT_SQUARE {}
    ;

KeyValuePairs
    : KeyValuePair {}
    | KeyValuePair TOK_COMMA KeyValuePairs {}
    ;

KeyValuePair
    : KeyExpression TOK_COLON ValueExpression {}
    ;

KeyExpression
    : AssignExpression {}
    ;

ValueExpression
    : AssignExpression {}
    ;

Lambda
    : TOK_IDENTIFIER TOK_RIGHT_ARROW AssignExpression {}
    | TOK_FUNCTION ParameterAttributes TOK_RIGHT_ARROW AssignExpression {}
    | TOK_DELEGATE ParameterAttributes TOK_RIGHT_ARROW AssignExpression {}
    | ParameterAttributes TOK_RIGHT_ARROW AssignExpression {}
    ;

FunctionLiteral
    : TOK_FUNCTION Typeopt ParameterAttributesopt FunctionBody {}
    | TOK_DELEGATE Typeopt ParameterAttributesopt FunctionBody {}
    | ParameterAttributes FunctionBody {}
    | FunctionBody {}
    ;

ParameterAttributesopt
    : {}
    | ParameterAttributes {}
    ;

ParameterAttributes
    : Parameters {}
    | Parameters FunctionAttributes {}
    ;

AssertExpression
    : TOK_ASSERT TOK_LEFT_PAR AssignExpression TOK_RIGHT_PAR {}
    | TOK_ASSERT TOK_LEFT_PAR AssignExpression TOK_COMMA AssignExpression TOK_RIGHT_PAR {}
    ;

MixinExpression
    : TOK_MIXIN TOK_LEFT_PAR AssignExpression TOK_RIGHT_PAR {}
    ;

ImportExpression
    : TOK_IMPORT TOK_LEFT_PAR AssignExpression TOK_RIGHT_PAR {}
    ;

TypeidExpression
    : TOK_TYPEID TOK_LEFT_PAR Type TOK_RIGHT_PAR {}
    | TOK_TYPEID TOK_LEFT_PAR Expression TOK_RIGHT_PAR {}
    ;

IsExpression
    : TOK_IS TOK_LEFT_PAR Type TOK_RIGHT_PAR {}
    | TOK_IS TOK_LEFT_PAR Type TOK_COLON TypeSpecialization TOK_RIGHT_PAR {}
    | TOK_IS TOK_LEFT_PAR Type TOK_EQ TypeSpecialization TOK_RIGHT_PAR {}
    | TOK_IS TOK_LEFT_PAR Type TOK_COLON TypeSpecialization TOK_COMMA TemplateParameterList TOK_RIGHT_PAR {}
    | TOK_IS TOK_LEFT_PAR Type TOK_EQ TypeSpecialization TOK_COMMA TemplateParameterList TOK_RIGHT_PAR {}
    | TOK_IS TOK_LEFT_PAR Type TOK_IDENTIFIER TOK_RIGHT_PAR {}
    | TOK_IS TOK_LEFT_PAR Type TOK_IDENTIFIER TOK_COLON TypeSpecialization TOK_RIGHT_PAR {}
    | TOK_IS TOK_LEFT_PAR Type TOK_IDENTIFIER TOK_EQ TypeSpecialization TOK_RIGHT_PAR {}
    | TOK_IS TOK_LEFT_PAR Type TOK_IDENTIFIER TOK_COLON TypeSpecialization TOK_COMMA TemplateParameterList TOK_RIGHT_PAR {}
    | TOK_IS TOK_LEFT_PAR Type TOK_IDENTIFIER TOK_EQ TypeSpecialization TOK_COMMA TemplateParameterList TOK_RIGHT_PAR {}
    ;

TypeSpecialization
    : Type {}
    | TOK_STRUCT {}
    | TOK_UNION {}
    | TOK_CLASS {}
    | TOK_INTERFACE {}
    | TOK_ENUM {}
    | TOK_FUNCTION {}
    | TOK_DELEGATE {}
    | TOK_SUPER {}
    | TOK_CONST {}
    | TOK_IMMUTABLE {}
    | TOK_INOUT {}
    | TOK_SHARED {}
    | TOK_RETURN {}
    | TOK__PARAMETERS {}
    ;

/* ***** Statements ***** */

Statement
    : TOK_SEMICOLON {}
    | NonEmptyStatement {}
    | ScopeBlockStatement {}
    ;

NoScopeNonEmptyStatement
    : NonEmptyStatement {}
    | BlockStatement {}
    ;

NoScopeStatement
    : TOK_SEMICOLON {}
    | NonEmptyStatement {}
    | BlockStatement {}
    ;

NonEmptyOrScopeBlockStatement
    : NonEmptyStatement {}
    | ScopeBlockStatement {}
    ;

NonEmptyStatement
    : NonEmptyStatementNoCaseNoDefault {}
    | CaseStatement {}
    | CaseRangeStatement {}
    | DefaultStatement {}
    ;

NonEmptyStatementNoCaseNoDefault
    : LabeledStatement {}
    | ExpressionStatement {}
    | DeclarationStatement {}
    | IfStatement {}
    | WhileStatement {}
    | DoStatement {}
    | ForStatement {}
    | ForeachStatement {}
    | SwitchStatement {}
    | FinalSwitchStatement {}
    | ContinueStatement {}
    | BreakStatement {}
    | ReturnStatement {}
    | GotoStatement {}
    | WithStatement {}
    | SynchronizedStatement {}
    | TryStatement {}
    | ScopeGuardStatement {}
    | ThrowStatement {}
/*    | AsmStatement {} */ /* TODO */
    | PragmaStatement {}
    | MixinStatement {}
    | ForeachRangeStatement {}
    /*| ConditionalStatement {}*/ /* TODO */
    /*| StaticAssert {}*/ /* TODO */
    | TemplateMixin {}
    | ImportDeclaration {}
    ;

ScopeStatement
    : NonEmptyStatement {}
    | BlockStatement {}
    ;

ScopeBlockStatement
    : BlockStatement {}
    ;


LabeledStatement
    : TOK_IDENTIFIER TOK_COLON {}
    | TOK_IDENTIFIER TOK_COLON NoScopeStatement {}
    | TOK_IDENTIFIER TOK_COLON Statement
    ;

BlockStatement
    : TOK_LEFT_BRACE TOK_RIGHT_BRACE {}
    | TOK_LEFT_BRACE StatementList TOK_RIGHT_BRACE {}
    ;

StatementList
    : Statement {}
    | Statement StatementList {}
    ;

ExpressionStatement
    : Expression TOK_SEMICOLON {}
    ;

DeclarationStatement
    : Declaration {}
    ;

IfStatement
    : TOK_IF TOK_LEFT_PAR IfCondition TOK_RIGHT_PAR ThenStatement {}
    | TOK_IF TOK_LEFT_PAR IfCondition TOK_RIGHT_PAR ThenStatement TOK_ELSE ElseStatement {}
    ;

IfCondition
    : Expression {}
    | TOK_AUTO TOK_IDENTIFIER TOK_ASSIGN Expression {}
    | BasicType Declarator TOK_ASSIGN Expression {}
    ;

ThenStatement
    : ScopeStatement {}
    ;

ElseStatement
    : ScopeStatement {}
    ;

WhileStatement
    : TOK_WHILE TOK_LEFT_PAR Expression TOK_RIGHT_PAR ScopeStatement {}
    ;

DoStatement
    : TOK_DO ScopeStatement TOK_WHILE TOK_LEFT_PAR Expression TOK_RIGHT_PAR TOK_SEMICOLON {}
    ;

ForStatement
    : TOK_FOR TOK_LEFT_PAR Initialize Testopt TOK_SEMICOLON Incrementopt TOK_RIGHT_PAR ScopeStatement {}
    ;

Initialize
    : TOK_SEMICOLON {}
    | NoScopeNonEmptyStatement {}
    ;

Testopt
    : {}
    | Testopt {}
    ;

Test
    : Expression {}
    ;

Incrementopt
    : {}
    | Increment {}
    ;

Increment
    : Expression {}
    ;

ForeachStatement
    : Foreach TOK_LEFT_PAR ForeachTypeList TOK_SEMICOLON Aggregate TOK_RIGHT_PAR NoScopeNonEmptyStatement {}
    ;

Foreach
    : TOK_FOREACH {}
    | TOK_FOREACH_REVERSE {}
    ;

ForeachTypeList
    : ForeachType {}
    | ForeachType TOK_COMMA ForeachTypeList {}
    ;

Refopt
    : {}
    | TOK_REF {}
    ;

ForeachType
    : Refopt BasicType Declarator {}
    Refopt TOK_IDENTIFIER {}
    ;

Aggregate
    : Expression {}
    ;

ForeachRangeStatement
    : Foreach TOK_LEFT_PAR ForeachType TOK_SEMICOLON LwrExpression TOK_DOUBLE_DOT UprExpression TOK_RIGHT_PAR ScopeStatement {}
    ;

LwrExpression
    : Expression {}
    ;

UprExpression
    : Expression {}
    ;

SwitchStatement
    : TOK_SWITCH TOK_LEFT_PAR Expression TOK_RIGHT_PAR ScopeStatement {}
    ;

CaseStatement
    : TOK_CASE ArgumentList TOK_COLON ScopeStatementList {}
    ;

CaseRangeStatement
    : TOK_CASE FirstExp TOK_COLON TOK_DOUBLE_DOT TOK_CASE LastExp TOK_COLON ScopeStatementList {}
    ;

FirstExp
    : AssignExpression {}
    ;

LastExp
    : AssignExpression {}
    ;

DefaultStatement
    : TOK_DEFAULT TOK_COLON ScopeStatementList {}
    ;

ScopeStatementList
    : StatementListNoCaseNoDefault {}
    ;

StatementListNoCaseNoDefault
    : StatementNoCaseNoDefault {}
    | StatementNoCaseNoDefault StatementListNoCaseNoDefault {}
    ;

StatementNoCaseNoDefault
    : TOK_SEMICOLON {}
    | NonEmptyStatementNoCaseNoDefault {}
    | ScopeBlockStatement {}
    ;

FinalSwitchStatement
    : TOK_FINAL TOK_SWITCH TOK_LEFT_PAR Expression TOK_RIGHT_PAR ScopeStatement {}
    ;


Identifieropt
    : TOK_IDENTIFIER {}
    | {}
    ;

Identifier
    : TOK_IDENTIFIER {}
    ;

ContinueStatement
    : TOK_CONTINUE Identifieropt TOK_SEMICOLON {}
    ;

BreakStatement
    : TOK_BREAK Identifieropt TOK_SEMICOLON {}
    ;

ReturnStatement
    : TOK_RETURN Expressionopt TOK_SEMICOLON {}
    ;

GotoStatement
    : TOK_GOTO Identifier TOK_SEMICOLON {}
    | TOK_GOTO TOK_DEFAULT TOK_SEMICOLON
    | TOK_GOTO TOK_CASE TOK_SEMICOLON
    | TOK_GOTO TOK_CASE Expression TOK_SEMICOLON {}
    ;

WithStatement
    : TOK_WITH TOK_LEFT_PAR Expression TOK_RIGHT_PAR ScopeStatement {}
    | TOK_WITH TOK_LEFT_PAR Symbol TOK_RIGHT_PAR ScopeStatement {}
    | TOK_WITH TOK_LEFT_PAR TemplateInstance TOK_RIGHT_PAR ScopeStatement {}
    ;

SynchronizedStatement
    : TOK_SYNCHRONIZED ScopeStatement {}
    | TOK_SYNCHRONIZED TOK_LEFT_PAR Expression TOK_RIGHT_PAR ScopeStatement {}
    ;

TryStatement
    : TOK_TRY ScopeStatement Catches {}
    | TOK_TRY ScopeStatement Catches FinallyStatement {}
    | TOK_TRY ScopeStatement FinallyStatement {}
    ;

Catches
    : LastCatch {}
    | Catch {}
    | Catch Catches {}
    ;

LastCatch
    : TOK_CATCH NoScopeNonEmptyStatement {}
    ;

Catch
    : TOK_CATCH TOK_LEFT_PAR CatchParameter TOK_RIGHT_PAR NoScopeNonEmptyStatement {}
    ;

CatchParameter
    : BasicType Identifier {}
    ;

FinallyStatement
    : TOK_FINALLY NoScopeNonEmptyStatement {}
    ;

ThrowStatement
    : TOK_THROW Expression TOK_SEMICOLON
    ;

ScopeGuardStatement
    : TOK_SCOPE_EXIT NonEmptyOrScopeBlockStatement {}
    | TOK_SCOPE_SUCCESS NonEmptyOrScopeBlockStatement {}
    | TOK_SCOPE_FAILURE NonEmptyOrScopeBlockStatement {}
    ;

/*AsmStatement
    : TOK_ASM TOK_LEFT_BRACE TOK_RIGHT_BRACE {}
    | TOK_ASM TOK_LEFT_BRACE AsmInstructionList TOK_RIGHT_BRACE {}
    ;

AsmInstructionList
    : AsmInstruction TOK_SEMICOLON {}
    | AsmInstruction TOK_SEMICOLON AsmInstructionList {}*/

PragmaStatement
    : Pragma NoScopeStatement {}
    ;

MixinStatement
    : TOK_MIXIN TOK_LEFT_PAR AssignExpression TOK_RIGHT_PAR TOK_SEMICOLON {}
    ;


/* ***** Structs & Unions ***** */

AggregateDeclaration
    : TOK_STRUCT Identifier StructBody {}
    | TOK_UNION Identifier StructBody {}
    | TOK_STRUCT Identifier TOK_SEMICOLON {}
    | TOK_UNION Identifier TOK_SEMICOLON {}
    | StructTemplateDeclaration {}
    | UnionTemplateDeclaration {}
    ;

StructBody
    : TOK_LEFT_BRACE TOK_RIGHT_BRACE {}
    | TOK_LEFT_BRACE StructBodyDeclarations TOK_RIGHT_BRACE {}
    ;

StructBodyDeclarations
    : StructBodyDeclaration {}
    | StructBodyDeclaration StructBodyDeclarations {}
    ;

StructBodyDeclaration
    : DeclDef {}
    | StructAllocator {}
    | StructDeallocator {}
    | StructPostblit {}
    | AliasThis {}
    ;

StructAllocator
    : ClassAllocator {}
    ;

StructDeallocator
    : ClassDeallocator {}
    ;

StructPostblit
    : TOK_THIS TOK_LEFT_PAR TOK_THIS TOK_RIGHT_PAR MemberFunctionAttributesopt FunctionBody {}
    ;


/* ***** Classes ***** */

ClassDeclaration
    : TOK_CLASS Identifier BaseClassListopt ClassBody {}
    | ClassTemplateDeclaration {}
    ;

BaseClassListopt
    : {}
    | BaseClassList {}
    ;

BaseClassList
    : TOK_COLON SuperClass {}
    | TOK_COLON SuperClass TOK_COMMA Interfaces {}
    | TOK_COLON Interfaces {}
    ;

SuperClassopt
    : {}
    | SuperClass {}
    ;

SuperClass
    : Identifier {}
    ;

Interfacesopt
    : {}
    | Interfaces {}
    ;

Interfaces
    : Interface {}
    | Interface TOK_COMMA Interfaces
    ;

Interface
    : Identifier {}
    ;

ClassBody
    : TOK_LEFT_BRACE TOK_RIGHT_BRACE {}
    | TOK_LEFT_BRACE ClassBodyDeclarations TOK_RIGHT_BRACE {}
    ;

ClassBodyDeclarations
    : ClassBodyDeclaration {}
    | ClassBodyDeclaration ClassBodyDeclarations {}
    ;

ClassBodyDeclaration
    : DeclDef {}
    | Invariant {}
    | ClassAllocator {}
    | ClassDeallocator {}
    ;

Constructor
    : TOK_THIS Parameters FunctionBody {}
    | TemplatedConstructor {}
    ;

Destructor
    : TOK_TILDA TOK_THIS TOK_LEFT_PAR TOK_RIGHT_PAR MemberFunctionAttributesopt FunctionBody {}
    ;

StaticConstructor
    : TOK_STATIC TOK_THIS TOK_LEFT_PAR TOK_RIGHT_PAR FunctionBody {}
    ;

StaticDestructor
    : TOK_STATIC TOK_TILDA TOK_THIS TOK_LEFT_PAR TOK_RIGHT_PAR FunctionBody {}
    ;

SharedStaticConstructor
    : TOK_SHARED TOK_STATIC TOK_THIS TOK_LEFT_PAR TOK_RIGHT_PAR FunctionBody {}
    ;

SharedStaticDestructor
    : TOK_SHARED TOK_STATIC TOK_TILDA TOK_THIS TOK_LEFT_PAR TOK_RIGHT_PAR FunctionBody {}
    ;

Invariant
    : TOK_INVARIANT TOK_LEFT_PAR TOK_RIGHT_PAR BlockStatement {}
    ;

ClassAllocator
    : TOK_NEW Parameters FunctionBody {}
    ;

ClassDeallocator
    : TOK_DELETE Parameters FunctionBody {}
    ;

AliasThis
    : TOK_ALIAS Identifier TOK_THIS TOK_SEMICOLON {}
    ;

NewAnonClassExpression
    : TOK_NEW AllocatorArgumentsopt TOK_CLASS ClassArgumentsopt SuperClassopt Interfacesopt {}
    | ClassBody {}
    ;

ClassArgumentsopt
    : {}
    | ClassArguments {}
    ;

ClassArguments
    : TOK_LEFT_PAR ArgumentListopt TOK_RIGHT_PAR {}
    ;

/* ***** Interfaces ***** */

InterfaceDeclaration
    : TOK_INTERFACE Identifier BaseInterfaceListopt InterfaceBody {}
    | InterfaceTemplateDeclaration {}
    ;

BaseInterfaceListopt
    : {}
    | BaseInterfaceList {}
    ;

BaseInterfaceList
    : TOK_COLON Interfaces {} /* FIXED InterfaceClasses */
    ;

InterfaceBody
    : TOK_LEFT_BRACE DeclDefsopt TOK_RIGHT_BRACE {}
    ;

/* ***** Enums ***** */

EnumDeclaration
    : TOK_ENUM EnumTag EnumBody {}
    | TOK_ENUM EnumBody {}
    | TOK_ENUM EnumTag TOK_COLON EnumBaseType EnumBody {}
    | TOK_ENUM TOK_COLON EnumBaseType EnumBody {}
    ;

EnumTag
    : Identifier {}
    ;

EnumBaseType
    : Type {}
    ;

EnumBody
    : EmptyEnumBody {}
    | EnumMembersBody {}
    ;

EmptyEnumBody
    : TOK_SEMICOLON {}
    ;

EnumMembersBody
    : TOK_LEFT_BRACE EnumMembers TOK_RIGHT_BRACE {}
    ;

EnumMembers
    : EnumMember {}
    | EnumMember TOK_COMMA {}
    | EnumMember TOK_COMMA EnumMembers {}
    ;

EnumMember
    : Identifier {}
    | Identifier TOK_ASSIGN AssignExpression {}
    | Type Identifier TOK_ASSIGN AssignExpression {}
    ;


/* ***** Functions ***** */

FunctionBody
    : BlockStatement {}
    | BodyStatement {}
    | InStatement BodyStatement {}
    | OutStatement BodyStatement {}
    | InStatement OutStatement BodyStatement {}
    | OutStatement InStatement BodyStatement {}
    ;

InStatement
    : TOK_IN BlockStatement {}
    ;

OutStatement
    : TOK_OUT BlockStatement {}
    | TOK_OUT TOK_LEFT_PAR TOK_IDENTIFIER TOK_RIGHT_PAR BlockStatement {}
    ;

BodyStatement
    : TOK_BODY BlockStatement {}
    ;


/* ***** Templates ***** */

TemplateDeclaration
    : TOK_TEMPLATE TemplateIdentifier TemplateParameters Constraintopt {}
    | TOK_LEFT_BRACE DeclDefs TOK_RIGHT_BRACE {}
    ;

TemplateIdentifier
    : Identifier {}
    ;

TemplateParametersopt
    : {}
    | TemplateParameters {}
    ;

TemplateParameters
    : TOK_LEFT_PAR TOK_RIGHT_PAR {}
    TOK_LEFT_PAR TemplateParameterList TOK_RIGHT_PAR {}
    ;

TemplateParameterList
    : TemplateParameter {}
    | TemplateParameter TOK_COMMA {}
    | TemplateParameter TOK_COMMA TemplateParameterList {}
    ;

TemplateParameter
    : TemplateTypeParameter {}
    | TemplateValueParameter {}
    | TemplateAliasParameter {}
    | TemplateTupleParameter {}
    | TemplateThisParameter {}
    ;

TemplateInstance
    : TemplateIdentifier TemplateArguments {}
    ;

TemplateArgumentsopt
    : {}
    | TemplateArguments {}
    ;

TemplateArguments
    : TOK_EXCLAMATION TOK_LEFT_PAR TOK_RIGHT_PAR {}
    | TOK_EXCLAMATION TOK_LEFT_PAR TemplateArgumentList TOK_RIGHT_PAR {}
    | TOK_EXCLAMATION TemplateSingleArgument {}
    ;

TemplateArgumentList
    : TemplateArgument {}
    | TemplateArgument TOK_COMMA {}
    | TemplateArgument TOK_COMMA TemplateArgumentList {}
    ;

TemplateArgument
    : Type {}
    | AssignExpression {}
    | Symbol {}
    ;

Symbol
    : SymbolTail {}
    | TOK_DOT SymbolTail {}
    ;

SymbolTail
    : Identifier {}
    | Identifier TOK_DOT SymbolTail {}
    | TemplateInstance {}
    | TemplateInstance TOK_DOT SymbolTail {}
    ;

TemplateSingleArgument
    : Identifier {}
    | BasicTypeX {}
    | TOK_CHAR_LITERAL {}
    | TOK_STRING_LITERAL {}
    | TOK_INT_CONSTANT {}
    | TOK_REAL_CONSTANT {}
    | TOK_TRUE {}
    | TOK_FALSE {}
    | TOK_NULL {}
    | TOK_THIS {}
    | TOK__FILE {}
    | TOK__MODULE {}
    | TOK__LINE {}
    | TOK__FUNCTION {}
    | TOK__PRETTY_FUNCTION {}
    ;

TemplateTypeParameter
    : Identifier {}
    | Identifier TemplateTypeParameterSpecialization {}
    | Identifier TemplateTypeParameterDefault {}
    | Identifier TemplateTypeParameterSpecialization TemplateTypeParameterDefault {}
    ;

TemplateTypeParameterSpecialization
    : TOK_COLON Type {}
    ;

TemplateTypeParameterDefault
    : TOK_ASSIGN Type {}
    ;

TemplateThisParameter
    : TOK_THIS TemplateTypeParameter {}
    ;

TemplateValueParameter
    : BasicType Declarator {}
    | BasicType Declarator TemplateValueParameterSpecialization {}
    | BasicType Declarator TemplateValueParameterDefault {}
    | BasicType Declarator TemplateValueParameterSpecialization TemplateValueParameterDefault {}
    ;

TemplateValueParameterSpecialization
    : TOK_COLON ConditionalExpression {}
    ;

TemplateValueParameterDefault
    : TOK_ASSIGN TOK__FILE {}
    | TOK_ASSIGN TOK__MODULE {}
    | TOK_ASSIGN TOK__LINE {}
    | TOK_ASSIGN TOK__FUNCTION {}
    | TOK_ASSIGN TOK__PRETTY_FUNCTION {}
    | TOK_ASSIGN AssignExpression {}
    ;

TemplateAliasParameter
    : TOK_ALIAS Identifier TemplateAliasParameterSpecializationopt TemplateAliasParameterDefaultopt {}
    | TOK_ALIAS BasicType Declarator TemplateAliasParameterSpecializationopt TemplateAliasParameterDefaultopt {}
    ;

TemplateAliasParameterSpecializationopt
    : {}
    | TemplateAliasParameterSpecialization {}
    ;

TemplateAliasParameterSpecialization
    : TOK_COLON Type {}
    | TOK_COLON ConditionalExpression {}
    ;

TemplateAliasParameterDefaultopt
    : {}
    | TemplateAliasParameterDefault {}
    ;

TemplateAliasParameterDefault
    : TOK_ASSIGN Type {}
    | TOK_ASSIGN ConditionalExpression {}
    ;

TemplateTupleParameter
    : Identifier TOK_ELLIPSIS {}
    ;

TemplatedConstructor
    : TOK_THIS TemplateParameters Parameters Constraintopt FunctionBody {}
    ;

ClassTemplateDeclaration
    : TOK_CLASS Identifier TOK_LEFT_PAR TemplateParameterList TOK_RIGHT_PAR Constraintopt BaseClassListopt ClassBody {}
    | TOK_CLASS Identifier TOK_LEFT_PAR TemplateParameterList TOK_RIGHT_PAR BaseClassListopt Constraintopt ClassBody {}
    ;

StructTemplateDeclaration
    : TOK_STRUCT Identifier TOK_LEFT_PAR TemplateParameterList TOK_RIGHT_PAR Constraintopt StructBody {}
    ;

UnionTemplateDeclaration
    : TOK_UNION Identifier TOK_LEFT_PAR TemplateParameterList TOK_RIGHT_PAR Constraintopt StructBody {}
    ;

InterfaceTemplateDeclaration
    : TOK_INTERFACE Identifier TOK_LEFT_PAR TemplateParameterList TOK_RIGHT_PAR Constraintopt BaseInterfaceListopt InterfaceBody {}
    ;

Constraint
    : TOK_IF TOK_LEFT_PAR ConstraintExpression TOK_RIGHT_PAR {}
    ;

ConstraintExpression
    : Expression {}
    ;

/* ***** Template Mixins ***** */

TemplateMixinDeclaration
    : TOK_MIXIN TOK_TEMPLATE TemplateIdentifier TemplateParameters Constraintopt {}
    | TOK_LEFT_BRACE DeclDefs TOK_RIGHT_BRACE {}
    ;

TemplateMixin
    : TOK_MIXIN MixinTemplateName TemplateArgumentsopt MixinIdentifieropt TOK_SEMICOLON {}
    ;

MixinTemplateName
    : TOK_DOT QualifiedIdentifierList {}
    | QualifiedIdentifierList {}
    | Typeof TOK_DOT QualifiedIdentifierList {}
    ;

QualifiedIdentifierList
    : Identifier {}
    | Identifier TOK_DOT QualifiedIdentifierList {}
    | TemplateInstance TOK_DOT QualifiedIdentifierList {}
    ;

MixinIdentifieropt
    : {}
    | MixinIdentifier {}
    ;

MixinIdentifier
    : Identifier {}
    ;

MixinDeclaration
    : TOK_MIXIN TOK_LEFT_PAR AssignExpression TOK_RIGHT_PAR TOK_SEMICOLON {}
    ;


/* ***** Conditional Compilation ***** */

/* TODO !!! */

/* ***** Traits ***** */

/* TODO !!! */

/* ***** Unit Tests ***** */

UnitTest
    : TOK_UNITTEST FunctionBody {}
    ;

/* ***** Inline Assembler ***** */

/* TODO !!! */







%%
