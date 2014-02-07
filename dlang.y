%{
#include "dlang_grm.hpp"
#include "dlang_lex.h"
#include "dast.h"

int yyerror(YYLTYPE* loc, ModuleNode **mainnode, yyscan_t scanner, const char *msg) {
    std::cout << "Error[" << loc->first_line << ": " << msg << std::endl;
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
%parse-param { ModuleNode **mainnode }
%parse-param { yyscan_t scanner }
%locations

%union {
    int ivalue;
    double rvalue;
    const char* str;
    Node *node;
    ModuleNode* module;
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

%type <module> Module

%type <node> Identifier
%type <node> Identifieropt
%type <node> IdentifierList

%type <node> ModuleDeclaration
%type <node> ModuleFullyQualifiedName
%type <node> ModuleName
%type <node> Packages
%type <node> PackageName
%type <node> DeclDef
%type <node> DeclDefsopt
%type <node> DeclDefs
%type <node> ImportDeclaration
%type <node> ImportList
%type <node> Import

%type <node> Declaration
%type <node> AliasDeclaration /* TODO */
%type <node> AliasThisDeclaration /* TODO */
%type <node> Decl /* TODO */

%type <node> ClassDeclaration
%type <node> ClassBody
%type <node> ClassTemplateDeclaration /* TODO */
%type <node> ClassBodyDeclarations
%type <node> ClassBodyDeclaration
%type <node> Constructor
%type <node> Parameters
%type <node> Parameter
%type <node> ParameterList
%type <node> FunctionBody

%type <node> BasicTypeX
%type <node> BasicType
%type <node> Type
%type <node> Declarator


/* NOT IMPLEMENTED LIST */
%type <node> ImportBindings
%type <node> ImportBindList
%type <node> ImportBind
%type <node> ModuleAliasIdentifier
%type <node> AliasInitializerList
%type <node> AliasInitializer
%type <node> Declarators
%type <node> DeclaratorInitializer
%type <node> DeclaratorIdentifierList
%type <node> DeclaratorIdentifier
%type <node> BasicType2opt
%type <node> BasicType2
%type <node> DeclaratorSuffixesopt
%type <node> DeclaratorSuffixes
%type <node> Constraintopt
%type <node> DeclaratorSuffix
%type <node> StorageClasses
%type <node> StorageClass
%type <node> TypeCtors
%type <node> TypeCtorsopt
%type <node> TypeCtor
%type <node> Typeopt
%type <node> Declarator2
%type <node> InOutopt
%type <node> InOut
%type <node> InOutX
%type <node> FunctionAttributesopt
%type <node> FunctionAttributes
%type <node> FunctionAttribute
%type <node> MemberFunctionAttributesopt
%type <node> MemberFunctionAttributes
%type <node> MemberFunctionAttribute
%type <node> DefaultInitializerExpression
%type <node> Initializer
%type <node> NonVoidInitializer
%type <node> ArrayInitializer
%type <node> ArrayMemberInitializations
%type <node> ArrayMemberInitialization
%type <node> StructInitializer
%type <node> StructMemberInitializers
%type <node> StructMemberInitializer
%type <node> AutoDeclaration
%type <node> AutoDeclarationX
%type <node> Typeof
%type <node> VoidInitializer
%type <node> AttributeSpecifier
%type <node> Attribute
%type <node> Property
%type <node> PropertyIdentifier
%type <node> DeclarationBlock
%type <node> LinkageAttribute
%type <node> LinkageType
%type <node> AlignAttribute
%type <node> DeprecatedAttribute
%type <node> ProtectionAttribute
%type <node> UserDefinedAttribute
%type <node> Pragma
%type <node> Expressionopt
%type <node> Expression
%type <node> CommaExpression
%type <node> AssignExpression
%type <node> ConditionalExpression
%type <node> OrOrExpression
%type <node> AndAndExpression
%type <node> OrExpression
%type <node> XorExpression
%type <node> AndExpression
%type <node> CmpExpression
%type <node> EqualExpression
%type <node> IdentityExpression
%type <node> RelExpression
%type <node> InExpression
%type <node> ShiftExpression
%type <node> AddExpression
%type <node> CatExpression
%type <node> MulExpression
%type <node> UnaryExpression
%type <node> ComplementExpression
%type <node> NewExpression
%type <node> NewExpressionWithArgs
%type <node> AllocatorArgumentsopt
%type <node> AllocatorArguments
%type <node> ArgumentListopt
%type <node> ArgumentList
%type <node> DeleteExpression
%type <node> CastExpression
%type <node> CastQual
%type <node> PowExpression
%type <node> PostfixExpression
%type <node> IndexExpression
%type <node> SliceExpression
%type <node> PrimaryExpression
%type <node> StringLiterals
%type <node> ArrayLiteral
%type <node> AssocArrayLiteral
%type <node> KeyValuePairs
%type <node> KeyValuePair
%type <node> KeyExpression
%type <node> ValueExpression
%type <node> Lambda
%type <node> FunctionLiteral
%type <node> ParameterAttributesopt
%type <node> ParameterAttributes
%type <node> AssertExpression
%type <node> MixinExpression
%type <node> ImportExpression
%type <node> TypeidExpression
%type <node> IsExpression
%type <node> TypeSpecialization
%type <node> Statement
%type <node> NoScopeNonEmptyStatement
%type <node> NoScopeStatement
%type <node> NonEmptyOrScopeBlockStatement
%type <node> NonEmptyStatement
%type <node> NonEmptyStatementNoCaseNoDefault
%type <node> ScopeStatement
%type <node> ScopeBlockStatement
%type <node> LabeledStatement
%type <node> BlockStatement
%type <node> StatementList
%type <node> ExpressionStatement
%type <node> DeclarationStatement
%type <node> IfStatement
%type <node> IfCondition
%type <node> ThenStatement
%type <node> ElseStatement
%type <node> WhileStatement
%type <node> DoStatement
%type <node> ForStatement
%type <node> Initialize
%type <node> Testopt
%type <node> Test
%type <node> Incrementopt
%type <node> Increment
%type <node> ForeachStatement
%type <node> Foreach
%type <node> ForeachTypeList
%type <node> Refopt
%type <node> ForeachType
%type <node> Aggregate
%type <node> ForeachRangeStatement
%type <node> LwrExpression
%type <node> UprExpression
%type <node> SwitchStatement
%type <node> CaseStatement
%type <node> CaseRangeStatement
%type <node> FirstExp
%type <node> LastExp
%type <node> DefaultStatement
%type <node> ScopeStatementList
%type <node> StatementListNoCaseNoDefault
%type <node> StatementNoCaseNoDefault
%type <node> FinalSwitchStatement
%type <node> ContinueStatement
%type <node> BreakStatement
%type <node> ReturnStatement
%type <node> GotoStatement
%type <node> WithStatement
%type <node> SynchronizedStatement
%type <node> TryStatement
%type <node> Catches
%type <node> LastCatch
%type <node> Catch
%type <node> CatchParameter
%type <node> FinallyStatement
%type <node> ThrowStatement
%type <node> ScopeGuardStatement
/*%type <node> AsmInstructionList*/
%type <node> PragmaStatement
%type <node> MixinStatement
%type <node> AggregateDeclaration
%type <node> StructBody
%type <node> StructBodyDeclarations
%type <node> StructBodyDeclaration
%type <node> StructAllocator
%type <node> StructDeallocator
%type <node> StructPostblit
%type <node> BaseClassListopt
%type <node> BaseClassList
%type <node> SuperClassopt
%type <node> SuperClass
%type <node> Interfacesopt
%type <node> Interfaces
%type <node> Interface
%type <node> Destructor
%type <node> StaticConstructor
%type <node> StaticDestructor
%type <node> SharedStaticConstructor
%type <node> SharedStaticDestructor
%type <node> Invariant
%type <node> ClassAllocator
%type <node> ClassDeallocator
%type <node> AliasThis
%type <node> NewAnonClassExpression
%type <node> ClassArgumentsopt
%type <node> ClassArguments
%type <node> InterfaceDeclaration
%type <node> BaseInterfaceListopt
%type <node> BaseInterfaceList
%type <node> InterfaceBody
%type <node> EnumDeclaration
%type <node> EnumTag
%type <node> EnumBaseType
%type <node> EnumBody
%type <node> EmptyEnumBody
%type <node> EnumMembersBody
%type <node> EnumMembers
%type <node> EnumMember
%type <node> InStatement
%type <node> OutStatement
%type <node> BodyStatement
%type <node> TemplateDeclaration
%type <node> TemplateIdentifier
%type <node> TemplateParametersopt
%type <node> TemplateParameters
%type <node> TemplateParameterList
%type <node> TemplateParameter
%type <node> TemplateInstance
%type <node> TemplateArgumentsopt
%type <node> TemplateArguments
%type <node> TemplateArgumentList
%type <node> TemplateArgument
%type <node> Symbol
%type <node> SymbolTail
%type <node> TemplateSingleArgument
%type <node> TemplateTypeParameter
%type <node> TemplateTypeParameterSpecialization
%type <node> TemplateTypeParameterDefault
%type <node> TemplateThisParameter
%type <node> TemplateValueParameter
%type <node> TemplateValueParameterSpecialization
%type <node> TemplateValueParameterDefault
%type <node> TemplateAliasParameter
%type <node> TemplateAliasParameterSpecializationopt
%type <node> TemplateAliasParameterSpecialization
%type <node> TemplateAliasParameterDefaultopt
%type <node> TemplateAliasParameterDefault
%type <node> TemplateTupleParameter
%type <node> TemplatedConstructor
%type <node> StructTemplateDeclaration
%type <node> UnionTemplateDeclaration
%type <node> InterfaceTemplateDeclaration
%type <node> Constraint
%type <node> ConstraintExpression
%type <node> TemplateMixinDeclaration
%type <node> TemplateMixin
%type <node> MixinTemplateName
%type <node> QualifiedIdentifierList
%type <node> MixinIdentifieropt
%type <node> MixinIdentifier
%type <node> MixinDeclaration
%type <node> UnitTest

%%

input
    : Module { *mainnode = $1; }
    | error { yyerrok; }
    ;

/* ***** Modules ***** */

Module
    : ModuleDeclaration DeclDefsopt { $$ = new ModuleNode($1->name()); delete $1; $$->addChild($2);}
    | DeclDefs { $$ = new ModuleNode(); if ($1) $$->addChild($1); }
    ;

DeclDefsopt
    : { $$ = 0; }
    | DeclDefs { $$ = $1; }
    ;

DeclDefs
    : DeclDef { $$ = new NodeList; $$->addChild($1); }
    | DeclDef DeclDefs { $2->addChild($1); $$ = $2; }
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
    : TOK_MODULE ModuleFullyQualifiedName TOK_SEMICOLON { $$ = $2; }
    ;
ModuleFullyQualifiedName
    : ModuleName { $$ = $1; }
    | Packages TOK_DOT ModuleName { $$ = new ModuleNode($1->name() + "." + $3->name()); delete $1; delete $3; }
    ;

ModuleName
    : Identifier { $$ = $1; }
    ;

Packages
    : PackageName { $$ = $1; }
    | Packages TOK_DOT PackageName { $$ = new IdentifierNode($1->name() + "." + $3->name()); delete $1; delete $3; }
    ;
PackageName
    : Identifier { $$ = $1; }
    ;

/* ***** Import Declaration ***** */

ImportDeclaration
    : TOK_IMPORT ImportList TOK_SEMICOLON { $$ = $2; }
    | TOK_STATIC TOK_IMPORT ImportList TOK_SEMICOLON { $$ = $3; }
    ;

ImportList
    : Import { $$ = new NodeList; $$->addChild($1);  }
    | ImportBindings { }
    | Import TOK_COMMA ImportList { $3->addChild($1); $$ = $3; }
    ;

Import
    : ModuleFullyQualifiedName { $$ = new ImportNode($$->name()); }
    | ModuleAliasIdentifier TOK_COLON ModuleFullyQualifiedName { $$ = new ImportNode($$->name()); }
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
    : AliasDeclaration { $$ = 0; } /* TODO */
    | AliasThisDeclaration { $$ = 0; } /* TODO */
    | Decl { $$ = $1; } /* TODO */
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
    : StorageClasses Decl { $$ = $2; }
    | BasicType Declarators TOK_SEMICOLON { $$ = new DeclarationTypedList($1); $$->addChild($2); }
    | BasicType Declarator FunctionBody { $$ = new DeclarationNode($1, $2); $$->addChild($3); }
    | AutoDeclaration { $$ = 0; } /* TODO */
    ;

Declarators
    : DeclaratorInitializer { $$ = new NodeList; $$->addChild($1); }
    | DeclaratorInitializer TOK_COMMA DeclaratorIdentifierList { $$ = $3; $$->addChild($1); }
    ;

DeclaratorInitializer
    : Declarator { $$ = $1; }
    | Declarator TOK_ASSIGN Initializer { $$ = $1; }
    ;

DeclaratorIdentifierList
    : DeclaratorIdentifier { $$ = new NodeList; $$->addChild($1); }
    | DeclaratorIdentifier TOK_COMMA DeclaratorIdentifierList { $$ = $3; $$->addChild($1); }
    ;

DeclaratorIdentifier
    : Identifier { $$ = $1; }
    | Identifier TOK_ASSIGN Initializer { $$ = $1; }
    ;

BasicType
    : BasicTypeX { $$ = $1; }
    | TOK_DOT IdentifierList { $$ = 0; } /* TODO */
    | IdentifierList { $$ = $1; }
    | Typeof { $$ = 0; }  /* TODO */
    | Typeof TOK_DOT IdentifierList {}  /* TODO */
    | TOK_CONST TOK_LEFT_PAR Type TOK_RIGHT_PAR { $$ = $3; }
    | TOK_IMMUTABLE TOK_LEFT_PAR Type TOK_RIGHT_PAR { $$ = $3; }
    | TOK_SHARED TOK_LEFT_PAR Type TOK_RIGHT_PAR { $$ = $3; }
    | TOK_INOUT TOK_LEFT_PAR Type TOK_RIGHT_PAR { $$ = $3; }
    ;

BasicTypeX
    : TOK_BOOL { $$ = new PrimitiveTypeNode(PrimitiveTypeNode::BOOL); }
    | TOK_BYTE { $$ = new PrimitiveTypeNode(PrimitiveTypeNode::BYTE); }
    | TOK_UBYTE { $$ = new PrimitiveTypeNode(PrimitiveTypeNode::UBYTE); }
    | TOK_SHORT { $$ = new PrimitiveTypeNode(PrimitiveTypeNode::SHORT); }
    | TOK_USHORT { $$ = new PrimitiveTypeNode(PrimitiveTypeNode::USHORT); }
    | TOK_INT { $$ = new PrimitiveTypeNode(PrimitiveTypeNode::INT); }
    | TOK_UINT { $$ = new PrimitiveTypeNode(PrimitiveTypeNode::UINT); }
    | TOK_LONG { $$ = new PrimitiveTypeNode(PrimitiveTypeNode::LONG); }
    | TOK_ULONG { $$ = new PrimitiveTypeNode(PrimitiveTypeNode::ULONG); }
    | TOK_CHAR { $$ = new PrimitiveTypeNode(PrimitiveTypeNode::CHAR); }
    | TOK_WCHAR { $$ = new PrimitiveTypeNode(PrimitiveTypeNode::WCHAR); }
    | TOK_DCHAR { $$ = new PrimitiveTypeNode(PrimitiveTypeNode::DCHAR); }
    | TOK_FLOAT { $$ = new PrimitiveTypeNode(PrimitiveTypeNode::FLOAT); }
    | TOK_DOUBLE { $$ = new PrimitiveTypeNode(PrimitiveTypeNode::DOUBLE); }
    | TOK_REAL { $$ = new PrimitiveTypeNode(PrimitiveTypeNode::REAL); }
    | TOK_IFLOAT { $$ = new PrimitiveTypeNode(PrimitiveTypeNode::IFLOAT); }
    | TOK_IDOUBLE { $$ = new PrimitiveTypeNode(PrimitiveTypeNode::IDOUBLE); }
    | TOK_IREAL { $$ = new PrimitiveTypeNode(PrimitiveTypeNode::IREAL); }
    | TOK_CFLOAT { $$ = new PrimitiveTypeNode(PrimitiveTypeNode::CFLOAT); }
    | TOK_CDOUBLE { $$ = new PrimitiveTypeNode(PrimitiveTypeNode::CDOUBLE); }
    | TOK_CREAL { $$ = new PrimitiveTypeNode(PrimitiveTypeNode::CREAL); }
    | TOK_VOID { $$ = new PrimitiveTypeNode(PrimitiveTypeNode::VOID); }
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
    : BasicType2opt TOK_LEFT_PAR Declarator TOK_RIGHT_PAR DeclaratorSuffixesopt { $$ = $3; }
    | BasicType2opt Identifier DeclaratorSuffixesopt { $$ = $2; }
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
    : Identifier { $$ = new NodeList; $$->addChild($1); }
    | Identifier TOK_DOT IdentifierList { $$ = $3; $$->addChild($1); }
    | TemplateInstance { $$ = 0; } /* TODO */
    | TemplateInstance TOK_DOT IdentifierList { $$ = 0; } /* TODO */
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
    : TypeCtorsopt BasicType { $$ = $2; }
    | TypeCtorsopt BasicType Declarator2 { $$ = $2; }
    ;

Declarator2
    : BasicType2opt DeclaratorSuffixesopt {}
    | BasicType2opt TOK_LEFT_PAR Declarator2 TOK_RIGHT_PAR DeclaratorSuffixesopt {}
    ;

Parameters
    : TOK_LEFT_PAR ParameterList TOK_RIGHT_PAR { $$ = $2; }
    | TOK_LEFT_PAR TOK_RIGHT_PAR { $$ = 0; }
    ;

ParameterList
    : Parameter { $$ = new NodeList; $$->addChild($1); }
    | Parameter TOK_COMMA ParameterList { $$ = $3; $$->addChild($1); }
    | TOK_ELLIPSIS { $$ = 0; } /* TODO */
    ;

Parameter
    : InOutopt BasicType Declarator { $$ = new DeclarationNode($2, $3);}
    | InOutopt BasicType Declarator TOK_ELLIPSIS { $$ = new DeclarationNode($2, $3); }
    | InOutopt BasicType Declarator TOK_ASSIGN DefaultInitializerExpression { $$ = new DeclarationNode($2, $3); }
    | InOutopt Type { $$ = new DeclarationNode($2, 0); }
    | InOutopt Type TOK_ELLIPSIS { $$ = 0; } /* TODO */
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
    | TOK_IDENTIFIER TOK_COLON Statement {}
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
    : Identifier { $$ = $1; }
    | { $$ = 0; }
    ;

Identifier
    : TOK_IDENTIFIER { $$ = new IdentifierNode($1); }
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
    | TOK_GOTO TOK_DEFAULT TOK_SEMICOLON {}
    | TOK_GOTO TOK_CASE TOK_SEMICOLON {}
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
    : TOK_THROW Expression TOK_SEMICOLON {}
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
    : TOK_CLASS Identifier BaseClassListopt ClassBody { $$ = new ClassNode($2->name()); delete $2; $$->addChild($4); $$->setPosition(@4.first_line, @4.last_line);}
    | ClassTemplateDeclaration { $$ = $1; }
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
    : TOK_LEFT_BRACE TOK_RIGHT_BRACE { $$ = 0; }
    | TOK_LEFT_BRACE ClassBodyDeclarations TOK_RIGHT_BRACE { $$ = $2; }
    ;

ClassBodyDeclarations
    : ClassBodyDeclaration { $$ = new NodeList; $$->addChild($1); }
    | ClassBodyDeclaration ClassBodyDeclarations { $$ = $2; $$->addChild($1); }
    ;

ClassBodyDeclaration
    : DeclDef { $$ = $1; }
    | Invariant { $$ = 0; } /* TODO */
    | ClassAllocator { $$ = 0; } /* TODO */
    | ClassDeallocator { $$ = 0; } /* TODO */
    ;

Constructor
    : TOK_THIS Parameters FunctionBody { $$ = new ConstructorNode($2); $$->addChild($3); $$->setPosition(@3.first_line, @3.last_line); }
    | TemplatedConstructor {$$ = 0;} /* TODO */
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
    : BlockStatement { $$ = 0;} /* TODO */
    | BodyStatement { $$ = 0; } /* TODO */
    | InStatement BodyStatement { $$ = 0;} /* TODO */
    | OutStatement BodyStatement { $$ = 0; } /* TODO */
    | InStatement OutStatement BodyStatement { $$ = 0;} /* TODO */
    | OutStatement InStatement BodyStatement { $$ = 0;} /* TODO */
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
