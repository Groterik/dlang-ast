%{
#include "dlang_grm.hpp"
#include "dlang_lex.h"
#include "dast.h"

#undef YYDEBUG
#define YYDEBUG 1

int yyerror(YYLTYPE* loc, ModuleNode **mainnode, yyscan_t scanner, const char *msg) {
    std::cout << "Error:" << loc->first_line << ": " << msg << std::endl;
    exit(1);
}

%}

%code requires {

#include "dast.h"
#include <iostream>

#define TRACE std::clog << "TRACE:\t" << __FILE__ << ' ' << __LINE__ << '\n'

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

%glr-parser

%union {
    int ivalue;
    double rvalue;
    const char* str;
    Node *node;
    ModuleNode* module;
    DeclarationNode* decl;
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
    : AttributeSpecifier { $$ = 0; } /* TODO */
    | ImportDeclaration { $$ = $1; }
    | EnumDeclaration { $$ = 0; } /* TODO */
    | ClassDeclaration { $$ = $1; }
    | InterfaceDeclaration { $$ = 0; } /* TODO */
    | AggregateDeclaration { $$ = 0; } /* TODO */
    | Declaration { $$ = $1; }
    | Constructor { $$ = $1; }
    | Destructor { $$ = $1; }
    | UnitTest { $$ = 0; } /* TODO */
    | StaticConstructor { $$ = 0; } /* TODO */
    | StaticDestructor { $$ = 0; } /* TODO */
    | SharedStaticConstructor { $$ = 0; } /* TODO */
    | SharedStaticDestructor { $$ = 0; } /* TODO */
/*    | ConditionalDeclaration { $$ = 0; } */ /* TODO */
/*    | DebugSpecification { $$ = 0; } */ /* TODO */
  /*  | VersionSpecification { $$ = 0; } */ /* TODO */
    /*| StaticAssert { $$ = 0; } */
    | TemplateDeclaration { $$ = 0; } /* TODO */
    | TemplateMixinDeclaration { $$ = 0; } /* TODO */
    | TemplateMixin { $$ = 0; } /* TODO */
    | MixinDeclaration { $$ = 0; } /* TODO */
    | TOK_SEMICOLON { $$ = 0; } /* TODO */
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
    : TOK_IDENTIFIER TOK_ASSIGN Type { $$ = 0; } /* TODO */
    ;

AliasThisDeclaration
    : TOK_ALIAS TOK_IDENTIFIER TOK_THIS { $$ = 0; } /* TODO */
    ;

Decl
    : StorageClasses Decl { $$ = $2; }
    | BasicType Declarators TOK_SEMICOLON { $$ = new DeclarationTypedList($1); $$->addChild($2); }
    | BasicType Declarator FunctionBody { $$ = new DeclarationTypedList($1); $$->addChild($2); $2->addChild($3); }
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
    | Typeof TOK_DOT IdentifierList { $$ = 0; } /* TODO */  /* TODO */
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
    : { $$ = 0; } /* TODO */
    | BasicType2 { $$ = 0; } /* TODO */
    ;

BasicType2
    : TOK_MUL { $$ = 0; } /* TODO */
    | TOK_LEFT_SQUARE TOK_RIGHT_SQUARE { $$ = 0; } /* TODO */
    | TOK_LEFT_SQUARE AssignExpression TOK_RIGHT_SQUARE { $$ = 0; } /* TODO */
    | TOK_LEFT_SQUARE AssignExpression TOK_DOUBLE_DOT AssignExpression TOK_RIGHT_SQUARE { $$ = 0; } /* TODO */
    | TOK_LEFT_SQUARE Type TOK_RIGHT_SQUARE { $$ = 0; } /* TODO */
    | TOK_DELEGATE Parameters MemberFunctionAttributesopt { $$ = 0; } /* TODO */
    | TOK_FUNCTION Parameters FunctionAttributesopt { $$ = 0; } /* TODO */
    ;

Declarator
    : BasicType2opt TOK_LEFT_PAR Declarator TOK_RIGHT_PAR DeclaratorSuffixesopt { $$ = $3; }
    | BasicType2opt Identifier DeclaratorSuffixesopt {
            if ($3 == 0) $$ = new DeclarationNode(0, $2->name());
            else {
                $$ = $3;
                $$->setName($2->name());
                delete $2;
                }
            }
    ;

DeclaratorSuffixesopt
    : { $$ = 0; } /* TODO */
    | DeclaratorSuffixes { $$ = 0; } /* TODO */
    ;

DeclaratorSuffixes
    : DeclaratorSuffix { $$ = 0; } /* TODO */
    | DeclaratorSuffix DeclaratorSuffixes { $$ = 0; } /* TODO */
    ;

Constraintopt
    : { $$ = 0; } /* TODO */
    | TOK_INT_CONSTANT { $$ = 0; } /* TODO */
    ;

DeclaratorSuffix
    : TOK_LEFT_SQUARE TOK_RIGHT_SQUARE { $$ = 0; } /* TODO */
    | TOK_LEFT_SQUARE AssignExpression TOK_RIGHT_SQUARE { $$ = 0; } /* TODO */
    | TOK_LEFT_SQUARE Type TOK_RIGHT_SQUARE { $$ = 0; } /* TODO */
    | TemplateParametersopt Parameters MemberFunctionAttributesopt Constraintopt { $$ = new FunctionNode("", $2); } /* TODO */
    ;

IdentifierList
    : Identifier { $$ = $1; }
    | Identifier TOK_DOT IdentifierList { $$ = new IdentifierNode($1->name() + "." + $3->name()); delete $1; delete $3; }
    | TemplateInstance { $$ = 0; } /* TODO */
    | TemplateInstance TOK_DOT IdentifierList { $$ = 0; } /* TODO */
    ;

StorageClasses
    : StorageClass { $$ = 0; } /* TODO */
    | StorageClass StorageClasses { $$ = 0; } /* TODO */
    ;

StorageClass
    : TOK_ABSTRACT { $$ = 0; } /* TODO */
    | TOK_AUTO { $$ = 0; } /* TODO */
    | TypeCtor { $$ = 0; } /* TODO */
    | TOK_DEPRECATED { $$ = 0; } /* TODO */
    | TOK_ENUM { $$ = 0; } /* TODO */
    | TOK_EXTERN { $$ = 0; } /* TODO */
    | TOK_FINAL { $$ = 0; } /* TODO */
    | TOK_NOTHROW { $$ = 0; } /* TODO */
    | TOK_OVERRIDE { $$ = 0; } /* TODO */
    | TOK_PURE { $$ = 0; } /* TODO */
    | TOK__GSHARED { $$ = 0; } /* TODO */
    | Property { $$ = 0; } /* TODO */
    | TOK_SCOPE { $$ = 0; } /* TODO */
    | TOK_STATIC { $$ = 0; } /* TODO */
    | TOK_SYNCHRONIZED { $$ = 0; } /* TODO */
    ;

TypeCtors
    : TypeCtor { $$ = 0; } /* TODO */
    | TypeCtor TypeCtors { $$ = 0; } /* TODO */
    ;

TypeCtorsopt
    : { $$ = 0; } /* TODO */
    | TypeCtors { $$ = 0; } /* TODO */
    ;

TypeCtor
    : TOK_CONST { $$ = 0; } /* TODO */
    | TOK_IMMUTABLE { $$ = 0; } /* TODO */
    | TOK_INOUT { $$ = 0; } /* TODO */
    | TOK_SHARED { $$ = 0; } /* TODO */
    ;

Typeopt
    : { $$ = 0; } /* TODO */
    | Type { $$ = 0; } /* TODO */
    ;

Type
    : TypeCtorsopt BasicType { $$ = $2; }
    | TypeCtorsopt BasicType Declarator2 { $$ = $2; }
    ;

Declarator2
    : BasicType2opt DeclaratorSuffixesopt { $$ = 0; } /* TODO */
    | BasicType2opt TOK_LEFT_PAR Declarator2 TOK_RIGHT_PAR DeclaratorSuffixesopt { $$ = 0; } /* TODO */
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
    : InOutopt BasicType Declarator { $$ = $3; $3->setType($2); }
    | InOutopt BasicType Declarator TOK_ELLIPSIS { $$ = $3; $3->setType($2); }
    | InOutopt BasicType Declarator TOK_ASSIGN DefaultInitializerExpression { $$ = $3; $3->setType($2); }
    | InOutopt Type { $$ = new DeclarationNode($2, std::string()); }
    | InOutopt Type TOK_ELLIPSIS { $$ = 0; } /* TODO */
    ;

InOutopt
    : { $$ = 0; } /* TODO */
    | InOut { $$ = 0; } /* TODO */
    ;

InOut
    : InOutX { $$ = 0; } /* TODO */
    | InOut InOutX { $$ = 0; } /* TODO */
    ;

InOutX
    : TOK_AUTO { $$ = 0; } /* TODO */
    | TypeCtor { $$ = 0; } /* TODO */
    | TOK_FINAL { $$ = 0; } /* TODO */
    | TOK_IN { $$ = 0; } /* TODO */
    | TOK_LAZY { $$ = 0; } /* TODO */
    | TOK_OUT { $$ = 0; } /* TODO */
    | TOK_REF { $$ = 0; } /* TODO */
    | TOK_SCOPE { $$ = 0; } /* TODO */
    ;

FunctionAttributesopt
    : { $$ = 0; } /* TODO */
    | FunctionAttributes { $$ = 0; } /* TODO */
    ;

FunctionAttributes
    : FunctionAttribute { $$ = 0; } /* TODO */
    | FunctionAttribute FunctionAttributes { $$ = 0; } /* TODO */
    ;

FunctionAttribute
    : TOK_NOTHROW { $$ = 0; } /* TODO */
    | TOK_PURE { $$ = 0; } /* TODO */
    | Property { $$ = 0; } /* TODO */
    ;

MemberFunctionAttributesopt
    : { $$ = 0; } /* TODO */
    | MemberFunctionAttributes { $$ = 0; } /* TODO */
    ;

MemberFunctionAttributes
    : MemberFunctionAttribute { $$ = 0; } /* TODO */
    | MemberFunctionAttribute MemberFunctionAttributes { $$ = 0; } /* TODO */
    ;

MemberFunctionAttribute
    : TOK_CONST { $$ = 0; } /* TODO */
    | TOK_IMMUTABLE { $$ = 0; } /* TODO */
    | TOK_INOUT { $$ = 0; } /* TODO */
    | TOK_SHARED { $$ = 0; } /* TODO */
    | FunctionAttribute { $$ = 0; } /* TODO */
    ;

DefaultInitializerExpression
    : AssignExpression { $$ = 0; } /* TODO */
    | TOK__FILE { $$ = 0; } /* TODO */
    | TOK__MODULE { $$ = 0; } /* TODO */
    | TOK__LINE { $$ = 0; } /* TODO */
    | TOK__FUNCTION { $$ = 0; } /* TODO */
    | TOK__PRETTY_FUNCTION { $$ = 0; } /* TODO */
    ;

Initializer
    : VoidInitializer { $$ = 0; } /* TODO */
    | NonVoidInitializer { $$ = 0; } /* TODO */
    ;

NonVoidInitializer
    : AssignExpression { $$ = 0; } /* TODO */
    | ArrayInitializer { $$ = 0; } /* TODO */
    | StructInitializer { $$ = 0; } /* TODO */
    ;

ArrayInitializer
    : TOK_LEFT_SQUARE TOK_RIGHT_SQUARE { $$ = 0; } /* TODO */
    | TOK_LEFT_SQUARE ArrayMemberInitializations TOK_RIGHT_SQUARE { $$ = 0; } /* TODO */
    ;

ArrayMemberInitializations
    : ArrayMemberInitialization { $$ = 0; } /* TODO */
    | ArrayMemberInitialization TOK_COMMA { $$ = 0; } /* TODO */
    | ArrayMemberInitialization TOK_COMMA ArrayMemberInitializations { $$ = 0; } /* TODO */
    ;

ArrayMemberInitialization
    : NonVoidInitializer { $$ = 0; } /* TODO */
    | AssignExpression TOK_COLON NonVoidInitializer { $$ = 0; } /* TODO */
    ;

StructInitializer
    : TOK_LEFT_BRACE TOK_RIGHT_BRACE { $$ = 0; } /* TODO */
    | TOK_LEFT_BRACE StructMemberInitializers TOK_RIGHT_BRACE { $$ = 0; } /* TODO */
    ;

StructMemberInitializers
    : StructMemberInitializer { $$ = 0; } /* TODO */
    | StructMemberInitializer TOK_COMMA { $$ = 0; } /* TODO */
    | StructMemberInitializer TOK_COMMA StructMemberInitializers { $$ = 0; } /* TODO */
    ;

StructMemberInitializer
    : NonVoidInitializer { $$ = 0; } /* TODO */
    | TOK_IDENTIFIER TOK_COLON NonVoidInitializer { $$ = 0; } /* TODO */
    ;

AutoDeclaration
    : StorageClasses AutoDeclarationX TOK_SEMICOLON { $$ = 0; } /* TODO */
    ;

AutoDeclarationX
    : TOK_IDENTIFIER TOK_ASSIGN Initializer { $$ = 0; } /* TODO */
    | AutoDeclarationX TOK_COMMA TOK_IDENTIFIER TOK_ASSIGN Initializer { $$ = 0; } /* TODO */
    ;

Typeof
    : TOK_TYPEOF TOK_LEFT_PAR Expression TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    | TOK_TYPEOF TOK_LEFT_PAR TOK_RETURN TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    ;

VoidInitializer
    : TOK_VOID { $$ = 0; } /* TODO */
    ;


/* ***** Attributes ***** */

AttributeSpecifier
    : Attribute TOK_COLON { $$ = 0; } /* TODO */
    | Attribute DeclarationBlock { $$ = 0; } /* TODO */
    ;

Attribute
    : LinkageAttribute { $$ = 0; } /* TODO */
    | AlignAttribute { $$ = 0; } /* TODO */
    | Pragma { $$ = 0; } /* TODO */
    | DeprecatedAttribute { $$ = 0; } /* TODO */
    | ProtectionAttribute { $$ = 0; } /* TODO */
    | TOK_STATIC { $$ = 0; } /* TODO */
    | TOK_EXTERN { $$ = 0; } /* TODO */
    | TOK_FINAL { $$ = 0; } /* TODO */
    | TOK_SYNCHRONIZED { $$ = 0; } /* TODO */
    | TOK_OVERRIDE { $$ = 0; } /* TODO */
    | TOK_ABSTRACT { $$ = 0; } /* TODO */
    | TOK_AUTO { $$ = 0; } /* TODO */
    | TOK_SCOPE { $$ = 0; } /* TODO */
    | TOK_CONST { $$ = 0; } /* TODO */
    | TOK_IMMUTABLE { $$ = 0; } /* TODO */
    | TOK_INOUT { $$ = 0; } /* TODO */
    | TOK_SHARED { $$ = 0; } /* TODO */
    | TOK__GSHARED { $$ = 0; } /* TODO */
    | TOK_ATDISABLE { $$ = 0; } /* TODO */
    | TOK_ATPROPERTY { $$ = 0; } /* TODO */
    | Property { $$ = 0; } /* TODO */
    ;

Property
    : TOK_AT PropertyIdentifier { $$ = 0; } /* TODO */
    | UserDefinedAttribute { $$ = 0; } /* TODO */
    ;

PropertyIdentifier
    : TOK_PROPERTY { $$ = 0; } /* TODO */
    | TOK_SAFE { $$ = 0; } /* TODO */
    | TOK_TRUSTED { $$ = 0; } /* TODO */
    | TOK_SYSTEM { $$ = 0; } /* TODO */
    | TOK_DISABLE { $$ = 0; } /* TODO */
    ;

DeclarationBlock
    : DeclDef { $$ = 0; } /* TODO */
    |TOK_LEFT_BRACE DeclDefsopt TOK_RIGHT_BRACE { $$ = 0; } /* TODO */
    ;

LinkageAttribute
    : TOK_EXTERN TOK_LEFT_PAR LinkageType TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    ;

LinkageType
    : TOK_LINK_C { $$ = 0; } /* TODO */
    | TOK_LINK_CPP { $$ = 0; } /* TODO */
    | TOK_LINK_D { $$ = 0; } /* TODO */
    | TOK_LINK_WINDOWS { $$ = 0; } /* TODO */
    | TOK_LINK_PASCAL { $$ = 0; } /* TODO */
    | TOK_LINK_SYSTEM { $$ = 0; } /* TODO */
    ;

AlignAttribute
    : TOK_ALIGN { $$ = 0; } /* TODO */
    | TOK_ALIGN TOK_LEFT_PAR TOK_INT_CONSTANT TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    ;

DeprecatedAttribute
    : TOK_DEPRECATED { $$ = 0; } /* TODO */
    | TOK_DEPRECATED TOK_LEFT_PAR TOK_STRING_LITERAL TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    ;

ProtectionAttribute
    : TOK_PRIVATE { $$ = 0; } /* TODO */
    | TOK_PACKAGE { $$ = 0; } /* TODO */
    | TOK_PROTECTED { $$ = 0; } /* TODO */
    | TOK_PUBLIC { $$ = 0; } /* TODO */
    | TOK_EXPORT { $$ = 0; } /* TODO */
    ;

UserDefinedAttribute
    : TOK_AT TOK_LEFT_PAR ArgumentList TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    | TOK_AT TOK_IDENTIFIER { $$ = 0; } /* TODO */
    | TOK_AT TOK_IDENTIFIER TOK_LEFT_PAR ArgumentListopt TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    ;

/* ***** Pragmas ***** */

Pragma
    : TOK_PRAGMA TOK_LEFT_PAR TOK_IDENTIFIER TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    | TOK_PRAGMA TOK_LEFT_PAR TOK_IDENTIFIER TOK_COMMA ArgumentList TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    ;

/* ***** Expressions ***** */

Expressionopt
    : { $$ = 0; } /* TODO */
    | Expression { $$ = 0; } /* TODO */
    ;

Expression
    : CommaExpression { $$ = 0; } /* TODO */
    ;

CommaExpression
    : AssignExpression { $$ = 0; } /* TODO */
    | AssignExpression TOK_COMMA CommaExpression { $$ = 0; } /* TODO */
    ;


AssignExpression
    : ConditionalExpression { $$ = 0; } /* TODO */
    | ConditionalExpression TOK_ASSIGN AssignExpression { $$ = 0; } /* TODO */
    | ConditionalExpression TOK_PLUS_ASSIGN AssignExpression { $$ = 0; } /* TODO */
    | ConditionalExpression TOK_MINUS_ASSIGN AssignExpression { $$ = 0; } /* TODO */
    | ConditionalExpression TOK_MUL_ASSIGN AssignExpression { $$ = 0; } /* TODO */
    | ConditionalExpression TOK_DIV_ASSIGN AssignExpression { $$ = 0; } /* TODO */
    | ConditionalExpression TOK_MOD_ASSIGN AssignExpression { $$ = 0; } /* TODO */
    | ConditionalExpression TOK_AND_BIN_ASSIGN AssignExpression { $$ = 0; } /* TODO */
    | ConditionalExpression TOK_OR_BIN_ASSIGN AssignExpression { $$ = 0; } /* TODO */
    | ConditionalExpression TOK_XOR_ASSIGN AssignExpression { $$ = 0; } /* TODO */
    | ConditionalExpression TOK_TILDA_ASSIGN AssignExpression { $$ = 0; } /* TODO */
    | ConditionalExpression TOK_LEFT_ASSIGN AssignExpression { $$ = 0; } /* TODO */
    | ConditionalExpression TOK_RIGHT_ASSIGN AssignExpression { $$ = 0; } /* TODO */
    | ConditionalExpression TOK_PINE_ASSIGN AssignExpression { $$ = 0; } /* TODO */
    | ConditionalExpression TOK_POW_ASSIGN AssignExpression { $$ = 0; } /* TODO */
    ;

ConditionalExpression
    : OrOrExpression { $$ = 0; } /* TODO */
    | OrOrExpression TOK_QUESTION Expression TOK_COLON ConditionalExpression { $$ = 0; } /* TODO */
    ;

OrOrExpression
    : AndAndExpression { $$ = 0; } /* TODO */
    | OrOrExpression TOK_OR AndAndExpression { $$ = 0; } /* TODO */
    ;

AndAndExpression
/*
ambiguous
OrExpression->XorExpression->AndExpression->ShiftExpression
and
CmpExpression->ShiftExpression
*/
    : OrExpression %dprec 2 { $$ = 0; } /* TODO */
    | AndAndExpression TOK_AND OrExpression { $$ = 0; } /* TODO */
    | CmpExpression %dprec 1 { $$ = 0; } /*TODO */
    | AndAndExpression TOK_AND CmpExpression { $$ = 0; } /* TODO */
    ;

OrExpression
    : XorExpression { $$ = 0; } /* TODO */
    | OrExpression TOK_VERTICAL XorExpression { $$ = 0; } /* TODO */
    ;

XorExpression
    : AndExpression { $$ = 0; } /* TODO */
    | XorExpression TOK_UP_ARROW AndExpression { $$ = 0; } /* TODO */
    ;

AndExpression
    : ShiftExpression { $$ = 0; } /* TODO */
    | AndExpression TOK_AMPERSAND ShiftExpression { $$ = 0; } /* TODO */
    ;

CmpExpression
    : ShiftExpression { $$ = 0; } /* TODO */
    | EqualExpression { $$ = 0; } /* TODO */
    | IdentityExpression { $$ = 0; } /* TODO */
    | RelExpression { $$ = 0; } /* TODO */
    | InExpression { $$ = 0; } /* TODO */
    ;

EqualExpression
    : ShiftExpression TOK_EQ ShiftExpression { $$ = 0; } /* TODO */
    | ShiftExpression TOK_NE ShiftExpression { $$ = 0; } /* TODO */
    ;

IdentityExpression
    : ShiftExpression TOK_IS ShiftExpression { $$ = 0; } /* TODO */
    | ShiftExpression TOK_NOTIS ShiftExpression { $$ = 0; } /* TODO */
    ;

RelExpression
    : ShiftExpression TOK_LSS ShiftExpression { $$ = 0; } /* TODO */
    | ShiftExpression TOK_LE ShiftExpression { $$ = 0; } /* TODO */
    | ShiftExpression TOK_GRT ShiftExpression { $$ = 0; } /* TODO */
    | ShiftExpression TOK_GE ShiftExpression { $$ = 0; } /* TODO */
    | ShiftExpression TOK_UNORDERED ShiftExpression { $$ = 0; } /* TODO */
    | ShiftExpression TOK_UNORDERED_E ShiftExpression { $$ = 0; } /* TODO */
    | ShiftExpression TOK_LG ShiftExpression { $$ = 0; } /* TODO */
    | ShiftExpression TOK_LGE ShiftExpression { $$ = 0; } /* TODO */
    | ShiftExpression TOK_UNORDERED_LE ShiftExpression { $$ = 0; } /* TODO */
    | ShiftExpression TOK_UNORDERED_L ShiftExpression { $$ = 0; } /* TODO */
    | ShiftExpression TOK_UNORDERED_GE ShiftExpression { $$ = 0; } /* TODO */
    | ShiftExpression TOK_UNORDERED_G ShiftExpression { $$ = 0; } /* TODO */
    ;

InExpression
    : ShiftExpression TOK_IN ShiftExpression { $$ = 0; } /* TODO */
    | ShiftExpression TOK_NOTIN ShiftExpression { $$ = 0; } /* TODO */
    ;

ShiftExpression
    : AddExpression { $$ = 0; } /* TODO */
    | ShiftExpression TOK_LEFT AddExpression { $$ = 0; } /* TODO */
    | ShiftExpression TOK_RIGHT AddExpression { $$ = 0; } /* TODO */
    | ShiftExpression TOK_PINE AddExpression { $$ = 0; } /* TODO */
    ;

AddExpression
    : MulExpression { $$ = 0; } /* TODO */
    | AddExpression TOK_PLUS MulExpression { $$ = 0; } /* TODO */
    | AddExpression TOK_MINUS MulExpression { $$ = 0; } /* TODO */
    | CatExpression { $$ = 0; } /* TODO */
    ;

CatExpression
    : AddExpression TOK_TILDA MulExpression { $$ = 0; } /* TODO */
    ;

MulExpression
    : UnaryExpression { $$ = 0; } /* TODO */
    | MulExpression TOK_MUL UnaryExpression { $$ = 0; } /* TODO */
    | MulExpression TOK_DIV UnaryExpression { $$ = 0; } /* TODO */
    | MulExpression TOK_MOD UnaryExpression { $$ = 0; } /* TODO */
    ;

UnaryExpression
    : TOK_AMPERSAND UnaryExpression { $$ = 0; } /* TODO */
    | TOK_INC UnaryExpression { $$ = 0; } /* TODO */
    | TOK_DEC UnaryExpression { $$ = 0; } /* TODO */
    | TOK_MUL UnaryExpression { $$ = 0; } /* TODO */
    | TOK_DIV UnaryExpression { $$ = 0; } /* TODO */
    | TOK_PLUS UnaryExpression { $$ = 0; } /* TODO */
    | TOK_EXCLAMATION UnaryExpression { $$ = 0; } /* TODO */
    | ComplementExpression { $$ = 0; } /* TODO */
    | TOK_LEFT_PAR Type TOK_RIGHT_PAR TOK_DOT Identifier { $$ = 0; } /* TODO */
    | TOK_LEFT_PAR Type TOK_RIGHT_PAR TOK_DOT TemplateInstance { $$ = 0; } /* TODO */
    | DeleteExpression { $$ = 0; } /* TODO */
    | CastExpression { $$ = 0; } /* TODO */
    | PowExpression { $$ = 0; } /* TODO */
    ;

ComplementExpression
    : TOK_TILDA UnaryExpression { $$ = 0; } /* TODO */
    ;

NewExpression
    : TOK_NEW AllocatorArgumentsopt Type { $$ = 0; } /* TODO */
    | NewExpressionWithArgs { $$ = 0; } /* TODO */
    ;

NewExpressionWithArgs
    : TOK_NEW AllocatorArgumentsopt Type TOK_LEFT_SQUARE AssignExpression TOK_RIGHT_SQUARE { $$ = 0; } /* TODO */
    | TOK_NEW AllocatorArgumentsopt Type TOK_LEFT_PAR ArgumentList TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    | NewAnonClassExpression { $$ = 0; } /* TODO */
    ;

AllocatorArgumentsopt
    : { $$ = 0; } /* TODO */
    | AllocatorArguments { $$ = 0; } /* TODO */
    ;


AllocatorArguments
    : TOK_LEFT_PAR ArgumentListopt TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    ;

ArgumentListopt
    : { $$ = 0; } /* TODO */
    | ArgumentList { $$ = 0; } /* TODO */
    ;

ArgumentList
    : AssignExpression { $$ = 0; } /* TODO */
    | AssignExpression TOK_COMMA { $$ = 0; } /* TODO */
    | AssignExpression TOK_COMMA ArgumentList { $$ = 0; } /* TODO */
    ;

DeleteExpression
    : TOK_DELETE UnaryExpression { $$ = 0; } /* TODO */
    ;

CastExpression
    : TOK_CAST TOK_LEFT_PAR Type TOK_RIGHT_PAR UnaryExpression { $$ = 0; } /* TODO */
    | TOK_CAST TOK_LEFT_PAR CastQual TOK_RIGHT_PAR UnaryExpression { $$ = 0; } /* TODO */
    | TOK_CAST TOK_LEFT_PAR TOK_RIGHT_PAR UnaryExpression { $$ = 0; } /* TODO */
    ;

CastQual
    : TOK_CONST { $$ = 0; } /* TODO */
    | TOK_CONST TOK_SHARED { $$ = 0; } /* TODO */
    | TOK_SHARED TOK_CONST { $$ = 0; } /* TODO */
    | TOK_INOUT { $$ = 0; } /* TODO */
    | TOK_INOUT TOK_SHARED { $$ = 0; } /* TODO */
    | TOK_SHARED TOK_INOUT { $$ = 0; } /* TODO */
    | TOK_IMMUTABLE { $$ = 0; } /* TODO */
    | TOK_SHARED { $$ = 0; } /* TODO */
    ;

PowExpression
    : PostfixExpression { $$ = 0; } /* TODO */
    | PostfixExpression TOK_POW UnaryExpression { $$ = 0; } /* TODO */
    ;

PostfixExpression
    : PrimaryExpression { $$ = 0; } /* TODO */
    | PostfixExpression TOK_DOT TOK_IDENTIFIER { $$ = 0; } /* TODO */
    | PostfixExpression TOK_DOT TemplateInstance { $$ = 0; } /* TODO */
    | PostfixExpression TOK_DOT NewExpression { $$ = 0; } /* TODO */
    | PostfixExpression TOK_INC { $$ = 0; } /* TODO */
    | PostfixExpression TOK_DEC { $$ = 0; } /* TODO */
    | PostfixExpression TOK_LEFT_PAR TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    | PostfixExpression TOK_LEFT_PAR ArgumentList TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    | TypeCtorsopt BasicType TOK_LEFT_PAR TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    | TypeCtorsopt BasicType TOK_LEFT_PAR ArgumentList TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    | IndexExpression { $$ = 0; } /* TODO */
    | SliceExpression { $$ = 0; } /* TODO */
    ;

IndexExpression
    : PostfixExpression TOK_RIGHT_SQUARE ArgumentList TOK_LEFT_SQUARE { $$ = 0; } /* TODO */
    ;

SliceExpression
    : PostfixExpression TOK_LEFT_SQUARE TOK_RIGHT_SQUARE { $$ = 0; } /* TODO */
    | PostfixExpression TOK_LEFT_SQUARE AssignExpression TOK_DOUBLE_DOT AssignExpression TOK_RIGHT_SQUARE { $$ = 0; } /* TODO */
    ;

PrimaryExpression
    : TOK_IDENTIFIER { $$ = 0; } /* TODO */
    | TOK_DOT TOK_IDENTIFIER { $$ = 0; } /* TODO */
    | TemplateInstance { $$ = 0; } /* TODO */
    | TOK_DOT TemplateInstance { $$ = 0; } /* TODO */
    | TOK_THIS { $$ = 0; } /* TODO */
    | TOK_SUPER { $$ = 0; } /* TODO */
    | TOK_NULL { $$ = 0; } /* TODO */
    | TOK_TRUE { $$ = 0; } /* TODO */
    | TOK_FALSE { $$ = 0; } /* TODO */
    | TOK_DOLLAR { $$ = 0; } /* TODO */
    | TOK__FILE { $$ = 0; } /* TODO */
    | TOK__MODULE { $$ = 0; } /* TODO */
    | TOK__LINE { $$ = 0; } /* TODO */
    | TOK__FUNCTION { $$ = 0; } /* TODO */
    | TOK__PRETTY_FUNCTION { $$ = 0; } /* TODO */
    | TOK_INT_CONSTANT { $$ = 0; } /* TODO */
    | TOK_REAL_CONSTANT { $$ = 0; } /* TODO */
    | TOK_CHAR_LITERAL { $$ = 0; } /* TODO */
    | TOK_STRING_LITERAL { $$ = 0; } /* TODO */
    | ArrayLiteral { $$ = 0; } /* TODO */
    | AssocArrayLiteral { $$ = 0; } /* TODO */
    | Lambda { $$ = 0; } /* TODO */
    | FunctionLiteral { $$ = 0; } /* TODO */
    | AssertExpression { $$ = 0; } /* TODO */
    | MixinExpression { $$ = 0; } /* TODO */
    | ImportExpression { $$ = 0; } /* TODO */
    | NewExpressionWithArgs { $$ = 0; } /* TODO */
    | BasicType TOK_DOT TOK_IDENTIFIER { $$ = 0; } /* TODO */
    | Typeof { $$ = 0; } /* TODO */
    | TypeidExpression { $$ = 0; } /* TODO */
    | IsExpression { $$ = 0; } /* TODO */
    | TOK_LEFT_PAR Expression TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    /*| TraitsExpression { $$ = 0; } */ /* TODO */
    ;

StringLiterals
    : TOK_STRING_LITERAL { $$ = 0; } /* TODO */
    | StringLiterals TOK_STRING_LITERAL { $$ = 0; } /* TODO */
    ;

ArrayLiteral
    : TOK_LEFT_SQUARE ArgumentList TOK_RIGHT_SQUARE { $$ = 0; } /* TODO */
    ;

AssocArrayLiteral
    : TOK_LEFT_SQUARE KeyValuePairs TOK_RIGHT_SQUARE { $$ = 0; } /* TODO */
    ;

KeyValuePairs
    : KeyValuePair { $$ = 0; } /* TODO */
    | KeyValuePair TOK_COMMA KeyValuePairs { $$ = 0; } /* TODO */
    ;

KeyValuePair
    : KeyExpression TOK_COLON ValueExpression { $$ = 0; } /* TODO */
    ;

KeyExpression
    : AssignExpression { $$ = 0; } /* TODO */
    ;

ValueExpression
    : AssignExpression { $$ = 0; } /* TODO */
    ;

Lambda
    : TOK_IDENTIFIER TOK_RIGHT_ARROW AssignExpression { $$ = 0; } /* TODO */
    | TOK_FUNCTION ParameterAttributes TOK_RIGHT_ARROW AssignExpression { $$ = 0; } /* TODO */
    | TOK_DELEGATE ParameterAttributes TOK_RIGHT_ARROW AssignExpression { $$ = 0; } /* TODO */
    | ParameterAttributes TOK_RIGHT_ARROW AssignExpression { $$ = 0; } /* TODO */
    ;

FunctionLiteral
    : TOK_FUNCTION Typeopt ParameterAttributesopt FunctionBody { $$ = 0; } /* TODO */
    | TOK_DELEGATE Typeopt ParameterAttributesopt FunctionBody { $$ = 0; } /* TODO */
    | ParameterAttributes FunctionBody { $$ = 0; } /* TODO */
    | FunctionBody { $$ = 0; } /* TODO */
    ;

ParameterAttributesopt
    : { $$ = 0; } /* TODO */
    | ParameterAttributes { $$ = 0; } /* TODO */
    ;

ParameterAttributes
    : Parameters { $$ = 0; } /* TODO */
    | Parameters FunctionAttributes { $$ = 0; } /* TODO */
    ;

AssertExpression
    : TOK_ASSERT TOK_LEFT_PAR AssignExpression TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    | TOK_ASSERT TOK_LEFT_PAR AssignExpression TOK_COMMA AssignExpression TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    ;

MixinExpression
    : TOK_MIXIN TOK_LEFT_PAR AssignExpression TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    ;

ImportExpression
    : TOK_IMPORT TOK_LEFT_PAR AssignExpression TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    ;

TypeidExpression
    : TOK_TYPEID TOK_LEFT_PAR Type TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    | TOK_TYPEID TOK_LEFT_PAR Expression TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    ;

IsExpression
    : TOK_IS TOK_LEFT_PAR Type TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    | TOK_IS TOK_LEFT_PAR Type TOK_COLON TypeSpecialization TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    | TOK_IS TOK_LEFT_PAR Type TOK_EQ TypeSpecialization TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    | TOK_IS TOK_LEFT_PAR Type TOK_COLON TypeSpecialization TOK_COMMA TemplateParameterList TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    | TOK_IS TOK_LEFT_PAR Type TOK_EQ TypeSpecialization TOK_COMMA TemplateParameterList TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    | TOK_IS TOK_LEFT_PAR Type TOK_IDENTIFIER TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    | TOK_IS TOK_LEFT_PAR Type TOK_IDENTIFIER TOK_COLON TypeSpecialization TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    | TOK_IS TOK_LEFT_PAR Type TOK_IDENTIFIER TOK_EQ TypeSpecialization TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    | TOK_IS TOK_LEFT_PAR Type TOK_IDENTIFIER TOK_COLON TypeSpecialization TOK_COMMA TemplateParameterList TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    | TOK_IS TOK_LEFT_PAR Type TOK_IDENTIFIER TOK_EQ TypeSpecialization TOK_COMMA TemplateParameterList TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    ;

TypeSpecialization
    : Type { $$ = 0; } /* TODO */
    | TOK_STRUCT { $$ = 0; } /* TODO */
    | TOK_UNION { $$ = 0; } /* TODO */
    | TOK_CLASS { $$ = 0; } /* TODO */
    | TOK_INTERFACE { $$ = 0; } /* TODO */
    | TOK_ENUM { $$ = 0; } /* TODO */
    | TOK_FUNCTION { $$ = 0; } /* TODO */
    | TOK_DELEGATE { $$ = 0; } /* TODO */
    | TOK_SUPER { $$ = 0; } /* TODO */
    | TOK_CONST { $$ = 0; } /* TODO */
    | TOK_IMMUTABLE { $$ = 0; } /* TODO */
    | TOK_INOUT { $$ = 0; } /* TODO */
    | TOK_SHARED { $$ = 0; } /* TODO */
    | TOK_RETURN { $$ = 0; } /* TODO */
    | TOK__PARAMETERS { $$ = 0; } /* TODO */
    ;

/* ***** Statements ***** */

Statement
    : TOK_SEMICOLON { $$ = 0; }
    | NonEmptyStatement { $$ = $1; }
    | ScopeBlockStatement { $$ = $1; }
    ;

NoScopeNonEmptyStatement
    : NonEmptyStatement { $$ = 0; } /* TODO */
    | BlockStatement { $$ = 0; } /* TODO */
    ;

NoScopeStatement
    : TOK_SEMICOLON { $$ = 0; } /* TODO */
    | NonEmptyStatement { $$ = 0; } /* TODO */
    | BlockStatement { $$ = 0; } /* TODO */
    ;

NonEmptyOrScopeBlockStatement
    : NonEmptyStatement { $$ = 0; } /* TODO */
    | ScopeBlockStatement { $$ = 0; } /* TODO */
    ;

NonEmptyStatement
    : NonEmptyStatementNoCaseNoDefault { $$ = $1; }
    | CaseStatement { $$ = 0; } /* TODO */
    | CaseRangeStatement { $$ = 0; } /* TODO */
    | DefaultStatement { $$ = 0; } /* TODO */
    ;

NonEmptyStatementNoCaseNoDefault
    : LabeledStatement { $$ = 0; } /* TODO */
    | ExpressionStatement { $$ = 0; } /* TODO */
    | DeclarationStatement { $$ = $1; }
    | IfStatement { $$ = 0; } /* TODO */
    | WhileStatement { $$ = 0; } /* TODO */
    | DoStatement { $$ = 0; } /* TODO */
    | ForStatement { $$ = 0; } /* TODO */
    | ForeachStatement { $$ = 0; } /* TODO */
    | SwitchStatement { $$ = 0; } /* TODO */
    | FinalSwitchStatement { $$ = 0; } /* TODO */
    | ContinueStatement { $$ = 0; } /* TODO */
    | BreakStatement { $$ = 0; } /* TODO */
    | ReturnStatement { $$ = 0; } /* TODO */
    | GotoStatement { $$ = 0; } /* TODO */
    | WithStatement { $$ = 0; } /* TODO */
    | SynchronizedStatement { $$ = 0; } /* TODO */
    | TryStatement { $$ = 0; } /* TODO */
    | ScopeGuardStatement { $$ = 0; } /* TODO */
    | ThrowStatement { $$ = 0; } /* TODO */
/*    | AsmStatement { $$ = 0; }  */ /* TODO */
    | PragmaStatement { $$ = 0; } /* TODO */
    | MixinStatement { $$ = 0; } /* TODO */
    | ForeachRangeStatement { $$ = 0; } /* TODO */
    /*| ConditionalStatement { $$ = 0; } */ /* TODO */
    /*| StaticAssert { $$ = 0; } */ /* TODO */
    | TemplateMixin { $$ = 0; } /* TODO */
    | ImportDeclaration { $$ = 0; } /* TODO */
    ;

ScopeStatement
    : NonEmptyStatement { $$ = 0; } /* TODO */
    | BlockStatement { $$ = 0; } /* TODO */
    ;

ScopeBlockStatement
    : BlockStatement { $$ = 0; } /* TODO */
    ;


LabeledStatement
    : TOK_IDENTIFIER TOK_COLON { $$ = 0; } /* TODO */
    | TOK_IDENTIFIER TOK_COLON NoScopeStatement { $$ = 0; } /* TODO */
    | TOK_IDENTIFIER TOK_COLON Statement { $$ = 0; } /* TODO */
    ;

BlockStatement
    : TOK_LEFT_BRACE TOK_RIGHT_BRACE { $$ = 0; }
    | TOK_LEFT_BRACE StatementList TOK_RIGHT_BRACE { $$ = $2; }
    ;

StatementList
    : Statement { $$ = new NodeList; $$->addChild($1); }
    | Statement StatementList { $$ = $2; $$->addChild($1); }
    ;

ExpressionStatement
    : Expression TOK_SEMICOLON { $$ = 0; } /* TODO */
    ;

DeclarationStatement
    : Declaration { $$ = $1; }
    ;

IfStatement
    : TOK_IF TOK_LEFT_PAR IfCondition TOK_RIGHT_PAR ThenStatement { $$ = 0; } /* TODO */
    | TOK_IF TOK_LEFT_PAR IfCondition TOK_RIGHT_PAR ThenStatement TOK_ELSE ElseStatement { $$ = 0; } /* TODO */
    ;

IfCondition
    : Expression { $$ = 0; } /* TODO */
    | TOK_AUTO TOK_IDENTIFIER TOK_ASSIGN Expression { $$ = 0; } /* TODO */
    | BasicType Declarator TOK_ASSIGN Expression { $$ = 0; } /* TODO */
    ;

ThenStatement
    : ScopeStatement { $$ = 0; } /* TODO */
    ;

ElseStatement
    : ScopeStatement { $$ = 0; } /* TODO */
    ;

WhileStatement
    : TOK_WHILE TOK_LEFT_PAR Expression TOK_RIGHT_PAR ScopeStatement { $$ = 0; } /* TODO */
    ;

DoStatement
    : TOK_DO ScopeStatement TOK_WHILE TOK_LEFT_PAR Expression TOK_RIGHT_PAR TOK_SEMICOLON { $$ = 0; } /* TODO */
    ;

ForStatement
    : TOK_FOR TOK_LEFT_PAR Initialize Testopt TOK_SEMICOLON Incrementopt TOK_RIGHT_PAR ScopeStatement { $$ = 0; } /* TODO */
    ;

Initialize
    : TOK_SEMICOLON { $$ = 0; } /* TODO */
    | NoScopeNonEmptyStatement { $$ = 0; } /* TODO */
    ;

Testopt
    : { $$ = 0; } /* TODO */
    | Testopt { $$ = 0; } /* TODO */
    ;

Test
    : Expression { $$ = 0; } /* TODO */
    ;

Incrementopt
    : { $$ = 0; } /* TODO */
    | Increment { $$ = 0; } /* TODO */
    ;

Increment
    : Expression { $$ = 0; } /* TODO */
    ;

ForeachStatement
    : Foreach TOK_LEFT_PAR ForeachTypeList TOK_SEMICOLON Aggregate TOK_RIGHT_PAR NoScopeNonEmptyStatement { $$ = 0; } /* TODO */
    ;

Foreach
    : TOK_FOREACH { $$ = 0; } /* TODO */
    | TOK_FOREACH_REVERSE { $$ = 0; } /* TODO */
    ;

ForeachTypeList
    : ForeachType { $$ = 0; } /* TODO */
    | ForeachType TOK_COMMA ForeachTypeList { $$ = 0; } /* TODO */
    ;

Refopt
    : { $$ = 0; } /* TODO */
    | TOK_REF { $$ = 0; } /* TODO */
    ;

ForeachType
    : Refopt BasicType Declarator { $$ = 0; } /* TODO */
    | Refopt TOK_IDENTIFIER { $$ = 0; } /* TODO */
    ;

Aggregate
    : Expression { $$ = 0; } /* TODO */
    ;

ForeachRangeStatement
    : Foreach TOK_LEFT_PAR ForeachType TOK_SEMICOLON LwrExpression TOK_DOUBLE_DOT UprExpression TOK_RIGHT_PAR ScopeStatement { $$ = 0; } /* TODO */
    ;

LwrExpression
    : Expression { $$ = 0; } /* TODO */
    ;

UprExpression
    : Expression { $$ = 0; } /* TODO */
    ;

SwitchStatement
    : TOK_SWITCH TOK_LEFT_PAR Expression TOK_RIGHT_PAR ScopeStatement { $$ = 0; } /* TODO */
    ;

CaseStatement
    : TOK_CASE ArgumentList TOK_COLON ScopeStatementList { $$ = 0; } /* TODO */
    ;

CaseRangeStatement
    : TOK_CASE FirstExp TOK_COLON TOK_DOUBLE_DOT TOK_CASE LastExp TOK_COLON ScopeStatementList { $$ = 0; } /* TODO */
    ;

FirstExp
    : AssignExpression { $$ = 0; } /* TODO */
    ;

LastExp
    : AssignExpression { $$ = 0; } /* TODO */
    ;

DefaultStatement
    : TOK_DEFAULT TOK_COLON ScopeStatementList { $$ = 0; } /* TODO */
    ;

ScopeStatementList
    : StatementListNoCaseNoDefault { $$ = 0; } /* TODO */
    ;

StatementListNoCaseNoDefault
    : StatementNoCaseNoDefault { $$ = 0; } /* TODO */
    | StatementNoCaseNoDefault StatementListNoCaseNoDefault { $$ = 0; } /* TODO */
    ;

StatementNoCaseNoDefault
    : TOK_SEMICOLON { $$ = 0; } /* TODO */
    | NonEmptyStatementNoCaseNoDefault { $$ = 0; } /* TODO */
    | ScopeBlockStatement { $$ = 0; } /* TODO */
    ;

FinalSwitchStatement
    : TOK_FINAL TOK_SWITCH TOK_LEFT_PAR Expression TOK_RIGHT_PAR ScopeStatement { $$ = 0; } /* TODO */
    ;


Identifieropt
    : Identifier { $$ = $1; }
    | { $$ = 0; }
    ;

Identifier
    : TOK_IDENTIFIER { $$ = new IdentifierNode($1); }
    ;

ContinueStatement
    : TOK_CONTINUE Identifieropt TOK_SEMICOLON { $$ = 0; } /* TODO */
    ;

BreakStatement
    : TOK_BREAK Identifieropt TOK_SEMICOLON { $$ = 0; } /* TODO */
    ;

ReturnStatement
    : TOK_RETURN Expressionopt TOK_SEMICOLON { $$ = 0; } /* TODO */
    ;

GotoStatement
    : TOK_GOTO Identifier TOK_SEMICOLON { $$ = 0; } /* TODO */
    | TOK_GOTO TOK_DEFAULT TOK_SEMICOLON { $$ = 0; } /* TODO */
    | TOK_GOTO TOK_CASE TOK_SEMICOLON { $$ = 0; } /* TODO */
    | TOK_GOTO TOK_CASE Expression TOK_SEMICOLON { $$ = 0; } /* TODO */
    ;

WithStatement
    : TOK_WITH TOK_LEFT_PAR Expression TOK_RIGHT_PAR ScopeStatement { $$ = 0; } /* TODO */
    | TOK_WITH TOK_LEFT_PAR Symbol TOK_RIGHT_PAR ScopeStatement { $$ = 0; } /* TODO */
    | TOK_WITH TOK_LEFT_PAR TemplateInstance TOK_RIGHT_PAR ScopeStatement { $$ = 0; } /* TODO */
    ;

SynchronizedStatement
    : TOK_SYNCHRONIZED ScopeStatement { $$ = 0; } /* TODO */
    | TOK_SYNCHRONIZED TOK_LEFT_PAR Expression TOK_RIGHT_PAR ScopeStatement { $$ = 0; } /* TODO */
    ;

TryStatement
    : TOK_TRY ScopeStatement Catches { $$ = 0; } /* TODO */
    | TOK_TRY ScopeStatement Catches FinallyStatement { $$ = 0; } /* TODO */
    | TOK_TRY ScopeStatement FinallyStatement { $$ = 0; } /* TODO */
    ;

Catches
    : LastCatch { $$ = 0; } /* TODO */
    | Catch { $$ = 0; } /* TODO */
    | Catch Catches { $$ = 0; } /* TODO */
    ;

LastCatch
    : TOK_CATCH NoScopeNonEmptyStatement { $$ = 0; } /* TODO */
    ;

Catch
    : TOK_CATCH TOK_LEFT_PAR CatchParameter TOK_RIGHT_PAR NoScopeNonEmptyStatement { $$ = 0; } /* TODO */
    ;

CatchParameter
    : BasicType Identifier { $$ = 0; } /* TODO */
    ;

FinallyStatement
    : TOK_FINALLY NoScopeNonEmptyStatement { $$ = 0; } /* TODO */
    ;

ThrowStatement
    : TOK_THROW Expression TOK_SEMICOLON { $$ = 0; } /* TODO */
    ;

ScopeGuardStatement
    : TOK_SCOPE_EXIT NonEmptyOrScopeBlockStatement { $$ = 0; } /* TODO */
    | TOK_SCOPE_SUCCESS NonEmptyOrScopeBlockStatement { $$ = 0; } /* TODO */
    | TOK_SCOPE_FAILURE NonEmptyOrScopeBlockStatement { $$ = 0; } /* TODO */
    ;

/*AsmStatement
    : TOK_ASM TOK_LEFT_BRACE TOK_RIGHT_BRACE { $$ = 0; }
    | TOK_ASM TOK_LEFT_BRACE AsmInstructionList TOK_RIGHT_BRACE { $$ = 0; }
    ;

AsmInstructionList
    : AsmInstruction TOK_SEMICOLON { $$ = 0; }
    | AsmInstruction TOK_SEMICOLON AsmInstructionList { $$ = 0; }*/

PragmaStatement
    : Pragma NoScopeStatement { $$ = 0; } /* TODO */
    ;

MixinStatement
    : TOK_MIXIN TOK_LEFT_PAR AssignExpression TOK_RIGHT_PAR TOK_SEMICOLON { $$ = 0; } /* TODO */
    ;


/* ***** Structs & Unions ***** */

AggregateDeclaration
    : TOK_STRUCT Identifier StructBody { $$ = 0; } /* TODO */
    | TOK_UNION Identifier StructBody { $$ = 0; } /* TODO */
    | TOK_STRUCT Identifier TOK_SEMICOLON { $$ = 0; } /* TODO */
    | TOK_UNION Identifier TOK_SEMICOLON { $$ = 0; } /* TODO */
    | StructTemplateDeclaration { $$ = 0; } /* TODO */
    | UnionTemplateDeclaration { $$ = 0; } /* TODO */
    ;

StructBody
    : TOK_LEFT_BRACE TOK_RIGHT_BRACE { $$ = 0; } /* TODO */
    | TOK_LEFT_BRACE StructBodyDeclarations TOK_RIGHT_BRACE { $$ = 0; } /* TODO */
    ;

StructBodyDeclarations
    : StructBodyDeclaration { $$ = 0; } /* TODO */
    | StructBodyDeclaration StructBodyDeclarations { $$ = 0; } /* TODO */
    ;

StructBodyDeclaration
    : DeclDef { $$ = 0; } /* TODO */
    | StructAllocator { $$ = 0; } /* TODO */
    | StructDeallocator { $$ = 0; } /* TODO */
    | StructPostblit { $$ = 0; } /* TODO */
    | AliasThis { $$ = 0; } /* TODO */
    ;

StructAllocator
    : ClassAllocator { $$ = 0; } /* TODO */
    ;

StructDeallocator
    : ClassDeallocator { $$ = 0; } /* TODO */
    ;

StructPostblit
    : TOK_THIS TOK_LEFT_PAR TOK_THIS TOK_RIGHT_PAR MemberFunctionAttributesopt FunctionBody { $$ = 0; } /* TODO */
    ;


/* ***** Classes ***** */

ClassDeclaration
    : TOK_CLASS Identifier BaseClassListopt ClassBody { $$ = new ClassNode($2->name()); delete $2; $$->addChild($4); $$->setPosition(@4.first_line, @4.last_line);}
    | ClassTemplateDeclaration { $$ = $1; }
    ;

BaseClassListopt
    : { $$ = 0; } /* TODO */
    | BaseClassList { $$ = 0; } /* TODO */
    ;

BaseClassList
    : TOK_COLON SuperClass { $$ = 0; } /* TODO */
    | TOK_COLON SuperClass TOK_COMMA Interfaces { $$ = 0; } /* TODO */
    | TOK_COLON Interfaces { $$ = 0; } /* TODO */
    ;

SuperClassopt
    : { $$ = 0; } /* TODO */
    | SuperClass { $$ = 0; } /* TODO */
    ;

SuperClass
    : Identifier { $$ = 0; } /* TODO */
    ;

Interfacesopt
    : { $$ = 0; } /* TODO */
    | Interfaces { $$ = 0; } /* TODO */
    ;

Interfaces
    : Interface { $$ = 0; } /* TODO */
    | Interface TOK_COMMA Interfaces
    ;

Interface
    : Identifier { $$ = 0; } /* TODO */
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
    : TOK_TILDA TOK_THIS TOK_LEFT_PAR TOK_RIGHT_PAR MemberFunctionAttributesopt FunctionBody { $$ = new DestructorNode; $$->addChild($6); $$->setPosition(@6.first_line, @6.last_line); }
    ;

StaticConstructor
    : TOK_STATIC TOK_THIS TOK_LEFT_PAR TOK_RIGHT_PAR FunctionBody { $$ = 0; } /* TODO */
    ;

StaticDestructor
    : TOK_STATIC TOK_TILDA TOK_THIS TOK_LEFT_PAR TOK_RIGHT_PAR FunctionBody { $$ = 0; } /* TODO */
    ;

SharedStaticConstructor
    : TOK_SHARED TOK_STATIC TOK_THIS TOK_LEFT_PAR TOK_RIGHT_PAR FunctionBody { $$ = 0; } /* TODO */
    ;

SharedStaticDestructor
    : TOK_SHARED TOK_STATIC TOK_TILDA TOK_THIS TOK_LEFT_PAR TOK_RIGHT_PAR FunctionBody { $$ = 0; } /* TODO */
    ;

Invariant
    : TOK_INVARIANT TOK_LEFT_PAR TOK_RIGHT_PAR BlockStatement { $$ = 0; } /* TODO */
    ;

ClassAllocator
    : TOK_NEW Parameters FunctionBody { $$ = 0; } /* TODO */
    ;

ClassDeallocator
    : TOK_DELETE Parameters FunctionBody { $$ = 0; } /* TODO */
    ;

AliasThis
    : TOK_ALIAS Identifier TOK_THIS TOK_SEMICOLON { $$ = 0; } /* TODO */
    ;

NewAnonClassExpression
    : TOK_NEW AllocatorArgumentsopt TOK_CLASS ClassArgumentsopt SuperClassopt Interfacesopt { $$ = 0; } /* TODO */
    | ClassBody { $$ = 0; } /* TODO */
    ;

ClassArgumentsopt
    : { $$ = 0; } /* TODO */
    | ClassArguments { $$ = 0; } /* TODO */
    ;

ClassArguments
    : TOK_LEFT_PAR ArgumentListopt TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    ;

/* ***** Interfaces ***** */

InterfaceDeclaration
    : TOK_INTERFACE Identifier BaseInterfaceListopt InterfaceBody { $$ = 0; } /* TODO */
    | InterfaceTemplateDeclaration { $$ = 0; } /* TODO */
    ;

BaseInterfaceListopt
    : { $$ = 0; } /* TODO */
    | BaseInterfaceList { $$ = 0; } /* TODO */
    ;

BaseInterfaceList
    : TOK_COLON Interfaces { $$ = 0; } /* TODO */ /* FIXED InterfaceClasses */
    ;

InterfaceBody
    : TOK_LEFT_BRACE DeclDefsopt TOK_RIGHT_BRACE { $$ = 0; } /* TODO */
    ;

/* ***** Enums ***** */

EnumDeclaration
    : TOK_ENUM EnumTag EnumBody { $$ = 0; } /* TODO */
    | TOK_ENUM EnumBody { $$ = 0; } /* TODO */
    | TOK_ENUM EnumTag TOK_COLON EnumBaseType EnumBody { $$ = 0; } /* TODO */
    | TOK_ENUM TOK_COLON EnumBaseType EnumBody { $$ = 0; } /* TODO */
    ;

EnumTag
    : Identifier { $$ = 0; } /* TODO */
    ;

EnumBaseType
    : Type { $$ = 0; } /* TODO */
    ;

EnumBody
    : EmptyEnumBody { $$ = 0; } /* TODO */
    | EnumMembersBody { $$ = 0; } /* TODO */
    ;

EmptyEnumBody
    : TOK_SEMICOLON { $$ = 0; } /* TODO */
    ;

EnumMembersBody
    : TOK_LEFT_BRACE EnumMembers TOK_RIGHT_BRACE { $$ = 0; } /* TODO */
    ;

EnumMembers
    : EnumMember { $$ = 0; } /* TODO */
    | EnumMember TOK_COMMA { $$ = 0; } /* TODO */
    | EnumMember TOK_COMMA EnumMembers { $$ = 0; } /* TODO */
    ;

EnumMember
    : Identifier { $$ = 0; } /* TODO */
    | Identifier TOK_ASSIGN AssignExpression { $$ = 0; } /* TODO */
    | Type Identifier TOK_ASSIGN AssignExpression { $$ = 0; } /* TODO */
    ;


/* ***** Functions  http://dlang.org/function.html ***** */

FunctionBody
    : BlockStatement { $$ = $1;} /* TODO */
    | BodyStatement { $$ = 0; } /* TODO */
    | InStatement BodyStatement { $$ = 0;} /* TODO */
    | OutStatement BodyStatement { $$ = 0; } /* TODO */
    | InStatement OutStatement BodyStatement { $$ = 0;} /* TODO */
    | OutStatement InStatement BodyStatement { $$ = 0;} /* TODO */
    ;

InStatement
    : TOK_IN BlockStatement { $$ = 0; } /* TODO */
    ;

OutStatement
    : TOK_OUT BlockStatement { $$ = 0; } /* TODO */
    | TOK_OUT TOK_LEFT_PAR TOK_IDENTIFIER TOK_RIGHT_PAR BlockStatement { $$ = 0; } /* TODO */
    ;

BodyStatement
    : TOK_BODY BlockStatement { $$ = 0; } /* TODO */
    ;


/* ***** Templates ***** */

TemplateDeclaration
    : TOK_TEMPLATE TemplateIdentifier TemplateParameters Constraintopt { $$ = 0; } /* TODO */
    | TOK_LEFT_BRACE DeclDefs TOK_RIGHT_BRACE { $$ = 0; } /* TODO */
    ;

TemplateIdentifier
    : Identifier { $$ = 0; } /* TODO */
    ;

TemplateParametersopt
    : { $$ = 0; } /* TODO */
    | TemplateParameters { $$ = 0; } /* TODO */
    ;

TemplateParameters
    : TOK_LEFT_PAR TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    | TOK_LEFT_PAR TemplateParameterList TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    ;

TemplateParameterList
    : TemplateParameter { $$ = 0; } /* TODO */
    | TemplateParameter TOK_COMMA { $$ = 0; } /* TODO */
    | TemplateParameter TOK_COMMA TemplateParameterList { $$ = 0; } /* TODO */
    ;

TemplateParameter
    : TemplateTypeParameter { $$ = 0; } /* TODO */
    | TemplateValueParameter { $$ = 0; } /* TODO */
    | TemplateAliasParameter { $$ = 0; } /* TODO */
    | TemplateTupleParameter { $$ = 0; } /* TODO */
    | TemplateThisParameter { $$ = 0; } /* TODO */
    ;

TemplateInstance
    : TemplateIdentifier TemplateArguments { $$ = 0; } /* TODO */
    ;

TemplateArgumentsopt
    : { $$ = 0; } /* TODO */
    | TemplateArguments { $$ = 0; } /* TODO */
    ;

TemplateArguments
    : TOK_EXCLAMATION TOK_LEFT_PAR TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    | TOK_EXCLAMATION TOK_LEFT_PAR TemplateArgumentList TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    | TOK_EXCLAMATION TemplateSingleArgument { $$ = 0; } /* TODO */
    ;

TemplateArgumentList
    : TemplateArgument { $$ = 0; } /* TODO */
    | TemplateArgument TOK_COMMA { $$ = 0; } /* TODO */
    | TemplateArgument TOK_COMMA TemplateArgumentList { $$ = 0; } /* TODO */
    ;

TemplateArgument
    : Type { $$ = 0; } /* TODO */
    | AssignExpression { $$ = 0; } /* TODO */
    | Symbol { $$ = 0; } /* TODO */
    ;

Symbol
    : SymbolTail { $$ = 0; } /* TODO */
    | TOK_DOT SymbolTail { $$ = 0; } /* TODO */
    ;

SymbolTail
    : Identifier { $$ = 0; } /* TODO */
    | Identifier TOK_DOT SymbolTail { $$ = 0; } /* TODO */
    | TemplateInstance { $$ = 0; } /* TODO */
    | TemplateInstance TOK_DOT SymbolTail { $$ = 0; } /* TODO */
    ;

TemplateSingleArgument
    : Identifier { $$ = 0; } /* TODO */
    | BasicTypeX { $$ = 0; } /* TODO */
    | TOK_CHAR_LITERAL { $$ = 0; } /* TODO */
    | TOK_STRING_LITERAL { $$ = 0; } /* TODO */
    | TOK_INT_CONSTANT { $$ = 0; } /* TODO */
    | TOK_REAL_CONSTANT { $$ = 0; } /* TODO */
    | TOK_TRUE { $$ = 0; } /* TODO */
    | TOK_FALSE { $$ = 0; } /* TODO */
    | TOK_NULL { $$ = 0; } /* TODO */
    | TOK_THIS { $$ = 0; } /* TODO */
    | TOK__FILE { $$ = 0; } /* TODO */
    | TOK__MODULE { $$ = 0; } /* TODO */
    | TOK__LINE { $$ = 0; } /* TODO */
    | TOK__FUNCTION { $$ = 0; } /* TODO */
    | TOK__PRETTY_FUNCTION { $$ = 0; } /* TODO */
    ;

TemplateTypeParameter
    : Identifier { $$ = 0; } /* TODO */
    | Identifier TemplateTypeParameterSpecialization { $$ = 0; } /* TODO */
    | Identifier TemplateTypeParameterDefault { $$ = 0; } /* TODO */
    | Identifier TemplateTypeParameterSpecialization TemplateTypeParameterDefault { $$ = 0; } /* TODO */
    ;

TemplateTypeParameterSpecialization
    : TOK_COLON Type { $$ = 0; } /* TODO */
    ;

TemplateTypeParameterDefault
    : TOK_ASSIGN Type { $$ = 0; } /* TODO */
    ;

TemplateThisParameter
    : TOK_THIS TemplateTypeParameter { $$ = 0; } /* TODO */
    ;

TemplateValueParameter
    : BasicType Declarator { $$ = 0; } /* TODO */
    | BasicType Declarator TemplateValueParameterSpecialization { $$ = 0; } /* TODO */
    | BasicType Declarator TemplateValueParameterDefault { $$ = 0; } /* TODO */
    | BasicType Declarator TemplateValueParameterSpecialization TemplateValueParameterDefault { $$ = 0; } /* TODO */
    ;

TemplateValueParameterSpecialization
    : TOK_COLON ConditionalExpression { $$ = 0; } /* TODO */
    ;

TemplateValueParameterDefault
    : TOK_ASSIGN TOK__FILE { $$ = 0; } /* TODO */
    | TOK_ASSIGN TOK__MODULE { $$ = 0; } /* TODO */
    | TOK_ASSIGN TOK__LINE { $$ = 0; } /* TODO */
    | TOK_ASSIGN TOK__FUNCTION { $$ = 0; } /* TODO */
    | TOK_ASSIGN TOK__PRETTY_FUNCTION { $$ = 0; } /* TODO */
    | TOK_ASSIGN AssignExpression { $$ = 0; } /* TODO */
    ;

TemplateAliasParameter
    : TOK_ALIAS Identifier TemplateAliasParameterSpecializationopt TemplateAliasParameterDefaultopt { $$ = 0; } /* TODO */
    | TOK_ALIAS BasicType Declarator TemplateAliasParameterSpecializationopt TemplateAliasParameterDefaultopt { $$ = 0; } /* TODO */
    ;

TemplateAliasParameterSpecializationopt
    : { $$ = 0; } /* TODO */
    | TemplateAliasParameterSpecialization { $$ = 0; } /* TODO */
    ;

TemplateAliasParameterSpecialization
    : TOK_COLON Type { $$ = 0; } /* TODO */
    | TOK_COLON ConditionalExpression { $$ = 0; } /* TODO */
    ;

TemplateAliasParameterDefaultopt
    : { $$ = 0; } /* TODO */
    | TemplateAliasParameterDefault { $$ = 0; } /* TODO */
    ;

TemplateAliasParameterDefault
    : TOK_ASSIGN Type { $$ = 0; } /* TODO */
    | TOK_ASSIGN ConditionalExpression { $$ = 0; } /* TODO */
    ;

TemplateTupleParameter
    : Identifier TOK_ELLIPSIS { $$ = 0; } /* TODO */
    ;

TemplatedConstructor
    : TOK_THIS TemplateParameters Parameters Constraintopt FunctionBody { $$ = 0; } /* TODO */
    ;

ClassTemplateDeclaration
    : TOK_CLASS Identifier TOK_LEFT_PAR TemplateParameterList TOK_RIGHT_PAR Constraintopt BaseClassListopt ClassBody { $$ = 0; } /* TODO */
    | TOK_CLASS Identifier TOK_LEFT_PAR TemplateParameterList TOK_RIGHT_PAR BaseClassListopt Constraintopt ClassBody { $$ = 0; } /* TODO */
    ;

StructTemplateDeclaration
    : TOK_STRUCT Identifier TOK_LEFT_PAR TemplateParameterList TOK_RIGHT_PAR Constraintopt StructBody { $$ = 0; } /* TODO */
    ;

UnionTemplateDeclaration
    : TOK_UNION Identifier TOK_LEFT_PAR TemplateParameterList TOK_RIGHT_PAR Constraintopt StructBody { $$ = 0; } /* TODO */
    ;

InterfaceTemplateDeclaration
    : TOK_INTERFACE Identifier TOK_LEFT_PAR TemplateParameterList TOK_RIGHT_PAR Constraintopt BaseInterfaceListopt InterfaceBody { $$ = 0; } /* TODO */
    ;

Constraint
    : TOK_IF TOK_LEFT_PAR ConstraintExpression TOK_RIGHT_PAR { $$ = 0; } /* TODO */
    ;

ConstraintExpression
    : Expression { $$ = 0; } /* TODO */
    ;

/* ***** Template Mixins ***** */

TemplateMixinDeclaration
    : TOK_MIXIN TOK_TEMPLATE TemplateIdentifier TemplateParameters Constraintopt { $$ = 0; } /* TODO */
    | TOK_LEFT_BRACE DeclDefs TOK_RIGHT_BRACE { $$ = 0; } /* TODO */
    ;

TemplateMixin
    : TOK_MIXIN MixinTemplateName TemplateArgumentsopt MixinIdentifieropt TOK_SEMICOLON { $$ = 0; } /* TODO */
    ;

MixinTemplateName
    : TOK_DOT QualifiedIdentifierList { $$ = 0; } /* TODO */
    | QualifiedIdentifierList { $$ = 0; } /* TODO */
    | Typeof TOK_DOT QualifiedIdentifierList { $$ = 0; } /* TODO */
    ;

QualifiedIdentifierList
    : Identifier { $$ = 0; } /* TODO */
    | Identifier TOK_DOT QualifiedIdentifierList { $$ = 0; } /* TODO */
    | TemplateInstance TOK_DOT QualifiedIdentifierList { $$ = 0; } /* TODO */
    ;

MixinIdentifieropt
    : { $$ = 0; } /* TODO */
    | MixinIdentifier { $$ = 0; } /* TODO */
    ;

MixinIdentifier
    : Identifier { $$ = 0; } /* TODO */
    ;

MixinDeclaration
    : TOK_MIXIN TOK_LEFT_PAR AssignExpression TOK_RIGHT_PAR TOK_SEMICOLON { $$ = 0; } /* TODO */
    ;


/* ***** Conditional Compilation ***** */

/* TODO !!! */

/* ***** Traits ***** */

/* TODO !!! */

/* ***** Unit Tests ***** */

UnitTest
    : TOK_UNITTEST FunctionBody { $$ = 0; } /* TODO */
    ;

/* ***** Inline Assembler ***** */

/* TODO !!! */







%%
