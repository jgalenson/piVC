%token T_Define
%token T_Declare
%token T_Pre T_Post
%token T_Pre T_Post
%token T_Bool T_Void
%token <int> T_Int
%token <string> T_Ident
%token T_True T_False
%token T_Dims
%token T_LessEqual T_GreaterEqual T_Equal T_NotEqual
%token T_And T_Or
%token T_While T_For
%token T_If T_Else
%token T_Return T_Break
%token T_Plus T_Minus T_Star T_Slash T_Less T_Greater T_Assign T_Not T_Semicolon T_Comma T_Period T_LSquareBracket T_RSquareBracket T_LParen T_RParen T_LCurlyBracket T_RCurlyBracket T_QuestionMark
%token T_Unknown
%token T_EOF

%start main             /* the entry point */
%type <int> main


%%

main:
| declarations T_EOF		{0(*we have to return an int here. i'm not sure why*)}
;

declarations:
| declaration declarations	{}
|				{}
;

declaration:
| globalVariableDecl		{}
| functionDecl			{}
;

globalVariableDecl:
| T_Define variable T_Assign T_QuestionMark T_Semicolon	{}
;

variable:
| dataType T_Ident		{}

dataType:
| T_Int				{}
| dataType T_Dims		{}
;

functionDecl:
| dataType T_Ident T_LParen formalsOrEmpty T_RParen statementBlock	{}
;

formalsOrEmpty:
| formals			{}
| 				{}
;

formals:
| variable T_Comma		{}
| variable 			{}
;

statementBlock:
| T_LCurlyBracket statements T_RCurlyBracket	{}
| statement	  	     			{}

statements:
| statement statements		{}
|				{}
;

variableDeclaration:
| variable T_Semicolon		{}
;

statement:
| variableDeclaration		{}
| 				{}

%%

