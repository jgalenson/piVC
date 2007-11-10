
%{
  open Printf
%}

%token T_Define
%token T_Declare
%token T_Pre T_Post
%token T_Pre T_Post
%token T_Bool T_Void T_Int T_Float T_String
%token <int> T_IntConstant
%token <float> T_FloatConstant
%token <bool> T_BoolConstant
%token <string> T_Identifier
%token T_True T_False T_Null
%token T_Dims
%token T_LessEqual T_GreaterEqual T_Equal T_NotEqual
%token T_And T_Or
%token T_While T_For
%token T_Else
%token T_If
%token T_Return T_Break
%token T_Break T_Return
%token T_At T_Plus T_Minus T_Star T_Slash T_Less T_Greater T_Assign T_Not T_Semicolon T_Comma T_Period T_LSquareBracket T_RSquareBracket T_LParen T_RParen T_LCurlyBracket T_RCurlyBracket T_QuestionMark
%token T_Unknown
%token T_EOF




//%type <int>  DeclList //change these
//%type <int>      Decl //change these

%nonassoc T_Assign
%left T_Or
%left T_And
%nonassoc T_Equal T_NotEqual
%nonassoc T_Less T_Greater T_LessEqual T_GreaterEqual
%left T_Plus T_Minus
%left T_Star T_Slash '%'
%right T_Not UnaryMinus
%left T_LSquareBracket T_Period

%nonassoc T_If
%nonassoc T_Else


%start main             /* the entry point */
%type <Ast.program> main

%%



main      :    DeclList T_EOF 
               {
		 Ast.create_program($1);
               }
          ;

DeclList  :    DeclList Decl        {$1 @ [$2]}
          |    Decl                 {[$1]}
          ;

Decl      :    VarDecl              {Ast.VarDecl ($1)} 
          |    FnDecl               {Ast.FnDecl  ($1)}
          ;

Type      : T_Int                   {Ast.Int}
          | T_Float                 {Ast.Float}
          | T_Bool                  {Ast.Bool}
          | T_Identifier            {Ast.Ident($1)}
          | Type T_Dims             {Ast.Array($1)}
          ;

FnDecl    : Type T_Identifier T_LParen FormalsOrEmpty T_RParen StmtBlock     {Ast.create_fnDecl $2 $1}
          | T_Void T_Identifier T_LParen FormalsOrEmpty T_RParen StmtBlock   {Ast.create_fnDecl $2 Ast.Void}
          ;

FormalsOrEmpty : Formals {}
               |         {}
               ;

Formals   : Var                {}
          | Formals T_Comma Var    {}
          ;

StmtBlock  : T_LCurlyBracket VarDeclListAndFirstStatement StmtList T_RCurlyBracket {} //has at least one statement
           | T_LCurlyBracket VarDeclList T_RCurlyBracket                           {} //has no statements
;


StmtList : StmtList Stmt {}
         | {}
;

VarDeclList : VarDeclList VarDecl {}
            | {}
;


VarDeclListAndFirstStatement : VarDeclList Stmt {}
;

          
VarDecl   : Var T_Semicolon                 {Ast.create_varDecl "name_goes_here"}
          ;

Var       : Type T_Identifier       {}
          ;


Stmt       : OptionalExpr T_Semicolon {}
           | IfStmt {}
           | WhileStmt {}
           | ForStmt {}
           | BreakStmt {}
           | ReturnStmt {}
           | StmtBlock {}
;

/* Adding the %prec attribute gives the else higher precedence, so it always binds with an else if possible*/
IfStmt       : T_If T_LParen Expr T_RParen Stmt T_Else Stmt %prec T_Else {}
             | T_If T_LParen Expr T_RParen Stmt %prec T_If {}
;

WhileStmt  : T_While T_LParen Expr T_RParen Stmt {}
;

ForStmt    : T_For T_LParen OptionalExpr T_Semicolon Expr T_Semicolon OptionalExpr T_RParen Stmt {}
;

ReturnStmt : T_Return OptionalExpr T_Semicolon {}
;

OptionalExpr : Expr {}
             | {}
;

BreakStmt : T_Break T_Semicolon {}
;

Expr     : LValue T_Assign Expr {}
         | Constant {}
         | LValue {}
         | Call {}
         | T_LParen Expr T_RParen {}
         | Expr T_Plus Expr {}
         | Expr T_Minus Expr {}
         | Expr T_Star Expr {}
         | Expr T_Slash Expr {}
         | Expr '%' Expr {}
         | T_Minus Expr %prec UnaryMinus {}
         | Expr T_Less Expr {}
         | Expr T_LessEqual Expr {}
         | Expr T_Greater Expr {}
         | Expr T_GreaterEqual Expr {}
         | Expr T_Equal Expr {}
         | Expr T_NotEqual Expr {}
         | Expr T_And Expr {}
         | Expr T_Or Expr {}
         | T_Not Expr {}
;

LValue   : T_Identifier                          {}
         | Expr T_Period T_Identifier                 {}
         | Expr T_LSquareBracket Expr T_RSquareBracket                     {}
;

Call     : T_Identifier T_LParen Actuals T_RParen          {}
         | Expr T_Period T_Identifier T_LParen Actuals T_RParen {}
;

ExprList : ExprList T_Comma Expr  {}
         | Expr           {}
;

Actuals  : ExprList       {}
         |                {}
;

Constant : T_IntConstant    {}
         | T_FloatConstant  {}
         | T_BoolConstant   {}
         | T_Null           {}
;

%%
