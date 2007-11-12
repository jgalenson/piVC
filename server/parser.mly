
%{
  open Printf
  open Ast
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

FnDecl    : Type T_Identifier T_LParen FormalsOrEmpty T_RParen StmtBlock     {Ast.create_fnDecl $2 $4 $1 $6}
          | T_Void T_Identifier T_LParen FormalsOrEmpty T_RParen StmtBlock   {Ast.create_fnDecl $2 $4 Ast.Void $6}
          ;

FormalsOrEmpty : Formals {$1}
               |         {[]}
               ;

Formals   : Var                    {[$1]}
          | Formals T_Comma Var    {$1 @ [$3]}
          ;

StmtBlock  : T_LCurlyBracket VarDeclListAndFirstStatement StmtList T_RCurlyBracket {$2 @ $3} //has at least one statement
           | T_LCurlyBracket VarDeclList T_RCurlyBracket                           {$2} //has no statements
;


StmtList : StmtList Stmt {$1 @ $2}
         | {[]}
;

VarDeclList : VarDeclList VarDecl {$1 @ [Ast.varDeclStmt $2]}
            | {[]}
;


VarDeclListAndFirstStatement : VarDeclList Stmt { $1 @ $2 }
;

          
VarDecl   : Var T_Semicolon                 { $1 }
          ;

Var       : Type T_Identifier               { Ast.create_varDecl $1 $2 }
          ;


Stmt       : OptionalExpr T_Semicolon { [Ast.exprStmt $1] }
           | IfStmt { [Ast.unimplementedStmt $1] }
           | WhileStmt { [Ast.unimplementedStmt $1] }
           | ForStmt { [Ast.unimplementedStmt $1] }
           | BreakStmt { [Ast.unimplementedStmt $1] }
           | ReturnStmt { [Ast.unimplementedStmt $1] }
           | StmtBlock { $1 }
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

OptionalExpr : Expr { $1 }
             | { Ast.emptyExpr }
;

BreakStmt : T_Break T_Semicolon {}
;

Expr     : LValue T_Assign Expr { Ast.unimplementedExprB $3 }
         | Constant { Ast.unimplementedExprA $1 }
         | LValue { Ast.unimplementedExprA $1 }
         | Call { Ast.unimplementedExprA $1 }
         | T_LParen Expr T_RParen { $2 }
         | Expr T_Plus Expr { Ast.plus $1 $3 }
         | Expr T_Minus Expr { Ast.unimplementedExprB $1 }
         | Expr T_Star Expr { Ast.unimplementedExprB $1 }
         | Expr T_Slash Expr { Ast.unimplementedExprB $1 }
         | Expr '%' Expr { Ast.unimplementedExprB $1 }
         | T_Minus Expr %prec UnaryMinus { Ast.unimplementedExprB $2 }
         | Expr T_Less Expr { Ast.unimplementedExprB $1 }
         | Expr T_LessEqual Expr { Ast.unimplementedExprB $1 }
         | Expr T_Greater Expr { Ast.unimplementedExprB $1 }
         | Expr T_GreaterEqual Expr { Ast.unimplementedExprB $1 }
         | Expr T_Equal Expr { Ast.unimplementedExprB $1 }
         | Expr T_NotEqual Expr { Ast.unimplementedExprB $1 }
         | Expr T_And Expr { Ast.unimplementedExprB $1 }
         | Expr T_Or Expr { Ast.unimplementedExprB $1 }
         | T_Not Expr { Ast.unimplementedExprB $2 }
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
