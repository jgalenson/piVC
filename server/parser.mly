
%{
  open Printf
  open Ast
  open Global
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

DeclList  :    DeclList Decl        { $1 @ [$2] }
          |    Decl                 { [$1] }
          ;

Decl      :    VarDecl              { Ast.VarDecl ($1) } 
          |    FnDecl               { Ast.FnDecl  ($1) }
          ;

Type      : T_Int                   { Ast.Int (Global.getCurrLocation )}
          | T_Float                 { Ast.Float (Global.getCurrLocation) }
          | T_Bool                  { Ast.Bool (Global.getCurrLocation) }
          | T_Identifier            { Ast.Ident($1, Global.getCurrLocation) }
          | Type T_Dims             { Ast.Array($1, Global.getCurrLocation) }
          ;

FnDecl    : Type T_Identifier T_LParen FormalsOrEmpty T_RParen StmtBlock     { Ast.create_fnDecl $2 $4 $1 $6 }
          | T_Void T_Identifier T_LParen FormalsOrEmpty T_RParen StmtBlock   { Ast.create_fnDecl $2 $4 (Ast.Void (getCurrLocation )) $6 } //TODO: this getCurrLocation isn't going to work
          ;

FormalsOrEmpty : Formals { $1 }
               |         { [] }
               ;

Formals   : Var                    { [$1] }
          | Formals T_Comma Var    { $1 @ [$3] }
          ;

StmtBlock  : T_LCurlyBracket StmtList T_RCurlyBracket { Ast.StmtBlock $2 }
;


StmtList : StmtList Stmt { $1 @ [$2] }
         | { [] }
;
          
VarDecl   : Var T_Semicolon                 { $1 }
          ;

Var       : Type T_Identifier               { Ast.create_varDecl $1 $2 }
          ;

Stmt       : VarDecl { Ast.VarDeclStmt $1 }
           | OptionalExpr T_Semicolon { Ast.Expr $1 }
           | IfStmt { $1 }
           | WhileStmt { $1 }
           | ForStmt { $1 }
           | BreakStmt { $1 }
           | ReturnStmt { $1 }
           | StmtBlock { $1 }
;

/* Adding the %prec attribute gives the else higher precedence, so it always binds with an else if possible*/
IfStmt       : T_If T_LParen Expr T_RParen Stmt T_Else Stmt %prec T_Else { Ast.IfStmt ($3, $5, $7) }
             | T_If T_LParen Expr T_RParen Stmt %prec T_If { Ast.IfStmt ($3, $5, EmptyStmt) }
;

WhileStmt  : T_While T_LParen Expr T_RParen Stmt { Ast.WhileStmt ($3, $5) }
;

ForStmt    : T_For T_LParen OptionalExpr T_Semicolon Expr T_Semicolon OptionalExpr T_RParen Stmt { Ast.ForStmt ($3, $5, $7, $9) }
;

ReturnStmt : T_Return OptionalExpr T_Semicolon { Ast.ReturnStmt ($2) }
;

OptionalExpr : Expr { $1 }
             | { Ast.EmptyExpr }
;

BreakStmt : T_Break T_Semicolon { Ast.BreakStmt }
;

Expr     : LValue T_Assign Expr { Ast.Assign ($1, $3) }
         | Constant { Ast.Constant ($1) }
         | LValue { Ast.LValue ($1) }
         | Call { $1 }
         | T_LParen Expr T_RParen { $2 }
         | Expr T_Plus Expr { Ast.Plus ($1, $3) }
         | Expr T_Minus Expr { Ast.Minus ($1, $3) }
         | Expr T_Star Expr { Ast.Times ($1, $3) }
         | Expr T_Slash Expr { Ast.Div ($1, $3) }
	     /* integerdivision */
         | Expr '%' Expr { Ast.Mod ($1, $3) }
         | T_Minus Expr %prec UnaryMinus { Ast.UMinus ($2) }
         | Expr T_Less Expr { Ast.LT ($1, $3) }
         | Expr T_LessEqual Expr { Ast.LE ($1, $3) }
         | Expr T_Greater Expr { Ast.GT ($1, $3) }
         | Expr T_GreaterEqual Expr { Ast.GE ($1, $3) }
         | Expr T_Equal Expr { Ast.EQ ($1, $3) }
         | Expr T_NotEqual Expr { Ast.NE ($1, $3) }
         | Expr T_And Expr { Ast.And ($1, $3) }
         | Expr T_Or Expr { Ast.Or ($1, $3) }
         | T_Not Expr { Ast.Not ($2) }
;

LValue   : T_Identifier                          { Ast.LvalA ($1) }
/*         | Expr T_Period T_Identifier                 {}*/
         | Expr T_LSquareBracket Expr T_RSquareBracket  { Ast.UnimplementedLval }
;

Call     : T_Identifier T_LParen Actuals T_RParen          { Ast.Call ($1, $3) }
/*         | Expr T_Period T_Identifier T_LParen Actuals T_RParen {}*/
;

ExprList : ExprList T_Comma Expr  { $1 @ [$3] }
         | Expr           { [$1] }
;

Actuals  : ExprList       { $1 }
         |                { [] }
;

Constant : T_IntConstant    { ConstInt ($1) }
         | T_FloatConstant  { ConstFloat ($1) }
         | T_BoolConstant   { ConstBool ($1) }
/*         | T_Null           { $1 }*/
;

%%
