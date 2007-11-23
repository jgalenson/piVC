
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
%token T_Assert T_Bar T_Div T_Plus T_Minus T_Star T_Slash T_Less T_Greater T_Assign T_Not T_Semicolon T_Comma T_Period T_LSquareBracket T_RSquareBracket T_LParen T_RParen T_LCurlyBracket T_RCurlyBracket T_QuestionMark
%token T_ForAll T_Exists T_Iff T_Implies T_Pre T_Post
%token T_Unknown
%token T_EOF




//%type <int>  DeclList //change these
//%type <int>      Decl //change these

%nonassoc T_Assign
%nonassoc T_Iff T_Implies
%left T_Or
%left T_And
%nonassoc T_Equal T_NotEqual
%nonassoc T_Less T_Greater T_LessEqual T_GreaterEqual
%left T_Plus T_Minus
%left T_Star T_Slash T_Div '%'
%right T_Not UnaryMinus
%left T_LSquareBracket T_Period

%nonassoc T_If
%nonassoc T_Else


%start main             /* the entry point */
%type <Ast.program> main

%%



main      :    DeclList T_EOF 
               {
		 Ast.create_program $1 (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 1));
               }
          ;

DeclList  :    DeclList Decl        { $1 @ [$2] }
          |    Decl                 { [$1] }
          ;

Decl      :    VarDecl              { Ast.VarDecl ($1.location_vd, $1)  } 
          |    FnDecl               { Ast.FnDecl ($1.location_fd, $1) }
          ;

Type      : T_Int                   { Ast.Int  (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 1) )}
          | T_Float                 { Ast.Float(create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 1) )}
          | T_Bool                  { Ast.Bool (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 1) )}
/*          | Identifier              { Ast.Identifier $1 }*/
          | Type T_Dims             { Ast.Array($1, create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2))}
          ;


Identifier : T_Identifier { (Ast.create_identifier $1 (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 1)))}


FnDecl    : T_Pre Annotation T_Post Annotation Type Identifier T_LParen FormalsOrEmpty T_RParen StmtBlock     { Ast.create_fnDecl $6 $8 $5 $10 $2 $4 (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 6)) }
          | T_Pre Annotation T_Post Annotation T_Void Identifier T_LParen FormalsOrEmpty T_RParen StmtBlock   { Ast.create_fnDecl $6 $8 (Ast.Void (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 1))) $10 $2 $4 (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 6)) }
          ;

FormalsOrEmpty : Formals { $1 }
               |         { [] }
               ;


Var       : Type Identifier               { Ast.create_varDecl $1 $2 (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2))}
          ;


Formals   : Var                    { [$1] }
          | Formals T_Comma Var    { $1 @ [$3] }
          ;

StmtBlock  : T_LCurlyBracket StmtList T_RCurlyBracket { Ast.StmtBlock((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)), $2) }
;


StmtList : StmtList Stmt { $1 @ [$2] }
         | { [] }
;
          
VarDecl   : Var T_Semicolon                 { $1 }
          ;

Stmt       : VarDecl { Ast.VarDeclStmt($1.location_vd, $1) }
           | OptionalExpr T_Semicolon {Ast.Expr((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2)),$1) }
           | IfStmt { $1 }
           | WhileStmt { $1 }
           | ForStmt { $1 }
           | BreakStmt { $1 }
           | ReturnStmt { $1 }
	   | AssertStmt { $1 }
           | StmtBlock { $1 }
;

/* Adding the %prec attribute gives the else higher precedence, so it always binds with an else if possible*/
IfStmt       : T_If T_LParen Expr T_RParen Stmt T_Else Stmt %prec T_Else { Ast.IfStmt ( (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 7)), $3, $5, $7) }
             | T_If T_LParen Expr T_RParen Stmt %prec T_If { Ast.IfStmt ( (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 5)), $3, $5, EmptyStmt) }
;

WhileStmt  : T_While T_Assert Annotation T_LParen Expr T_RParen Stmt { Ast.WhileStmt ( (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 5)), $5, $7, $3) }
;

ForStmt    : T_For T_Assert Annotation T_LParen OptionalExpr T_Semicolon Expr T_Semicolon OptionalExpr T_RParen Stmt { Ast.ForStmt ( (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 9)), $5, $7, $9, $11, $3) }
;

ReturnStmt : T_Return OptionalExpr T_Semicolon {Ast.ReturnStmt ( (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)), $2) }
;

OptionalExpr : Expr { $1 }
             | { Ast.EmptyExpr }
;

BreakStmt : T_Break T_Semicolon { Ast.BreakStmt (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2)) }
;

AssertStmt : T_Assert Annotation T_Semicolon { Ast.AssertStmt ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2)), $2) }
           ;
  
Expr     : LValue T_Assign Expr { Ast.Assign ( (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)), $1, $3) }
         | Constant { Ast.Constant ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 1)), $1) }
         | LValue { Ast.LValue ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 1)), $1) }
         | Call { $1 }
         | T_LParen Expr T_RParen { $2 }
         | Expr T_Plus Expr { Ast.Plus((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)), $1, $3) }
         | Expr T_Minus Expr { Ast.Minus((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)),$1, $3) }
         | Expr T_Star Expr { Ast.Times((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)),$1, $3) }
         | Expr T_Slash Expr { Ast.Div((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)),$1, $3) }
         | Expr T_Div Expr { Ast.IDiv((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)),$1, $3) }
         | Expr '%' Expr { Ast.Mod ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)),$1, $3) }
         | T_Minus Expr %prec UnaryMinus { Ast.UMinus ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2)),$2) }
         | Expr T_Less Expr { Ast.LT ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)),$1, $3) }
         | Expr T_LessEqual Expr { Ast.LE ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)),$1, $3) }
         | Expr T_Greater Expr { Ast.GT ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)),$1, $3) }
         | Expr T_GreaterEqual Expr { Ast.GE ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2)),$1, $3) }
         | Expr T_Equal Expr { Ast.EQ ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2)),$1, $3) }
         | Expr T_NotEqual Expr { Ast.NE ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2)),$1, $3) }
         | Expr T_Iff Expr         { Ast.Iff ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2)), $1, $3) }
         | Expr T_Implies Expr     { Ast.Implies ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2)), $1, $3) }
         | Expr T_And Expr { Ast.And ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2)),$1, $3) }
         | Expr T_Or Expr { Ast.Or ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2)),$1, $3) }
         | T_Not Expr { Ast.Not ((create_location (Parsing.rhs_start_pos 2) (Parsing.rhs_end_pos 2)),$2) }
	 | T_Bar Expr T_Bar { Ast.Length ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2)),$2) }
;

LValue   : Identifier                          { Ast.NormLval ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 1)), $1) }
/*         | Expr T_Period Identifier                 {}*/
         | Expr T_LSquareBracket Expr T_RSquareBracket  { Ast.ArrayLval ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 1)), $1, $3) }
;

Call     : Identifier T_LParen Actuals T_RParen          { Ast.Call ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 4)),$1, $3) }
/*         | Expr T_Period Identifier T_LParen Actuals T_RParen {}*/
;

ExprList : ExprList T_Comma Expr  { $1 @ [$3] }
         | Expr           { [$1] }
;

Actuals  : ExprList       { $1 }
         |                { [] }
;

Constant : T_IntConstant    { ConstInt ((create_location (Parsing.rhs_start_pos 2) (Parsing.rhs_end_pos 1)),$1) }
/*         | T_FloatConstant  { ConstFloat ((create_location (Parsing.rhs_start_pos 2) (Parsing.rhs_end_pos 1)),$1) }*/
         | T_True           { ConstBool ((create_location (Parsing.rhs_start_pos 2) (Parsing.rhs_end_pos 2)), true) }
         | T_False          { ConstBool ((create_location (Parsing.rhs_start_pos 2) (Parsing.rhs_end_pos 2)), false) }	     
/*         | T_Null           { $1 }*/
;

Annotation : Expr                                { $1 }
/*	   | Annotation T_And Annotation         { Ast.And ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2)), $1, $3) }
           | Annotation T_Or Annotation          { Ast.Or ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2)), $1, $3) }
           | Annotation T_Iff Annotation         { Ast.Iff ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2)), $1, $3) }
           | Annotation T_Implies Annotation     { Ast.Implies ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2)), $1, $3) }
           | T_Not Annotation                    { Ast.Not ((create_location (Parsing.rhs_start_pos 2) (Parsing.rhs_end_pos 2)), $2) }
	   | T_LParen Annotation T_RParen        { $2 } // Source of shift/reduce conflict says Aaron
*/
	   ;

%%
