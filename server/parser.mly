
%{
  open Printf
  open Ast






exception NoCondition
exception ExprNotIdent



type temp_expr =
  | Assign of Ast.location * Ast.lval * temp_expr
  | Constant of Ast.location * Ast.constant
  | LValue of Ast.location * Ast.lval
  | TempCall of Ast.location * temp_expr * temp_expr list
  | Plus of Ast.location * temp_expr * temp_expr
  | Minus of Ast.location * temp_expr * temp_expr
  | Times of Ast.location * temp_expr * temp_expr
  | Div of Ast.location * temp_expr * temp_expr
  | IDiv of Ast.location * temp_expr * temp_expr	
  | Mod of Ast.location * temp_expr * temp_expr
  | UMinus of Ast.location * temp_expr
  | LT of Ast.location * temp_expr * temp_expr
  | LE of Ast.location * temp_expr * temp_expr
  | GT of Ast.location * temp_expr * temp_expr
  | GE of Ast.location * temp_expr * temp_expr
  | EQ of Ast.location * temp_expr * temp_expr
  | NE of Ast.location * temp_expr * temp_expr
  | And of Ast.location * temp_expr * temp_expr
  | Or of Ast.location * temp_expr * temp_expr
  | Not of Ast.location * temp_expr
  | Iff of Ast.location * temp_expr * temp_expr
  | Implies of Ast.location * temp_expr * temp_expr
  | Length of Ast.location * temp_expr
  | EmptyExpr


let identifier_of_expression expr =
  match expr with
    | LValue(loc, l) -> (match l with
                          Ast.NormLval(loc, identifier) -> identifier
                        | _ -> raise ExprNotIdent
                        )
    | _ -> raise ExprNotIdent (*TODO: you should raise a semantic error (not an exception) if the expr is not a non-array ident*)

let rec expr_list_of_temp_expr_list = function
     | [] -> []
     | x :: l -> expr_from_temp_expr false x :: expr_list_of_temp_expr_list l

and condition_from_temp_expr = function
    | Assign (loc,l, e) -> raise NoCondition
    | Constant (loc,c) -> raise NoCondition
    | LValue (loc,l) -> raise NoCondition
    | TempCall (loc,s, el) -> el
    | Plus (loc,t1, t2) -> condition_from_temp_expr t2
    | Minus (loc,t1, t2) -> condition_from_temp_expr t2
    | Times (loc,t1, t2) -> condition_from_temp_expr t2
    | Div (loc,t1, t2) -> condition_from_temp_expr t2
    | IDiv (loc,t1, t2) -> condition_from_temp_expr t2
    | Mod (loc,t1, t2) -> condition_from_temp_expr t2
    | UMinus (loc,t) -> condition_from_temp_expr t
    | LT (loc,t1, t2) -> condition_from_temp_expr t2
    | LE (loc,t1, t2) -> condition_from_temp_expr t2
    | GT (loc,t1, t2) -> condition_from_temp_expr t2
    | GE (loc,t1, t2) -> condition_from_temp_expr t2
    | EQ (loc,t1, t2) -> condition_from_temp_expr t2
    | NE (loc,t1, t2) -> condition_from_temp_expr t2
    | And (loc,t1, t2) -> condition_from_temp_expr t2
    | Or (loc,t1, t2) -> condition_from_temp_expr t2
    | Not (loc,t) -> condition_from_temp_expr t
    | Length (loc, t) -> raise NoCondition
    | Iff (loc,t1, t2) -> condition_from_temp_expr t2
    | Implies (loc,t1, t2) -> condition_from_temp_expr t2
    | EmptyExpr -> raise NoCondition

and expr_from_temp_expr has_condition = function
    | Assign (loc,l, e) -> Ast.Assign(loc,l,expr_from_temp_expr has_condition e)
    | Constant (loc,c) -> Ast.Constant(loc,c)
    | LValue (loc,l) -> Ast.LValue(loc, l)
    | TempCall (loc,s, el) -> (
                              match has_condition with
                                 true  -> expr_from_temp_expr false s
                               | false -> Ast.Call(loc,identifier_of_expression s, expr_list_of_temp_expr_list el)
                              )
    | Plus (loc,t1, t2) -> Ast.Plus(loc, expr_from_temp_expr false t1, expr_from_temp_expr has_condition t2)
    | Minus (loc,t1, t2) -> Ast.Minus(loc, expr_from_temp_expr false t1, expr_from_temp_expr has_condition t2)
    | Times (loc,t1, t2) -> Ast.Times(loc, expr_from_temp_expr false t1, expr_from_temp_expr has_condition t2)
    | Div (loc,t1, t2) -> Ast.Div(loc, expr_from_temp_expr false t1, expr_from_temp_expr has_condition t2)
    | IDiv (loc,t1, t2) -> Ast.IDiv(loc, expr_from_temp_expr false t1, expr_from_temp_expr has_condition t2)
    | Mod (loc,t1, t2) -> Ast.Mod(loc, expr_from_temp_expr false t1, expr_from_temp_expr has_condition t2)
    | UMinus (loc,t) -> Ast.UMinus(loc, expr_from_temp_expr has_condition t)
    | LT (loc,t1, t2) -> Ast.LT(loc, expr_from_temp_expr false t1, expr_from_temp_expr has_condition t2)
    | LE (loc,t1, t2) -> Ast.LE(loc, expr_from_temp_expr false t1, expr_from_temp_expr has_condition t2)
    | GT (loc,t1, t2) -> Ast.GT(loc, expr_from_temp_expr false t1, expr_from_temp_expr has_condition t2)
    | GE (loc,t1, t2) -> Ast.GE(loc, expr_from_temp_expr false t1, expr_from_temp_expr has_condition t2)
    | EQ (loc,t1, t2) -> Ast.EQ(loc, expr_from_temp_expr false t1, expr_from_temp_expr has_condition t2)
    | NE (loc,t1, t2) -> Ast.NE(loc, expr_from_temp_expr false t1, expr_from_temp_expr has_condition t2)
    | And (loc,t1, t2) -> Ast.And(loc, expr_from_temp_expr false t1, expr_from_temp_expr has_condition t2)
    | Or (loc,t1, t2) -> Ast.Or(loc, expr_from_temp_expr false t1, expr_from_temp_expr has_condition t2)
    | Not (loc,t) -> Ast.Not(loc, expr_from_temp_expr has_condition t)
    | Length (loc, t) -> Ast.Length(loc, expr_from_temp_expr false t)
    | Iff (loc,t1, t2) -> Ast.Iff(loc, expr_from_temp_expr false t1, expr_from_temp_expr has_condition t2)
    | Implies (loc,t1, t2) -> Ast.Implies(loc, expr_from_temp_expr false t1, expr_from_temp_expr has_condition t2)
    | EmptyExpr -> Ast.EmptyExpr



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
%token T_Typedef T_Struct
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
%left T_LParen

%nonassoc T_If
%nonassoc T_Else


%start main             /* the entry point */
%type <Ast.program> main
%type <temp_expr> Expr
%type <temp_expr> OptionalExpr
%type <temp_expr> Annotation

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
          | Identifier              { Ast.Identifier($1, (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 1) )) }
          | Type T_Dims             { Ast.Array($1, create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2))}
          ;


Identifier : T_Identifier { (Ast.create_identifier $1 (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 1)))}


FnDecl    : T_Pre Annotation T_Post Annotation Type Identifier T_LParen FormalsOrEmpty T_RParen StmtBlock     { Ast.create_fnDecl $6 $8 $5 $10 (expr_from_temp_expr false $2) (expr_from_temp_expr false $4) (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 6)) }
          | T_Pre Annotation T_Post Annotation T_Void Identifier T_LParen FormalsOrEmpty T_RParen StmtBlock   { Ast.create_fnDecl $6 $8 (Ast.Void (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 1))) $10 (expr_from_temp_expr false $2) (expr_from_temp_expr false $4) (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 6)) }
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
           | OptionalExpr T_Semicolon {Ast.Expr((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2)),expr_from_temp_expr false $1) }
           | IfStmt { $1 }
           | WhileStmt { $1 }
           | ForStmt { $1 }
           | BreakStmt { $1 }
           | ReturnStmt { $1 }
	   | AssertStmt { $1 }
           | StmtBlock { $1 }
;

/* Adding the %prec attribute gives the else higher precedence, so it always binds with an else if possible*/
IfStmt       : T_If T_LParen Expr T_RParen Stmt T_Else Stmt %prec T_Else { Ast.IfStmt ( (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 7)), expr_from_temp_expr false $3, $5, $7) }
             | T_If T_LParen Expr T_RParen Stmt %prec T_If { Ast.IfStmt ( (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 5)), expr_from_temp_expr false $3, $5, EmptyStmt) }
;

WhileStmt  : T_While T_Assert Annotation Stmt {
               let condition = expr_from_temp_expr false (List.nth (condition_from_temp_expr $3) 0) in
               Ast.WhileStmt ( (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 5)), condition, $4, expr_from_temp_expr true $3)
	   }

;/*T_LParen Expr T_RParen*/

ForStmt    : T_For T_Assert Annotation Stmt {
             let for_components = condition_from_temp_expr $3 in
               Ast.ForStmt ( (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 9)), expr_from_temp_expr false (List.nth for_components 0), expr_from_temp_expr false (List.nth for_components 1), expr_from_temp_expr false (List.nth for_components 2), $4, expr_from_temp_expr true $3) }
;/*T_LParen OptionalExpr T_Semicolon Expr T_Semicolon OptionalExpr T_RParen*/

ReturnStmt : T_Return OptionalExpr T_Semicolon {Ast.ReturnStmt ( (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)), expr_from_temp_expr false $2) }
;

OptionalExpr : Expr { $1 }
             | { EmptyExpr }
;

BreakStmt : T_Break T_Semicolon { Ast.BreakStmt (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2)) }
;

AssertStmt : T_Assert Annotation T_Semicolon { Ast.AssertStmt ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2)), expr_from_temp_expr false $2) }
           ;
  
Expr     : LValue T_Assign Expr { Assign ( (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)), $1, $3) }
         | Constant { Constant ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 1)), $1) }
         | LValue { LValue ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 1)), $1) }
         | Call { $1 }
         | T_LParen Expr T_RParen { $2 }
         | Expr T_Plus Expr { Plus((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)), $1, $3) }
         | Expr T_Minus Expr { Minus((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)),$1, $3) }
         | Expr T_Star Expr { Times((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)),$1, $3) }
         | Expr T_Slash Expr { Div((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)),$1, $3) }
         | Expr T_Div Expr { IDiv((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)),$1, $3) }
         | Expr '%' Expr { Mod ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)),$1, $3) }
         | T_Minus Expr %prec UnaryMinus { UMinus ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2)),$2) }
         | Expr T_Less Expr { LT ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)),$1, $3) }
         | Expr T_LessEqual Expr { LE ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)),$1, $3) }
         | Expr T_Greater Expr { GT ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)),$1, $3) }
         | Expr T_GreaterEqual Expr { GE ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)),$1, $3) }
         | Expr T_Equal Expr { EQ ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)),$1, $3) }
         | Expr T_NotEqual Expr { NE ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)),$1, $3) }
         | Expr T_Iff Expr         { Iff ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)), $1, $3) }
         | Expr T_Implies Expr     { Implies ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)), $1, $3) }
         | Expr T_And Expr { And ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)),$1, $3) }
         | Expr T_Or Expr { Or ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)),$1, $3) }
         | T_Not Expr { Not ((create_location (Parsing.rhs_start_pos 2) (Parsing.rhs_end_pos 3)),$2) }
	 | T_Bar Expr T_Bar { Length ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)),$2) }
;

LValue   : Identifier                          { Ast.NormLval ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 1)), $1) }
/*         | Expr T_Period Identifier                 {}*/
         | Identifier T_LSquareBracket Expr T_RSquareBracket  { Ast.ArrayLval ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 1)), $1, expr_from_temp_expr false $3) }
;

Call     : Expr T_LParen Actuals T_RParen %prec T_LParen                                   { TempCall ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 4)),$1, $3) }
         | Expr T_LParen OptionalExpr T_Semicolon Expr T_Semicolon OptionalExpr T_RParen   { TempCall ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 4)),$1, [$3; $5; $7]) }

/*         | Expr T_Period Identifier T_LParen Actuals T_RParen {}*/
;

ExprList : ExprList T_Comma Expr  { $1 @ [$3] }
         | Expr           { [$1] }
;

Actuals  : ExprList       { $1 }
         |                { [] }
;

Constant : T_IntConstant    { ConstInt ((create_location (Parsing.rhs_start_pos 2) (Parsing.rhs_end_pos 1)),$1) }
         | T_FloatConstant  { ConstFloat ((create_location (Parsing.rhs_start_pos 2) (Parsing.rhs_end_pos 1)),$1)}
         | T_True           { ConstBool ((create_location (Parsing.rhs_start_pos 2) (Parsing.rhs_end_pos 2)), true) }
         | T_False          { ConstBool ((create_location (Parsing.rhs_start_pos 2) (Parsing.rhs_end_pos 2)), false) }	     
/*         | T_Null           { $1 }*/
;

Annotation : Expr    %prec T_Period                            { $1 }
/*	   | Annotation T_And Annotation         { Ast.And ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2)), $1, $3) }
           | Annotation T_Or Annotation          { Ast.Or ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2)), $1, $3) }
           | Annotation T_Iff Annotation         { Ast.Iff ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2)), $1, $3) }
           | Annotation T_Implies Annotation     { Ast.Implies ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2)), $1, $3) }
           | T_Not Annotation                    { Ast.Not ((create_location (Parsing.rhs_start_pos 2) (Parsing.rhs_end_pos 2)), $2) }
	   | T_LParen Annotation T_RParen        { $2 } // Source of shift/reduce conflict says Aaron
*/
	   ;

%%
