
%{
open Printf
open Ast
open Lexing

exception UnexpectedStmt

let r_parens_locs = ref []
let last_l_paren_of_call_args_loc = ref ({pos_fname="";pos_lnum=0;pos_bol=0;pos_cnum=0;})

let get_last_r_paren_before_call_args () = 
  if last_l_paren_of_call_args_loc.contents.pos_cnum = 0 then
    None
  else
    begin
      let loc_is_before_last_call_loc loc = 
        loc.pos_cnum < last_l_paren_of_call_args_loc.contents.pos_cnum
      in
        try
          Some(List.find loc_is_before_last_call_loc (List.rev r_parens_locs.contents))
        with ex -> None
    end
       

type temp_expr =
  | Assign of Ast.location * Ast.lval * temp_expr
  | Constant of Ast.location * Ast.constant
  | LValue of Ast.location * Ast.lval
  | TempCall of Ast.location * temp_expr * temp_expr list
  | TempVarDeclAndAssign of Ast.stmt * expr
  | Plus of Ast.location * temp_expr * temp_expr
  | Minus of Ast.location * temp_expr * temp_expr
  | Times of Ast.location * temp_expr * temp_expr
  | Div of Ast.location * temp_expr * temp_expr
  | IDiv of Ast.location * temp_expr * temp_expr	
  | Mod of Ast.location * temp_expr * temp_expr
  | UMinus of Ast.location * temp_expr
  | ForAll of location * varDecl list * temp_expr
  | Exists of location * varDecl list * temp_expr
  | LT of Ast.location * temp_expr * temp_expr
  | ArrayUpdate of location * temp_expr * temp_expr * temp_expr
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
  | NewArray of Ast.location * varType * temp_expr
  | EmptyExpr

let loc start_token end_token = create_location (Parsing.rhs_start_pos start_token) (Parsing.rhs_end_pos end_token)

(*Condenses a list of statements into either a single statement or statement block.*)
let condense_stmt_list sl = match List.length sl with
    1 -> List.hd sl
  | 2 -> 
    begin
      match List.nth sl 1 with
          StmtBlock(loc,stmts) -> StmtBlock(create_location (location_of_stmt (List.nth sl 0)).loc_start loc.loc_end, List.hd sl::stmts)
        | _ -> StmtBlock(create_location (location_of_stmt (List.nth sl 0)).loc_start (location_of_stmt (List.nth sl 1)).loc_end, sl)    
    end
  | _ -> raise UnexpectedStmt


let tempVarDeclAndAssign_from_stmt_list stmts = 
  let exp = match (List.nth stmts 1) with
      Ast.Expr(loc, exp) -> exp
    | _ -> raise UnexpectedStmt
  in
  TempVarDeclAndAssign((List.nth stmts 0), exp)

let identifier_of_expression expr =
  match expr with
    | LValue(loc, l) -> (match l with
                             Ast.NormLval(loc, identifier) -> identifier
                           | _ -> raise Parsing.Parse_error
                        )
    | _ -> raise Parsing.Parse_error

let assign_expr_of_stmt s = match s with
    Expr(loc, expr) -> expr
  | _ -> raise UnexpectedStmt

let rec expr_list_of_temp_expr_list = function
     | [] -> []
     | x :: l -> expr_from_temp_expr false x :: expr_list_of_temp_expr_list l

and condition_from_call_interior el = 
  match List.hd el with
      TempVarDeclAndAssign(s,e) -> (Some(s), e::List.tl (expr_list_of_temp_expr_list el))
    | _ -> (None, expr_list_of_temp_expr_list el)

and location_of_temp_expr = function
    | Assign (loc,l, e) -> loc
    | Constant (loc,c) -> loc
    | LValue (loc,l) -> loc
    | TempVarDeclAndAssign(l,s) -> location_union (location_of_stmt l) (location_of_expr s)
    | TempCall (loc,s, el) -> loc
    | Plus (loc,t1, t2) -> loc
    | Minus (loc,t1, t2) -> loc
    | Times (loc,t1, t2) -> loc
    | Div (loc,t1, t2) -> loc
    | IDiv (loc,t1, t2) -> loc
    | Mod (loc,t1, t2) -> loc
    | UMinus (loc,t) -> loc
    | ForAll (loc,decls,e) -> loc
    | Exists (loc,decls,e) -> loc
    | ArrayUpdate (loc,exp,assign_to,assign_val) -> loc
    | LT (loc,t1, t2) -> loc
    | LE (loc,t1, t2) -> loc
    | GT (loc,t1, t2) -> loc
    | GE (loc,t1, t2) -> loc
    | EQ (loc,t1, t2) -> loc
    | NE (loc,t1, t2) -> loc
    | And (loc,t1, t2) -> loc
    | Or (loc,t1, t2) -> loc
    | Not (loc,t) -> loc
    | Length (loc, t) -> loc
    | Iff (loc,t1, t2) -> loc
    | Implies (loc,t1, t2) -> loc
    | NewArray(loc,t,e) -> loc
    | EmptyExpr -> gdl ()

and condition_from_temp_expr = function
    | Assign (loc,l, e) -> raise Parsing.Parse_error
    | Constant (loc,c) -> raise Parsing.Parse_error
    | LValue (loc,l) -> raise Parsing.Parse_error
    | TempVarDeclAndAssign(l,s) -> raise Parsing.Parse_error
    | TempCall (loc,s, el) -> condition_from_call_interior el
    | Plus (loc,t1, t2) -> condition_from_temp_expr t2
    | Minus (loc,t1, t2) -> condition_from_temp_expr t2
    | Times (loc,t1, t2) -> condition_from_temp_expr t2
    | Div (loc,t1, t2) -> condition_from_temp_expr t2
    | IDiv (loc,t1, t2) -> condition_from_temp_expr t2
    | Mod (loc,t1, t2) -> condition_from_temp_expr t2
    | UMinus (loc,t) -> condition_from_temp_expr t
    | ForAll (loc,decls,e) -> condition_from_temp_expr e
    | Exists (loc,decls,e) -> condition_from_temp_expr e
    | ArrayUpdate (loc,exp,assign_to,assign_val) -> raise Parsing.Parse_error
    | LT (loc,t1, t2) -> condition_from_temp_expr t2
    | LE (loc,t1, t2) -> condition_from_temp_expr t2
    | GT (loc,t1, t2) -> condition_from_temp_expr t2
    | GE (loc,t1, t2) -> condition_from_temp_expr t2
    | EQ (loc,t1, t2) -> condition_from_temp_expr t2
    | NE (loc,t1, t2) -> condition_from_temp_expr t2
    | And (loc,t1, t2) -> condition_from_temp_expr t2
    | Or (loc,t1, t2) -> condition_from_temp_expr t2
    | Not (loc,t) -> condition_from_temp_expr t
    | Length (loc, t) -> raise Parsing.Parse_error
    | Iff (loc,t1, t2) -> condition_from_temp_expr t2
    | Implies (loc,t1, t2) -> condition_from_temp_expr t2
    | NewArray(loc,t,e) -> raise Parsing.Parse_error
    | EmptyExpr -> raise Parsing.Parse_error


(* "has_condition" reflects whether this expression is followed by a while loop condition.
   We don't know whether the condition is a condition or an argument of a function call until
   after we're well advanced in the parsing, so we put the condition in the expression. When we
   reach the end, we yank out the final condition and put it as the loop condition.
*)
and expr_from_temp_expr has_condition expr =
  let location_to_truncate_to = ref None in
  let rec efte has_condition expr = 
    match expr with
      | Assign (loc,l, e) -> Ast.Assign(loc,l,efte has_condition e)
      | Constant (loc,c) -> Ast.Constant(loc,c)
      | LValue (loc,l) -> Ast.LValue(loc, l)
      | TempCall (loc,s, el) -> (
          match has_condition with
              true ->
                begin 
                  location_to_truncate_to :=
                    (*This is a hack to include closing right parens in the location.
                      These right parens are not officially part of the expr,
                      however we still want them to be highlighted. Normally, this
                      would occur automatically, because the expr in parens would be inside
                      another larger expr which would have a location that includes the parens.
                      However, because we are simply using the location of the expr immediately before
                      the call args, there may be closing right parens between this expr and the call
                      args.
                    *)
                    begin
                      match get_last_r_paren_before_call_args () with
                          None -> Some((location_of_temp_expr s).loc_end);
                        | Some(loc) ->
                            begin
                              if (location_of_temp_expr s).loc_end.pos_cnum < loc.pos_cnum then 
                                Some(loc)
                              else Some((location_of_temp_expr s).loc_end)
                            end
                    end;
                  efte false s
                end
            | false -> Ast.Call(loc,identifier_of_expression s, expr_list_of_temp_expr_list el)
        )
      | TempVarDeclAndAssign(s,e) -> e
      | Plus (loc,t1, t2) -> Ast.Plus(loc, efte false t1, efte has_condition t2)
      | Minus (loc,t1, t2) -> Ast.Minus(loc, efte false t1, efte has_condition t2)
      | Times (loc,t1, t2) -> Ast.Times(loc, efte false t1, efte has_condition t2)
      | Div (loc,t1, t2) -> Ast.Div(loc, efte false t1, efte has_condition t2)
      | IDiv (loc,t1, t2) -> Ast.IDiv(loc, efte false t1, efte has_condition t2)
      | Mod (loc,t1, t2) -> Ast.Mod(loc, efte false t1, efte has_condition t2)
      | UMinus (loc,t) -> Ast.UMinus(loc, efte has_condition t)
      | ForAll (loc,decls,e) -> Ast.ForAll(loc, decls, efte has_condition e)
      | Exists (loc,decls,e) -> Ast.Exists(loc, decls, efte has_condition e)
      | ArrayUpdate (loc,exp,assign_to,assign_val) -> Ast.ArrayUpdate (loc, efte false exp, efte false assign_to, efte false assign_val)
      | LT (loc,t1, t2) -> Ast.LT(loc, efte false t1, efte has_condition t2)
      | LE (loc,t1, t2) -> Ast.LE(loc, efte false t1, efte has_condition t2)
      | GT (loc,t1, t2) -> Ast.GT(loc, efte false t1, efte has_condition t2)
      | GE (loc,t1, t2) -> Ast.GE(loc, efte false t1, efte has_condition t2)
      | EQ (loc,t1, t2) -> Ast.EQ(loc, efte false t1, efte has_condition t2)
      | NE (loc,t1, t2) -> Ast.NE(loc, efte false t1, efte has_condition t2)
      | And (loc,t1, t2) -> Ast.And(loc, efte false t1, efte has_condition t2)
      | Or (loc,t1, t2) -> Ast.Or(loc, efte false t1, efte has_condition t2)
      | Not (loc,t) -> Ast.Not(loc, efte has_condition t)
      | Length (loc, t) -> Ast.Length(loc, efte false t)
      | Iff (loc,t1, t2) -> Ast.Iff(loc, efte false t1, efte has_condition t2)
      | Implies (loc,t1, t2) -> Ast.Implies(loc, efte false t1, efte has_condition t2)
      | NewArray(loc,t,e) -> Ast.NewArray(loc, t, efte false e)
      | EmptyExpr -> Ast.EmptyExpr
  in
  let new_expr = efte has_condition expr in
    match location_to_truncate_to.contents with
        None -> new_expr
      | Some(loc) -> truncate_loc_of_expr new_expr loc
          

%}

%token T_Define
%token T_Declare
%token T_Pre T_Post
%token T_Pre T_Post
%token T_Bool T_Void T_Int T_Float T_String T_Predicate
%token T_New
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
%token T_Typedef T_Class
%token T_Assert T_Termination T_Bar T_Div T_Mod T_Plus T_Minus T_Star T_Slash T_Less T_Greater T_Assign T_Not T_Semicolon T_Comma T_Period T_LSquareBracket T_RSquareBracket T_LParen T_RParen T_LCurlyBracket T_RCurlyBracket T_QuestionMark T_LeftArrow T_Colon
%token T_ForAll T_Exists T_Iff T_Implies T_Pre T_Post
%token T_Unknown
%token T_EOF

%left VeryLowPrecedence
%nonassoc T_Assign
%nonassoc T_Iff T_Implies
%left T_Or
%left T_And
%left T_ForAll T_Exists
%nonassoc T_Equal T_NotEqual
%nonassoc T_Less T_Greater T_LessEqual T_GreaterEqual
%left T_Plus T_Minus
%left T_Semicolon
%left T_Star T_Slash T_Div T_Mod
%right T_Not UnaryMinus
%left MediumPrecedence
%left T_LSquareBracket T_Period T_LCurlyBracket
%left T_LParen
%left T_Termination
%left VeryHighPrecedence
%left VeryVeryHighPrecedence



%nonassoc T_If
%nonassoc T_Else


%start main             /* the entry point */
%type <Ast.program> main
%type <temp_expr> Expr
%type <temp_expr> OptionalExpr
%type <temp_expr> AnnotationExpr
%type <temp_expr> Annotation

%%



main      :    DeclList T_EOF
               {
		 Ast.create_program $1 (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 1));
               }
          ;

DeclList  :    DeclList Decl        { $1 @ [$2] }
          |                     { [] }
          ;

Decl      :    VarDeclOutsideOfFunc              { Ast.VarDecl ($1.location_vd, $1) } 
          |    FnDecl               { Ast.FnDecl ($1.location_fd, $1)  }
          |    Predicate            { Ast.Predicate ($1.location_p, $1)}
          |    ClassDecl            { Ast.ClassDecl ($1.location_cd, $1)}
          ;


/*Note: if this works, you should change DeclList to use the same pattern*/
DeclListInsideClass  :    DeclListInsideClass DeclInsideClass        { $1 @ [$2] }
                     |                                               { [] }
                     ;

DeclInsideClass :    VarDecl              { Ast.VarDecl ($1.location_vd, $1) } 
                |    FnDecl               { Ast.FnDecl ($1.location_fd, $1)  }
                |    Predicate            { Ast.Predicate ($1.location_p, $1)}
                ;


Type      : T_Int                   { Ast.Int  (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 1) )}
          | T_Float                 { Ast.Float(create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 1) )}
          | T_Bool                  { Ast.Bool (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 1) )}
          | Identifier              { Ast.Identifier($1, (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 1) )) }
          | Type T_Dims             { Ast.Array($1, create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2))}
          ;


Identifier : T_Identifier { (Ast.create_identifier $1 (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 1)))}


FnDecl    : BeforeFunc Type Identifier T_LParen FormalsOrEmpty T_RParen StmtBlock     {
              let (pre,post,term) = $1 in
                Ast.create_fnDecl $3 $5 $2 $7 (create_precondition (expr_from_temp_expr false pre)) (create_postcondition (expr_from_temp_expr false post)) term (loc 2 6)
          }
          | BeforeFunc T_Void Identifier T_LParen FormalsOrEmpty T_RParen StmtBlock   {
              let (pre,post,term) = $1 in
                Ast.create_fnDecl $3 $5 (Ast.Void(loc 2 2)) $7 (create_precondition (expr_from_temp_expr false pre)) (create_postcondition (expr_from_temp_expr false post)) term (loc 2 6)
          }
          ;

ClassDecl : T_Class Identifier T_LCurlyBracket DeclListInsideClass T_RCurlyBracket{
  {className=$2;members=$4;location_cd=loc 1 2}
}

BeforeFunc : 
          | Termination AnnotationPre AnnotationPost {$2, $3, Some($1)} 
          | AnnotationPre AnnotationPost {$1, $2, None} 
          | AnnotationPre Termination AnnotationPost {$1, $3, Some($2)} 
          | AnnotationPre AnnotationPost Termination {$1, $2, Some($3)} 
          | Termination AnnotationPost AnnotationPre {$3, $2, Some($1)} 
          | AnnotationPost AnnotationPre {$2, $1, None} 
          | AnnotationPost Termination AnnotationPre {$3, $1, Some($2)} 
          | AnnotationPost AnnotationPre Termination {$2, $1, Some($3)} 
 
BeforeBranch :
          | Termination AnnotationWithLabel 
              {(create_annotation (expr_from_temp_expr true (fst $2)) (snd $2), Some($1), condition_from_temp_expr (fst $2))}
          | AnnotationWithLabel Termination T_LParen CallInterior T_RParen /*the "call" is an EmptyExpr followed by the actuals, which are actually the components of the for or component of while stmt*/
              {(create_annotation (expr_from_temp_expr false (fst $1)) (snd $1), Some($2), condition_from_call_interior $4)}
          | AnnotationWithLabel %prec MediumPrecedence
              {(create_annotation (expr_from_temp_expr true (fst $1)) (snd $1), None, condition_from_temp_expr (fst $1))}

 
Predicate : T_Predicate Identifier T_LParen FormalsOrEmpty T_RParen T_Assign Expr T_Semicolon { {predName=$2;formals_p=$4;expr=(expr_from_temp_expr false $7);location_p=(loc 1 7)} }
          ;

FormalsOrEmpty : Formals { $1 }
               |         { [] }
               ;


Var       : Type Identifier               { Ast.create_varDecl $1 $2 (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2))}
          ;



VarOutSideOfFunc       : Type Identifier               { Ast.create_varDecl $1 $2 (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2))}
          | Type Identifier T_Assign T_QuestionMark { Ast.create_annotation_free_varDecl $1 $2 (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2))}
          ;



ParamVar  : Type Identifier               { Ast.create_param_varDecl $1 $2 (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2))}
          ;


Formals   : ParamVar                    { [$1] }
          | Formals T_Comma ParamVar    { $1 @ [$3] }
          ;

StmtBlock  : T_LCurlyBracket StmtList T_RCurlyBracket { Ast.StmtBlock((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)), $2) }
;


StmtList : StmtList Stmt { $1 @ $2 }
         | { [] }
;

VarDeclOutsideOfFunc   : VarOutSideOfFunc T_Semicolon                 { $1 }
          ;
          
VarDecl   : Var T_Semicolon                 { $1 }
          ;


Stmt       : VarDecl { [Ast.VarDeclStmt($1.location_vd, $1)] }
           | VarDeclAndAssign T_Semicolon { $1 }
           | OptionalExpr T_Semicolon {[Ast.Expr((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2)),expr_from_temp_expr false $1)] }
           | IfStmt { [$1] }
           | WhileStmt { [$1] }
           | ForStmt { [$1] }
           | BreakStmt { [$1] }
           | ReturnStmt { [$1] }
	   | AssertStmt { [$1] }
           | StmtBlock { [$1] }
;

VarDeclAndAssign : Var T_Assign Expr {
                     [
                       Ast.VarDeclStmt($1.location_vd, $1);
                       let assign_rhs_expr = expr_from_temp_expr false $3 in
                       let loc = create_location ($1.varName.location_id.loc_start) ((location_of_expr assign_rhs_expr).loc_end) in
                         Ast.Expr(loc, Ast.Assign(loc, Ast.NormLval($1.varName.location_id, $1.varName), assign_rhs_expr))
                     ]
}
;

/* Adding the %prec attribute gives the else higher precedence, so it always binds with an else if possible*/
IfStmt       : T_If T_LParen Expr T_RParen Stmt T_Else Stmt %prec T_Else { Ast.IfStmt ( (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 7)), expr_from_temp_expr false $3, condense_stmt_list $5, condense_stmt_list $7) }
             | T_If T_LParen Expr T_RParen Stmt %prec T_If { Ast.IfStmt ( (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 5)), expr_from_temp_expr false $3, condense_stmt_list $5, EmptyStmt) }
;


WhileStmt  : T_While BeforeBranch Stmt {
               let (annotation,termination,condition) = $2 in
               let condition = List.nth (snd condition) 0 in
                 Ast.WhileStmt ( loc 1 3, condition, condense_stmt_list $3, annotation, termination)
	   }

;



ForStmt      : T_For BeforeBranch Stmt {
                 let (annotation,termination,assign_stmt_and_for_components) = $2 in
                 let for_components = snd assign_stmt_and_for_components in
                 let for_stmt = Ast.ForStmt ( loc 1 3, (List.nth for_components 0), (List.nth for_components 1), (List.nth for_components 2), condense_stmt_list $3, annotation, termination) in
                 let assign_stmt = fst assign_stmt_and_for_components in
                   match assign_stmt with
                       None -> for_stmt
                     | Some(stmt) -> Ast.StmtBlock(Ast.location_of_stmt for_stmt, [stmt;for_stmt])
}
;


Termination : T_Termination T_LParen TerminationArgs T_RParen {Ast.create_ranking_annotation $3 (create_location (Parsing.rhs_start_pos 2) (Parsing.rhs_end_pos 4))}


TerminationArgs   : Expr                    { [expr_from_temp_expr false $1] }
                  | TerminationArgs T_Comma Expr    { $1 @ [expr_from_temp_expr false $3] }
                  ;			

ReturnStmt : T_Return OptionalExpr T_Semicolon {Ast.ReturnStmt ( (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 3)), expr_from_temp_expr false $2) }
;

OptionalExpr : Expr { $1 }
             | { EmptyExpr }
;

BreakStmt : T_Break T_Semicolon { Ast.BreakStmt (create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2)) }
;

AssertStmt : AnnotationWithLabel T_Semicolon { Ast.AssertStmt ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2)), create_annotation (expr_from_temp_expr false (fst $1)) (snd $1)) }
           ;

/*All quantified variables are deemed integers*/
CommaSeperatedListOfUniversalVarDecls : Identifier T_Comma CommaSeperatedListOfUniversalVarDecls {(create_universal_varDecl (Int(get_dummy_location ())) $1 $1.location_id) :: $3}
                                      | Identifier { [create_universal_varDecl (Int(get_dummy_location ())) $1 $1.location_id] }
;
/*All quantified variables are deemed integers*/
CommaSeperatedListOfExistentialVarDecls : Identifier T_Comma CommaSeperatedListOfExistentialVarDecls {(create_existential_varDecl (Int(get_dummy_location ())) $1 $1.location_id) :: $3}
                                        | Identifier { [create_existential_varDecl (Int(get_dummy_location ())) $1 $1.location_id] }
;


LValue   : Identifier                          { Ast.NormLval ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 1)), $1) }
         | Expr T_LSquareBracket Expr T_RSquareBracket  { Ast.ArrayLval ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 4)), expr_from_temp_expr false $1, expr_from_temp_expr false $3) }
         | Identifier T_Period Identifier { Ast.InsideObject (loc 1 3, $1, $3) }

;
/*
Expr T_Period SuccessiveIdents  {
             let ident_list = $3 in
             let rec collapse idents_remaining =
               match List.rev idents_remaining with
                   a :: b -> Ast.LValue(gdl(),InsideObject(gdl(),collapse (List.rev b),a))
                 | [] -> expr_from_temp_expr false $1

             in
             let last_ident = List.hd (List.rev ident_list) in
             let non_last_idents = List.rev (List.tl (List.rev ident_list)) in
               InsideObject(gdl(),collapse non_last_idents,last_ident)
           }
*/

/*
SuccessiveIdents : Identifier %prec VeryLowPrecedence { [$1] }
         | Identifier T_Period SuccessiveIdents { [$1]@$3 }
*/ 

Expr     : LValue T_Assign Expr   { Assign(loc 1 3, $1, $3)}
         | Constant               { Constant (loc 1 1, $1) }
         | LValue                 { LValue (loc 1 1, $1) }
         | Call                   { $1 }
         | T_LParen Expr T_RParen { $2 }
         | Expr T_Plus Expr       { Plus(loc 1 3, $1, $3) }
         | Expr T_Minus Expr      { Minus(loc 1 3, $1, $3) }
         | Expr T_Star Expr       { Times(loc 1 3, $1, $3) }
         | Expr T_Slash Expr      { Div(loc 1 3, $1, $3) }
         | Expr T_Div Expr        { IDiv(loc 1 3, $1, $3) }
         | Expr T_Mod Expr        { Mod (loc 1 3, $1, $3) }
         | T_Minus Expr %prec UnaryMinus { UMinus (loc 1 2, $2) }
         | T_ForAll CommaSeperatedListOfUniversalVarDecls T_Period Expr %prec T_ForAll { ForAll(loc 1 4, $2,$4) }
         | T_Exists CommaSeperatedListOfExistentialVarDecls T_Period Expr %prec T_Exists { Exists(loc 1 4, $2,$4) }             
         | Expr T_LCurlyBracket Expr T_LeftArrow Expr T_RCurlyBracket { ArrayUpdate(loc 1 5, $1, $3, $5) }
         | Expr T_Less Expr       { LT (loc 1 3, $1, $3) }
         | Expr T_LessEqual Expr  { LE (loc 1 3, $1, $3) }
         | Expr T_Greater Expr    { GT (loc 1 3, $1, $3) }
         | Expr T_GreaterEqual Expr { GE (loc 1 3,$1, $3) }
         | Expr T_Equal Expr      { EQ (loc 1 3, $1, $3) }
         | Expr T_NotEqual Expr   { NE (loc 1 3, $1, $3) }
         | Expr T_Iff Expr        { Iff (loc 1 3, $1, $3) }
         | Expr T_Implies Expr    { Implies (loc 1 3, $1, $3) }
         | Expr T_And Expr        { And (loc 1 3,$1, $3) }
         | Expr T_Or Expr         { Or (loc 1 3, $1, $3) }
         | T_Not Expr             { Not (loc 1 2, $2) }
	 | T_Bar Expr T_Bar       { Length (loc 1 3, $2) }
	 | T_New Type T_LSquareBracket Expr T_RSquareBracket { NewArray (loc 1 5,$2, $4) }
;

Call     : Expr T_LParen CallInterior T_RParen { TempCall (loc 1 4,$1,$3) }
;


CallInterior  : Actuals                                       { $1 }
         | OptionalExpr T_Semicolon Expr T_Semicolon OptionalExpr { [$1; $3; $5] }
         | VarDeclAndAssign T_Semicolon Expr T_Semicolon OptionalExpr {[(tempVarDeclAndAssign_from_stmt_list $1); $3; $5] }
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
;


Annotation     : AnnotationLValue T_Assign Annotation   { Assign(loc 1 3, $1, $3)}
         | Constant               { Constant (loc 1 1, $1) }
         | AnnotationLValue  %prec VeryLowPrecedence          { LValue (loc 1 1, $1) }
         | AnnotationCall                   { $1 }
         | T_LParen Annotation T_RParen {r_parens_locs := r_parens_locs.contents @ (*[{pos_fname="";pos_lnum=10;pos_bol=20;pos_cnum=50;}]*)[Parsing.rhs_end_pos 3]; $2 }
         | Annotation T_Plus Annotation       { Plus(loc 1 3, $1, $3) }
         | Annotation T_Minus Annotation      { Minus(loc 1 3, $1, $3) }
         | Annotation T_Star Annotation       { Times(loc 1 3, $1, $3) }
         | Annotation T_Slash Annotation      { Div(loc 1 3, $1, $3) }
         | Annotation T_Div Annotation        { IDiv(loc 1 3, $1, $3) }
         | Annotation T_Mod Annotation        { Mod (loc 1 3, $1, $3) }
         | T_Minus Annotation %prec UnaryMinus { UMinus (loc 1 2, $2) }
         | T_ForAll CommaSeperatedListOfUniversalVarDecls T_Period Annotation %prec T_ForAll { ForAll(loc 1 4, $2,$4) }
         | T_Exists CommaSeperatedListOfExistentialVarDecls T_Period Annotation %prec T_Exists { Exists(loc 1 4, $2,$4) }
         | AnnotationLValue T_LCurlyBracket Annotation T_LeftArrow Annotation T_RCurlyBracket  { ArrayUpdate(loc 1 5, LValue(loc 1 1,$1), $3, $5) }
         | Annotation T_Less Annotation       { LT (loc 1 3, $1, $3) }
         | Annotation T_LessEqual Annotation  { LE (loc 1 3, $1, $3) }
         | Annotation T_Greater Annotation    { GT (loc 1 3, $1, $3) }
         | Annotation T_GreaterEqual Annotation { GE (loc 1 3,$1, $3) }
         | Annotation T_Equal Annotation      { EQ (loc 1 3, $1, $3) }
         | Annotation T_NotEqual Annotation   { NE (loc 1 3, $1, $3) }
         | Annotation T_Iff Annotation        { Iff (loc 1 3, $1, $3) }
         | Annotation T_Implies Annotation    { Implies (loc 1 3, $1, $3) }
         | Annotation T_And Annotation        { And (loc 1 3,$1, $3) }
         | Annotation T_Or Annotation         { Or (loc 1 3, $1, $3) }
         | T_Not Annotation             { Not (loc 1 2, $2) }
	 | T_Bar Annotation T_Bar       { Length (loc 1 3, $2) }
;


AnnotationLValue   : Identifier                          { Ast.NormLval ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 1)), $1) }
         | Annotation T_LSquareBracket Annotation T_RSquareBracket  { Ast.ArrayLval ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 4)), expr_from_temp_expr false $1, expr_from_temp_expr false $3) }
         | Identifier T_Period Identifier { Ast.InsideObject (loc 1 3, $1, $3) }
;

AnnotationCall     : Annotation T_LParen CallInterior T_RParen { last_l_paren_of_call_args_loc := Parsing.rhs_end_pos 2; TempCall (loc 1 4,$1,$3) }
;


AnnotationExpr : Annotation    %prec T_Period                            { $1 }
/*	   | Annotation T_And Annotation         { Ast.And ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2)), $1, $3) }
           | Annotation T_Or Annotation          { Ast.Or ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2)), $1, $3) }
           | Annotation T_Iff Annotation         { Ast.Iff ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2)), $1, $3) }
           | Annotation T_Implies Annotation     { Ast.Implies ((create_location (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2)), $1, $3) }
           | T_Not Annotation                    { Ast.Not ((create_location (Parsing.rhs_start_pos 2) (Parsing.rhs_end_pos 2)), $2) }
	   | T_LParen Annotation T_RParen        { $2 } // Source of shift/reduce conflict says Aaron
*/
	   ;

AnnotationWithLabel : T_Assert Identifier T_Colon AnnotationExpr {($4, Some($2))}
           | T_Assert AnnotationExpr {($2, None)}



AnnotationPre : T_Pre AnnotationExpr {$2}
AnnotationPost : T_Post AnnotationExpr {$2}

/*
LocationOpt : Identifier T_Colon { Some $1 }
	   | T_Colon { None }
;
*/
%%
