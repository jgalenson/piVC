
%{
  open Printf
%}

%token T_Define
%token T_Declare
%token T_Pre T_Post
%token T_Pre T_Post
%token T_Bool T_Void T_Int T_Float
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
%token T_Plus T_Minus T_Star T_Slash T_Less T_Greater T_Assign T_Not T_Semicolon T_Comma T_Period T_LSquareBracket T_RSquareBracket T_LParen T_RParen T_LCurlyBracket T_RCurlyBracket T_QuestionMark
%token T_Unknown
%token T_EOF




//%type <int>  DeclList //change these
//%type <int>      Decl //change these

%nonassoc '='
%left T_Or
%left T_And
%nonassoc T_Equal T_NotEqual
%nonassoc '<' '>' T_LessEqual T_GreaterEqual
%left '+' '-'
%left '*' '/' '%'
%right '!' UnaryMinus
%left '[' '.'

%nonassoc T_If
%nonassoc T_Else


%start main             /* the entry point */
%type <unit> main

%%



main      :    DeclList             {printf("we're done");}
          ;

DeclList  :    DeclList Decl        {}
          |    Decl                 {}
          ;

Decl      :    VarDecl              {} 
          |    FnDecl               {}
          ;

Type      : T_Int                   {}
          | T_Float                 {}
          | T_Bool                  {}
          | T_Identifier            {}
          | Type T_Dims             {}
          ;

FnDecl    : Type T_Identifier '(' FormalsOrEmpty ')' StmtBlock     {}
          | T_Void T_Identifier '(' FormalsOrEmpty ')' StmtBlock   {}
          ;

FormalsOrEmpty : Formals {}
               |         {}
               ;

Formals   : Var                {}
          | Formals ',' Var    {}
          ;

StmtBlock  : '{' VarDeclListAndFirstStatement StmtList '}' {} //has at least one statement
           | '{' VarDeclList '}'                           {} //has no statements
;


StmtList : StmtList Stmt {}
         | {}
;

VarDeclList : VarDeclList VarDecl {}
            | {}
;


VarDeclListAndFirstStatement : VarDeclList Stmt {}
;

          
VarDecl   : Var ';'                 {}
          ;

Var       : Type T_Identifier       {}
          ;


Stmt       : OptionalExpr ';' {}
           | IfStmt {}
           | WhileStmt {}
           | ForStmt {}
           | BreakStmt {}
           | ReturnStmt {}
           | StmtBlock {}
;

/* Adding the %prec attribute gives the else higher precedence, so it always binds with an else if possible*/
IfStmt       : T_If '(' Expr ')' Stmt T_Else Stmt %prec T_Else {}
             | T_If '(' Expr ')' Stmt %prec T_If {}
;

WhileStmt  : T_While '(' Expr ')' Stmt {}
;

ForStmt    : T_For '(' OptionalExpr ';' Expr ';' OptionalExpr ')' Stmt {}
;

ReturnStmt : T_Return OptionalExpr ';' {}
;

OptionalExpr : Expr {}
             | {}
;

BreakStmt : T_Break ';' {}
;

Expr     : LValue '=' Expr {}
         | Constant {}
         | LValue {}
         | Call {}
         | '(' Expr ')' {}
         | Expr '+' Expr {}
         | Expr '-' Expr {}
         | Expr '*' Expr {}
         | Expr '/' Expr {}
         | Expr '%' Expr {}
         | '-' Expr %prec UnaryMinus {}
         | Expr '<' Expr {}
         | Expr T_LessEqual Expr {}
         | Expr '>' Expr {}
         | Expr T_GreaterEqual Expr {}
         | Expr T_Equal Expr {}
         | Expr T_NotEqual Expr {}
         | Expr T_And Expr {}
         | Expr T_Or Expr {}
         | '!' Expr {}
;

LValue   : T_Identifier                          {}
         | Expr '.' T_Identifier                 {}
         | Expr '[' Expr ']'                     {}
;

Call     : T_Identifier '(' Actuals ')'          {}
         | Expr '.' T_Identifier '(' Actuals ')' {}
;

ExprList : ExprList ',' Expr  {}
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


let goParse () =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      Parser.main Lexer.token lexbuf
    done
  with End_of_file -> exit 0
      
let _ = Printexc.print goParse ()