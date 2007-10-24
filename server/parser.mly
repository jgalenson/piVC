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
%token T_If T_Else
%token T_Return T_Break
%token T_Break T_Return
%token T_Plus T_Minus T_Star T_Slash T_Less T_Greater T_Assign T_Not T_Semicolon T_Comma T_Period T_LSquareBracket T_RSquareBracket T_LParen T_RParen T_LCurlyBracket T_RCurlyBracket T_QuestionMark
%token T_Unknown
%token T_EOF




%type <declList>  DeclList 
%type <decl>      Decl

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


%start Program             /* the entry point */
%type <int> Program

%%



Program   :    DeclList             {;}
          ;

DeclList  :    DeclList Decl        {;}
          |    Decl                 {;}
          ;

Decl      :    VarDecl              {;} 
          |    FnDecl               {;}
          ;

Type      : T_Int                   {;}
          | T_Float                 {;}
          | T_Bool                  {;}
          | T_Identifier            {;}
          | Type T_Dims             {;}
          ;

FnDecl    : Type T_Identifier '(' FormalsOrEmpty ')' StmtBlock     {;}
          | T_Void T_Identifier '(' FormalsOrEmpty ')' StmtBlock   {;}
          ;

FormalsOrEmpty : Formals {;}
               |         {;}
               ;

Formals   : Var                {;}
          | Formals ',' Var    {;}
          ;


Fields : Field Fields {;}
       | {;}
       ;


Field : VarDecl {;}
      | FnDecl {;}
;

Prototype : Type T_Identifier '(' Formals ')' ';' {;}
          | T_Void T_Identifier '(' Formals ')' ';' {;}
;

PrototypeList : PrototypeList Prototype {;}
              | {;}
;


StmtBlock  : '{' VarDeclListAndFirstStatement StmtList '}' {;} //has at least one statement
           | '{' VarDeclList '}'                           {;} //has no statements
;


StmtList : StmtList Stmt {;}
         | {;}
;

VarDeclList : VarDeclList VarDecl {;}
            | {;}
;


VarDeclListAndFirstStatement : VarDeclList Stmt {;}
;

          
VarDecl   : Var ';'                 {;}
          ;

Var       : Type T_Identifier       {;}
          ;


Stmt       : OptionalExpr ';' {;}
           | IfStmt {;}
           | WhileStmt {;}
           | ForStmt {;}
           | BreakStmt {;}
           | ReturnStmt {;}
           | StmtBlock {;}
;

IfStmt       : T_If '(' Expr ')' StmtWithoutDanglingElse {;}
             | IfStmtWithElse {;}
;

IfStmtWithElse : T_If '(' Expr ')' StmtWithoutDanglingElse T_Else Stmt {;}
;

StmtWithoutDanglingElse : OptionalExpr ';' {;}
                        | IfStmtWithElse {;}
                        | WhileStmt {;}
                        | ForStmt {;}
                        | BreakStmt {;}
                        | ReturnStmt {;}
                        | StmtBlock {;}
;

WhileStmt  : T_While '(' Expr ')' Stmt {;}
;

ForStmt    : T_For '(' OptionalExpr ';' Expr ';' OptionalExpr ')' Stmt {;}
;

ReturnStmt : T_Return OptionalExpr ';' {;}
;

OptionalExpr : Expr {;}
             | {;}
;

BreakStmt : T_Break ';' {;}
;

Expr     : LValue '=' Expr {;}
         | Constant {;}
         | LValue {;}
         | Call {;}
         | '(' Expr ')' {;}
         | Expr '+' Expr {;}
         | Expr '-' Expr {;}
         | Expr '*' Expr {;}
         | Expr '/' Expr {;}
         | Expr '%' Expr {;}
         | '-' Expr %prec UnaryMinus {;}
         | Expr '<' Expr {;}
         | Expr T_LessEqual Expr {;}
         | Expr '>' Expr {;}
         | Expr T_GreaterEqual Expr {;}
         | Expr T_Equal Expr {;}
         | Expr T_NotEqual Expr {;}
         | Expr T_And Expr {;}
         | Expr T_Or Expr {;}
         | '!' Expr {;}
;

LValue   : T_Identifier                          {;}
         | Expr '.' T_Identifier                 {;}
         | Expr '[' Expr ']'                     {;}
;

Call     : T_Identifier '(' Actuals ')'          {;}
         | Expr '.' T_Identifier '(' Actuals ')' {;}
;

ExprList : ExprList ',' Expr  {;}
         | Expr           {;}
;

Actuals  : ExprList       {;}
         |                {;}
;

Constant : T_IntConstant    {;}
         | T_FloatConstant  {;}
         | T_BoolConstant   {;}
         | T_Null           {;}
;

%%

