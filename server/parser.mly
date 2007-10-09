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
%token T_Plus T_Minus T_Star T_Slash T_Less T_Greater T_Assign T_Not T_Semicolon T_Comma T_Period T_LSquareBracket T_RSquareBracket T_LParen T_RParen T_LCurlyBracket T_RCurlyBracket
%token T_Unknown
%token T_EOF

/* note from Jason: everything below this comment is a hack
   made to get the lexer working. 
 */
   

%start main
%type <int> main


%%
main:
  T_Int { $1 }
;

expr:
  T_Int { $1 }
;