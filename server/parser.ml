type token =
  | T_Define
  | T_Declare
  | T_Pre
  | T_Post
  | T_Bool
  | T_Void
  | T_Int of (int)
  | T_Ident of (string)
  | T_True
  | T_False
  | T_Dims
  | T_LessEqual
  | T_GreaterEqual
  | T_Equal
  | T_NotEqual
  | T_And
  | T_Or
  | T_While
  | T_For
  | T_If
  | T_Else
  | T_Return
  | T_Break
  | T_Plus
  | T_Minus
  | T_Star
  | T_Slash
  | T_Less
  | T_Greater
  | T_Assign
  | T_Not
  | T_Semicolon
  | T_Comma
  | T_Period
  | T_LSquareBracket
  | T_RSquareBracket
  | T_LParen
  | T_RParen
  | T_LCurlyBracket
  | T_RCurlyBracket
  | T_Unknown
  | T_EOF

open Parsing;;
let yytransl_const = [|
  257 (* T_Define *);
  258 (* T_Declare *);
  259 (* T_Pre *);
  260 (* T_Post *);
  261 (* T_Bool *);
  262 (* T_Void *);
  265 (* T_True *);
  266 (* T_False *);
  267 (* T_Dims *);
  268 (* T_LessEqual *);
  269 (* T_GreaterEqual *);
  270 (* T_Equal *);
  271 (* T_NotEqual *);
  272 (* T_And *);
  273 (* T_Or *);
  274 (* T_While *);
  275 (* T_For *);
  276 (* T_If *);
  277 (* T_Else *);
  278 (* T_Return *);
  279 (* T_Break *);
  280 (* T_Plus *);
  281 (* T_Minus *);
  282 (* T_Star *);
  283 (* T_Slash *);
  284 (* T_Less *);
  285 (* T_Greater *);
  286 (* T_Assign *);
  287 (* T_Not *);
  288 (* T_Semicolon *);
  289 (* T_Comma *);
  290 (* T_Period *);
  291 (* T_LSquareBracket *);
  292 (* T_RSquareBracket *);
  293 (* T_LParen *);
  294 (* T_RParen *);
  295 (* T_LCurlyBracket *);
  296 (* T_RCurlyBracket *);
  297 (* T_Unknown *);
  298 (* T_EOF *);
    0|]

let yytransl_block = [|
  263 (* T_Int *);
  264 (* T_Ident *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\000\000"

let yylen = "\002\000\
\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\001\000\003\000"

let yydgoto = "\002\000\
\004\000\000\000"

let yysindex = "\255\255\
\250\254\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000"

let yytablesize = 1
let yytable = "\001\000\
\003\000"

let yycheck = "\001\000\
\007\001"

let yynames_const = "\
  T_Define\000\
  T_Declare\000\
  T_Pre\000\
  T_Post\000\
  T_Bool\000\
  T_Void\000\
  T_True\000\
  T_False\000\
  T_Dims\000\
  T_LessEqual\000\
  T_GreaterEqual\000\
  T_Equal\000\
  T_NotEqual\000\
  T_And\000\
  T_Or\000\
  T_While\000\
  T_For\000\
  T_If\000\
  T_Else\000\
  T_Return\000\
  T_Break\000\
  T_Plus\000\
  T_Minus\000\
  T_Star\000\
  T_Slash\000\
  T_Less\000\
  T_Greater\000\
  T_Assign\000\
  T_Not\000\
  T_Semicolon\000\
  T_Comma\000\
  T_Period\000\
  T_LSquareBracket\000\
  T_RSquareBracket\000\
  T_LParen\000\
  T_RParen\000\
  T_LCurlyBracket\000\
  T_RCurlyBracket\000\
  T_Unknown\000\
  T_EOF\000\
  "

let yynames_block = "\
  T_Int\000\
  T_Ident\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 30 "parser.mly"
        ( _1 )
# 178 "parser.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 34 "parser.mly"
        ( _1 )
# 185 "parser.ml"
               : 'expr))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : int)
