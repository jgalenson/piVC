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

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> int
