(* lexer.mli *)

val files : (string*int) list ref ;;
val lang : Lexing.lexbuf -> Parser.token ;;
