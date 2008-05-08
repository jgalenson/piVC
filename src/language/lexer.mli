(* lexer.mli *)

val files : (string*int) list ref ;;
val actual_cnum : int ref ;;
val lang : Lexing.lexbuf -> Parser.token ;;
