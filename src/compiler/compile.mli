open Semantic_checking ;;
open Ast ;;

(*Takes in a list of (file_name, file_contents).
  It appends all the files together, prepends the includes file, and feeds
  the entire thing to the parser.
  The file names are used so that errors can be mapped back to files.
  (Even though the files are all appended together, the parser is smart
  enough to always know what file it's currently reading.
*)
val parse_strings : (string * string) list -> (program option * error list) ;;
