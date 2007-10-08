(* The piVC lexer *)



{
  open Printf
}

let digit = ['0'-'9']

rule lang = parse
  | digit+ as num
    {
      printf "Found num: %s\n" num;
      1
    }
  | _
    {
      (* no-op *)
      2
    }
  | eof {0}

{

(* This stuff is here for testing purposes. We will remove it later.*)

let rec iterate buffer =
  match lang buffer with
    0 -> 0
  | j -> iterate buffer;;

let _ = iterate(Lexing.from_channel stdin)

}