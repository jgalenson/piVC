(*
 * Copyright 2005, 2006, 2007 Aaron R. Bradley and Zohar Manna
 *
 * This file is part of PiVC.
 * 
 * PiVC is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 3 of the License, or (at your
 * option) any later version.
 * 
 * PiVC is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * for more details.

 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

(*
open Ast

let lineNum = ref 1
let colNum = ref 1
let byteNum = ref 0
let lastToken = ref "INIT"

let currLocation = ref {lineStart = 1; lineEnd = 1; colStart = 0; colEnd = 0; byteStart = 0; byteEnd = 0}

let getCurrLocation = 
  !currLocation

*)
(*
let s = ref []

let finished_parsing = ref false

let last str = last_token := str

let get_last () = !last_token


let reset_linenum () = line_num := 1

let incr_linenum () = incr line_num 

let get_linenum () = !line_num


let save () = s := (!line_num) :: !s

let saved () =
  match !s with
  | [] -> assert false
  | h :: t -> s := t; h


let pln () = print_endline (string_of_int !line_num)


*)
(*
let countNewLines str  = 
  let numNewLines = ref 0 in
  let countFunction c = match c with
      '\n'  -> incr numNewLines
    | _     -> print_string("")
  in String.iter countFunction str; !numNewLines

let updateLocation (lexbuf) = 
  let tokenStr = Lexing.lexeme lexbuf in
  let tokenLength = String.length tokenStr in
  let numNewLines = countNewLines tokenStr in
  let colEnd = match numNewLines with
      0 -> !currLocation.colEnd + tokenLength
     |_ -> tokenLength - (String.rindex tokenStr '\n')
  in
  let newLocation = ref
     {byteStart = !currLocation.byteEnd + 1;
      byteEnd = !currLocation.byteEnd + tokenLength;
      lineStart = !currLocation.lineEnd;
      lineEnd = !currLocation.lineEnd + numNewLines;
      colStart = !currLocation.colEnd;
      colEnd = colEnd;
     } in
  print_string("hiya");
  ignore (currLocation = newLocation) (*ignore fact that assignment does not return unit*)
*)
(*   print_int(lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum);
   print_string(Lexing.lexeme lexbuf);
   print_string("\n");
*)
