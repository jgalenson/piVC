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

let linenum = ref 1

let lastt = ref "INIT"

let s = ref []

let finished_parsing = ref false

let last str = lastt := str

let get_last () = !lastt


let reset_linenum () = linenum := 1

let incr_linenum () = incr linenum

let get_linenum () = !linenum


let save () = s := (!linenum) :: !s

let saved () =
  match !s with
  | [] -> assert false
  | h :: t -> s := t; h


let pln () = print_endline (string_of_int !linenum)


let updateLocation (lexbuf) = 
   print_string("")
(*   print_int(lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum);
   print_string(Lexing.lexeme lexbuf);
   print_string("\n");
*)