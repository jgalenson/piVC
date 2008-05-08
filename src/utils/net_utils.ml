(* Gets one string of input from the stream.
   We first get an int (the number of chars)
   and then a string of that many chars. *)
let get_input ic = 
  let in_len = input_binary_int ic in
  (*print_endline ("Got input of length " ^ (string_of_int in_len));*)
  let in_buf = Buffer.create in_len in
  Buffer.add_channel in_buf ic in_len;
(*  Utils.convert_line_endings (Buffer.contents in_buf) ;;*)
  Buffer.contents in_buf

(* Sends one string to the stream. *)
let send_output oc str =
  let out_len = String.length str in
  output_binary_int oc out_len;
  output_string oc str
