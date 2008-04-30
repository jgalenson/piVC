module String_map = Map.Make(String)

exception Config_Key_Not_Found of string;;
exception Config_Key_Could_Not_Be_Parsed_To_Int of string * string;;

let key_value_map = ref String_map.empty

let get_value key = 
  try
    String_map.find key (!key_value_map)
  with
      Not_found -> raise (Config_Key_Not_Found key)
        
let get_value_int key = 
  let value = get_value key in
  try
    int_of_string (value)
  with
      Failure("int_of_string") -> raise (Config_Key_Could_Not_Be_Parsed_To_Int (key,value))
        

let rec trim str = 
  let start_index = match String.get str 0 with
      ' ' -> 1
    | _ -> 0
  in
  let end_index = match String.get str ((String.length str) - 1) with
      ' ' -> (String.length str) - 1
    | _ -> String.length str
  in
    if start_index = 0 && end_index = String.length str then
      str
    else
      trim (String.sub str start_index (end_index-start_index))
  

(*This function assumes Unix line endings. Because it's also using a lot of other Unix-only
  system calls, I think that the line endings requirement is not a problem.*)
let read_from_file file_path = 
  let file = Unix.openfile file_path [Unix.O_RDONLY] 0o640 in
  let file_size = (Unix.fstat file).Unix.st_size in
  let file_text =
    let file_text_temp = String.create file_size in
    ignore (Unix.read file file_text_temp 0 file_size);
    file_text_temp
  in
  let rec load_key_values str = 
    if String.length str > 0 then
      let index_of_end =
        if String.contains str '\n' then
          String.index str '\n'
        else
          String.length str
      in
      let key_value = String.sub str 0 index_of_end in
      let rest = 
        if index_of_end < (String.length str)-1 then
          String.sub str (index_of_end + 1) ((String.length str)-index_of_end-1)
        else
          ""
      in
        if (String.length key_value > 0) && (String.get key_value 0 != '#') then
          begin
            let index_of_equals = String.index key_value '=' in
            let key = trim (String.sub key_value 0 index_of_equals) in 
            let value = trim (String.sub key_value (index_of_equals + 1) ((String.length key_value)-index_of_equals-1)) in
              String_map.add key value (load_key_values rest)
          end
        else
          load_key_values rest
    else
      String_map.empty
  in
    load_key_values file_text
        
        
let load file_path = 
  key_value_map := read_from_file file_path
