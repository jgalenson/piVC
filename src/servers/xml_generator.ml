exception InvalidNodeType

type xml_node_content =
  | Text of string
  | Children of xml_node list
  | Empty

and xml_node = {
  mutable name: string;
  mutable contents: xml_node_content;
  mutable attributes: (string * string) list;
}

let create node_name = {name = node_name; contents = Empty; attributes = []}

let add_attribute attribute (node:xml_node) = 
  node.attributes <- List.append node.attributes [attribute]

let set_text text node = match node.contents with
    Text(str) -> node.contents <- Text(text)
  | Children(children) -> raise InvalidNodeType
  | Empty -> node.contents <- Text(text)

let add_child child node = match node.contents with
    Text(str) -> raise InvalidNodeType
  | Children(children) -> node.contents <- Children(List.append children [child])
  | Empty -> node.contents <- Children([child])


(* PRINTING FUNCTIONS *)

let rec gen_num_spaces num_spaces = 
  if num_spaces <= 0 then
    ""
  else
    " " ^ gen_num_spaces (num_spaces-1)

let replace_bad_chars str = 
  let str1 = Str.global_replace (Str.regexp "&") "&amp;" str in
  let str2 = Str.global_replace (Str.regexp "<") "&lt;" str1 in
  let str3 = Str.global_replace (Str.regexp ">") "&gt;" str2 in
  let str4 = Str.global_replace (Str.regexp "'") "&apos;" str3 in
  let str5 = Str.global_replace (Str.regexp "\"") "&quot;" str4 in
    str5

let rec string_of_attribute_list attribute_list = match attribute_list with
    [] -> ""
  | e :: l -> " " ^ replace_bad_chars (fst e) ^ "=\"" ^ replace_bad_chars (snd e) ^ "\"" ^ string_of_attribute_list l

and string_of_xml_node node = 
  string_of_xml_node_with_spacing node 0

and string_of_xml_node_with_spacing node num_spaces = 
  (gen_num_spaces num_spaces) ^ "<" ^ replace_bad_chars node.name ^ (string_of_attribute_list node.attributes) ^ ">" ^
    begin
      match node.contents with
          Text(str) -> replace_bad_chars str
        | Empty -> ""
        | Children(nodes) -> "\n" ^ string_of_xml_nodes_with_spacing nodes (num_spaces + 2) ^ (gen_num_spaces num_spaces)
    end
    ^ "</" ^ node.name ^ ">" ^ "\n"

and string_of_xml_nodes_with_spacing nodes num_spaces = match nodes with
    [] -> ""
  | e :: l -> (string_of_xml_node_with_spacing e num_spaces) ^ (string_of_xml_nodes_with_spacing l num_spaces)
(*
and string_of_xml_node_content_with_spacing content num_spaces = match content with
    Text(str) -> replace_bad_chars str
  | Children(nodes) -> string_of_xml_nodes_with_spacing nodes num_spaces
  | Empty -> ""
*)
