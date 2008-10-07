exception InvalidNodeType

type xml_node_content =
  | Text of string
  | Children of xml_node list
  | Empty

and xml_node = {
  mutable node_name: string;
  mutable node_contents: xml_node_content;
  mutable node_attributes: (string * string) list;
}

let create node_name = {node_name = node_name; node_contents = Empty; node_attributes = []}

let add_attribute attribute (node:xml_node) = 
  node.node_attributes <- List.append node.node_attributes [attribute]

let set_text text node = match node.node_contents with
    Text(str) -> node.node_contents <- Text(text)
  | Children(children) -> raise InvalidNodeType
  | Empty -> node.node_contents <- Text(text)

let add_child child node = match node.node_contents with
    Text(str) -> raise InvalidNodeType
  | Children(children) -> node.node_contents <- Children(List.append children [child])
  | Empty -> node.node_contents <- Children([child])


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

let rec string_of_attribute_list attribute_list =
  let fold_fn prev_str cur_attr =
    prev_str ^ " "  ^ replace_bad_chars (fst cur_attr) ^ "=\"" ^ replace_bad_chars (snd cur_attr) ^ "\""
  in
  List.fold_left fold_fn "" attribute_list

and string_of_xml_node node = 
  string_of_xml_node_with_spacing node 0

and string_of_xml_node_with_spacing node num_spaces = 
  (gen_num_spaces num_spaces) ^ "<" ^ replace_bad_chars node.node_name ^ (string_of_attribute_list node.node_attributes) ^ ">" ^
    begin
      match node.node_contents with
          Text(str) -> replace_bad_chars str
        | Empty -> ""
        | Children(nodes) -> "\n" ^ string_of_xml_nodes_with_spacing nodes (num_spaces + 2) ^ (gen_num_spaces num_spaces)
    end
    ^ "</" ^ node.node_name ^ ">" ^ "\n"

and string_of_xml_nodes_with_spacing nodes num_spaces =
  let fold_fn prev_str cur_node =
    prev_str ^ string_of_xml_node_with_spacing cur_node num_spaces
  in
  List.fold_left fold_fn "" nodes
