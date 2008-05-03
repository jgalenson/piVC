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
  "<" ^ replace_bad_chars node.name ^ (string_of_attribute_list node.attributes) ^ ">\n" ^
    string_of_xml_node_content node.contents ^
    "</" ^ node.name ^ ">\n"

and string_of_xml_nodes nodes = match nodes with
    [] -> ""
  | e :: l -> (string_of_xml_node e) ^ (string_of_xml_nodes l)

and string_of_xml_node_content content = match content with
    Text(str) -> replace_bad_chars str ^ "\n"
  | Children(nodes) -> string_of_xml_nodes nodes
  | Empty -> ""
