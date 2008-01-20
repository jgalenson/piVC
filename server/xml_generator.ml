exception InvalidNodeType

type xml_node_content =
  | Text of string
  | Children of xml_node list
  | Empty

and xml_node = {
  name: string;
  content: xml_node_content;
  mutable attributes: (string * string) list;
}

let create node_name = {name = node_name; content = Empty; attributes = []}

let add_attribute attribute (node:xml_node) = 
  node.attributes <- List.append node.attributes [attribute]

let set_text text node = match !node.content with
    Text(str) -> !node.content = Text(text)
  | Children(children) -> raise InvalidNodeType
  | Empty -> !node.content = Text(text)

let add_child child node = match !node.content with
    Text(str) -> raise InvalidNodeType
  | Children(children) -> !node.content = Children(List.append children [child])
  | Empty -> !node.content = Children([child])


(* PRINTING FUNCTIONS *)
let rec string_of_attribute_list attribute_list = match attribute_list with
    [] -> ""
  | e :: l -> " " ^ fst e ^ "=\"" ^ snd e ^ "\""

and string_of_xml_node node = 
  "<" ^ node.name ^ (string_of_attribute_list node.attributes) ^ ">\n" ^
    string_of_xml_node_content node.content ^ "\n" ^
    "</" ^ node.name ^ ">\n"

and string_of_xml_nodes nodes = match nodes with
    [] -> ""
  | e :: l -> (string_of_xml_node e) ^ (string_of_xml_nodes l)

and string_of_xml_node_content content = match content with
    Text(str) -> str
  | Children(nodes) -> string_of_xml_nodes nodes
  | Empty -> ""
