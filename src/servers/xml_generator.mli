type xml_node_content =
  | Text of string
  | Children of xml_node list
  | Empty

and xml_node = {
  mutable node_name: string;
  mutable node_contents: xml_node_content;
  mutable node_attributes: (string * string) list;
} ;;

val create : string -> xml_node ;;
val add_attribute : string * string -> xml_node -> unit ;;
val set_text : string -> xml_node -> unit ;;
val add_child : xml_node -> xml_node -> unit ;;
val string_of_xml_node : xml_node -> string ;;
