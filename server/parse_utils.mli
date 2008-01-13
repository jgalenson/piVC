open Semantic_checking
open Ast
  
val goParse : Pervasives.in_channel -> (program option * error Queue.t)
val parse_string : string -> (program option * error Queue.t)
