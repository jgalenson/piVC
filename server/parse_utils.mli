open Semantic_checking
open Ast
  
val goParse : Pervasives.in_channel -> (program option * error Queue.t)
