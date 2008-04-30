(*This should be called once, with the absolute path to the config file.*)
val load : string -> unit

(*Takes a config key, returns the corresponding value.
  Raises Config_Key_Not_Found if necessary.
*)
val get_value : string -> string

(*Takes a config key, returns the corresponding value parsed to an integer.
  Raises Config_Key_Not_Found or Config_Key_Could_Not_Be_Parsed_To_Int if necessary.
*)
val get_value_int : string -> int
