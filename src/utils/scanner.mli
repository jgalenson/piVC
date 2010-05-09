module Scanner :
  sig
    type t = { str : string; mutable index : int }
    exception NoMoreTokens
    val spaces : Str.regexp
    val punct : Str.regexp
    val create : string -> t
    val has_token : t -> bool
    val next_token : t -> string
    val rest_str : t -> string
  end ;;
