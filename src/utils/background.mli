module Background :
  sig
    type 'a t = 'a Event.channel
    val create : ('a -> 'b) -> 'a -> 'b t
    val get_result : 'a t -> 'a
  end ;;
