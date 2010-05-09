module Scanner =
  struct
    type t = { str : string; mutable index : int }
    exception NoMoreTokens
    let spaces = Str.regexp "[ \t]"
    let punct = Str.regexp "[ ().,;=:{}]"
    let create initial_string = { str = initial_string; index = 0 }
    let has_token s = (s.index < (String.length s.str))
    let rest_str s = Str.string_after s.str s.index
    let next_token s =
      
      let is_space s = Str.string_match spaces s 0 in
      let is_punct s = Str.string_match punct s 0 in
      let is_delimeter s = (is_space s) || (is_punct s) in
      let cur () = String.sub s.str s.index 1 in
      let incr () = s.index <- (s.index + 1) in

      while ((has_token s) && (is_space (cur ()))) do incr () done;
      if not (has_token s) then raise NoMoreTokens;
      let rec build_token () =
	if not (has_token s) then "" else
	  begin
	    let cur_str = cur () in
	    if (is_delimeter cur_str) then
	      ""
	    else
	      begin
		incr ();
		cur_str ^ (build_token ())
	      end
	  end
      in
	let cur_str = cur () in
	incr ();
	if (is_punct cur_str) then
	  cur_str
	else
	  cur_str ^ (build_token ())

  end ;;
