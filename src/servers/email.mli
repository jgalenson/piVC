type submission_info = {
  to_addrs : string list;
  comment : string option;
}

type user_info = {
  user_name : string;
  user_email_addr : string;
}

val send_test_email : unit -> unit
val send_error_notification : string -> unit
val send_email : string -> string -> string list -> string list -> string -> string -> unit
val email_heading : string -> string
val email_segment_of_options : Utils.options -> string
val email_segment_of_user : user_info -> string
val email_heading : string -> string
val go_submit : string -> submission_info -> user_info -> Utils.options -> Verify.function_validity_information list option -> Semantic_checking.error list -> string
  
