type submission_info = {
  to_addrs : string list;
  comment : string option;
}

type user_info = {
  user_name : string;
  user_email_addr : string;
}


type report_type = Bug_report | Feedback

type report_info = {
  report_type : report_type;
  report_comment : string option;
}


val send_test_email : unit -> unit
val send_error_notification : string -> unit
val send_email : string -> string -> string list -> string list -> string -> string -> unit
val email_heading : string -> string
val email_segment_of_options : Utils.options -> string
val email_segment_of_user : user_info -> string
val email_heading : string -> string
val go_submit : string -> submission_info -> user_info -> Utils.options -> Verify.function_validity_information list option -> Semantic_checking.error list -> string
val go_report : report_info -> string option -> user_info option -> Utils.options option -> string
  
