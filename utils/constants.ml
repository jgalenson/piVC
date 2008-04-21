let main_server_port = 4242 ;;
let dp_server_port = 4243 ;;
(*let dp_server_address = "128.12.102.200" ;; (* TODO: Yuck. *)*)
(*let dp_server_address = "127.0.1.1" ;; (* TODO: Yuck. *)*)
let dp_server_address = Unix.inet_addr_any ;;
let num_cached_vcs = 5;;
let truncate_output_length = 200;;

(*The following paths are all relative to the bin file that is running*)
let yices_path = "./yices";;
let includes_path = "included.pi";;
