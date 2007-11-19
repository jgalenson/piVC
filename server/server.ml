open Unix
open Printf

let port = 4000

(*let address = Unix.ADDR_INET((Unix.inet_addr_of_string ip_address), port)*)

let socket_incoming = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0
let socket_outgoing = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0

let buffer = String.create 255

let _ = bind socket_incoming (ADDR_INET(inet_addr_any, port));
        listen socket_incoming 1;
        while true do
          let (u, sa) = accept socket_incoming in
            printf("Processing request\n");
            flush Pervasives.stdout;
            (*let q = Queue.create ()*)
            (*connect socket_outgoing (ADDR_INET((gethostbyname ip_address*)
            close u;
        done


(* Thanks go:
http://abaababa.ouvaton.org/caml/intercept.ml*)