
open Printf
open Xml

let default_port = 4000

(*let address = Unix.ADDR_INET((Unix.inet_addr_of_string ip_address), port)*)

let socket_incoming = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0
let socket_outgoing = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0

let buffer = String.create 255

let listen port = print_string "Listening...\n";
        flush Pervasives.stdout;
        Unix.bind socket_incoming (Unix.ADDR_INET(Unix.inet_addr_any, port));
        Unix.listen socket_incoming 1;
        while true do
          let (u, sa) = Unix.accept socket_incoming in
            printf("Processing request\n");
            flush Pervasives.stdout;
            (*let q = Queue.create ()*)
            (*connect socket_outgoing (Unix.ADDR_INET((gethostbyname ip_address*)
            Unix.close u;
        done
(*Based on the public domain example at http://abaababa.ouvaton.org/caml/intercept.ml*)

let xml_test =
    let x = Xml.parse_file "test_input.xml" in
    print_string (Xml.to_string_fmt x);



