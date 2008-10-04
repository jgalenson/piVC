(*
 * Text-based interface with yices.
 *)

(* ID of manager *)
type id

(* Call once before anything else. *)
val init : unit -> unit

(* Create a new logical context. *)
val new_context : unit -> id
(* Remove all assertions. *)
val reset_context : id -> unit
(* Delete context; ID is not longer valid. *)
val delete_context : id -> unit

(*
 * Typical usage:
 *   send id "...";
 *   wait id;  (* wait for yices to reply *)
 *   let s = recv id in  (* get answer *)
 *     ...
 *)
val send : id -> string -> unit
val wait : id -> unit
val recv : id -> string

val kill : id -> unit

exception NonLinearProblem
