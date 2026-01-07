open Core
open MParser
open Utils

(* --- Types --- *)
let typ str =
  (* parse_string takes: parser -> input -> user_state -> result *)
  match parse_string (Typ.parse << eof) str () with
  | Success t -> t
  | Failed (msg, _) ->
      let () = print_string msg in
      raise Syntax_error
