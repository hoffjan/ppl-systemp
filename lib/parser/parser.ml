open Core
open MParser

let exit_with msg =
  let () = print_string ("Parse error:\n" ^ msg) in
  exit 1

(* --- Types --- *)
let typ str =
  (* parse_string takes: parser -> input -> user_state -> result *)
  match parse_string (Typ.parse << eof) str () with
  | Success t -> t
  | Failed (msg, _) -> exit_with msg

(* --- Types --- *)
let exp str =
  (* parse_string takes: parser -> input -> user_state -> result *)
  match parse_string (Exp.parse << eof) str { env = String.Map.empty; consts = Syntax.Label.Map.empty } with
  | Success t -> t
  | Failed (msg, _) -> exit_with msg
