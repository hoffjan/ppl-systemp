open Core
open MParser

let exit_with msg =
  let () = print_string ("Parse error:\n" ^ msg) in
  exit 1

(* --- Types --- *)
let typ ?(typ_of_str = fun _ -> failwith "No type variable function provided") str =
  (* parse_string takes: parser -> input -> user_state -> result *)
  match parse_string (Typ.parse typ_of_str << eof) str () with
  | Success t -> t
  | Failed (msg, _) -> exit_with msg

(* --- Types --- *)
let exp str =
  (* parse_string takes: parser -> input -> user_state -> result *)
  match
    parse_string (Exp.parse << eof) str
      { types = String.Map.singleton "bool" Syntax.Typ.bool; env = String.Map.empty; consts = Syntax.Label.Map.empty }
  with
  | Success t -> t
  | Failed (msg, _) -> exit_with msg
