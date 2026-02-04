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
  let module T = Syntax.Typ in
  let consts =
    match T.out T.bool with T.Tsum bconst -> Map.map ~f:(fun _ -> bconst) bconst | _ -> failwith "bool is a sum type"
  in
  (* parse_string takes: parser -> input -> user_state -> result *)
  match
    parse_string (Exp.parse << eof) str { types = String.Map.singleton "bool" T.bool; env = String.Map.empty; consts }
  with
  | Success t -> t
  | Failed (msg, _) -> exit_with msg
