open Core
open MParser

let exit_with msg =
  let () = print_string ("Parse error:\n" ^ msg) in
  exit 1

(* --- Types --- *)
let typ ?(typ_of_str = fun _ -> failwith "No type variable function provided") str =
  (* parse_string takes: parser -> input -> user_state -> result *)
  match parse_string (Typ.parse typ_of_str) str () with
  | Success t -> t
  | Failed (msg, _) -> exit_with msg

let starting_state =
  let module T = Syntax.Typ in
  let consts =
    match T.out T.bool with T.Tsum bconst -> Map.map ~f:(fun _ -> bconst) bconst | _ -> failwith "bool is a sum type"
  in
  { Exp.types = String.Map.singleton "bool" T.bool; env = String.Map.empty; consts }

(* --- Expression --- *)
let exp str = match parse_string Exp.parse str starting_state with Success t -> t | Failed (msg, _) -> exit_with msg

(* --- Traces --- *)

let trace str =
  match parse_string Trace.parse str starting_state with Success t -> t | Failed (msg, _) -> exit_with msg
