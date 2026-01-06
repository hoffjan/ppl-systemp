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

(* (\* We need 'rec' because 'term' and 'atom' reference each other *\) *)
(* let rec exp s =  *)
(*   (choice [lam; app]) s *)

(* and atom s = *)
(*   (choice [ *)
(*     (ident |>> fun name -> Var name); (\* Variable *\) *)
(*     (parens term)                     (\* Parenthesized term *\) *)
(*   ]) s *)

(* and app s =  *)
(*   (\* chainl1 parses "atom atom..." and folds them with the operator *\) *)
(*   (\* The 'operator' is implicit application *\) *)
(*   let app_op = return (fun t1 t2 -> App (t1, t2)) in *)
(*   (chain_left1 atom app_op) s *)

(* and abs s =  *)
(*   ( *)
(*     symbol "\\" >>         (\* Match lambda slash *\) *)
(*     ident >>= fun v ->     (\* Match variable argument *\) *)
(*     symbol "." >>          (\* Match dot *\) *)
(*     term |>> fun t ->      (\* Match body *\) *)
(*     Abs (v, t) *)
(*   ) s *)

(* (\* --- 4. Entry Point --- *\) *)
(* let parse_lc str =  *)
(*   (\* parse_string takes: parser -> input -> user_state -> result *\) *)
(*   match parse_string (term << eof) str () with *)
(*   | Success t -> Result.Ok t *)
(*   | Failed (msg, _) -> Result.Error msg *)

(* (\* --- 5. Helper: Pretty Printer --- *\) *)
(* let rec string_of_term = function *)
(*   | Var s -> s *)
(*   | Abs (v, t) -> Printf.sprintf "(\\%s. %s)" v (string_of_term t) *)
(*   | App (t1, t2) -> Printf.sprintf "(%s %s)" (string_of_term t1) (string_of_term t2) *)

(* (\* --- 6. Demo --- *\) *)
(* let () = *)
(*   let tests = [ *)
(*     "x"; *)
(*     "\\x. x"; *)
(*     "x y z";           *)
(*     "\\x. x y";        *)
(*     "(\\x. x) y" *)
(*   ] in *)

(*   Printf.printf "Testing Lambda Parser:\n"; *)
(*   List.iter (fun str -> *)
(*     Printf.printf "Input: \"%s\"\n" str; *)
(*     match parse_lc str with *)
(*     | Ok t -> Printf.printf "Parsed: %s\n\n" (string_of_term t) *)
(*     | Error e -> Printf.printf "Error: %s\n\n" e *)
(*   ) tests *)
