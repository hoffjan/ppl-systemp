open Core
open MParser
open Utils
module E = Syntax.Exp

let var_ident s = (ident lowercase) s

let var s =
  let parse =
    let* str = var_ident in
    let* env = get_user_state in
    match Map.find env str with Some var -> return (E.var var) | None -> fail ("Unbound variable: " ^ str)
  in
  parse s

let with_bound_var str p =
  let var = E.Var.new_var str in
  let* env = get_user_state in
  let* () = set_user_state (Map.set env ~key:str ~data:var) in
  let* result = p in
  let* () = set_user_state env in
  return (var, result)

let rec exp s = app s

and app s =
  let parse =
    let* es = many1 atom in
    let e, es = match es with [] -> failwith "impossible" | e :: es -> (e, es) in
    let f func arg = E.app ~func ~arg in
    return (List.fold ~init:e ~f es)
  in
  parse s

and atom s = (choice [ lam; var; parens exp ]) s

and lam s =
  (*    fn (x : int) x *)
  let parse =
    let* _ = symbol "fn" in
    let* _ = symbol "(" in
    let* argv = var_ident in
    let* _ = symbol ":" in
    let* argt = Typ.parse in
    let* _ = symbol ")" in
    let* argv, body = with_bound_var argv exp in
    return (E.lam ~argv ~argt ~body)
  in
  parse s

let parse s = (spaces >> exp) s
