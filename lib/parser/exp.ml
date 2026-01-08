open Core
open MParser
open Utils
module E = Syntax.Exp
module T = Syntax.Typ
module Label = Syntax.Label

let keywords = [ "fn"; "let"; "rec"; "in"; "Cons"; "Nil"; "type"; "case" ]
let is_keyword str = List.mem keywords ~equal:String.( = ) str

let var_ident s =
  let parser =
    let* str = look_ahead (ident lowercase) in
    if is_keyword str then fail ("Expecting identifyer but found keyword: " ^ str) else ident lowercase
  in
  parser s

let typ_ident s = (ident lowercase) s
let sym_nil s = (symbol "Nil") s

let sym_cons p =
  let* () = symbol "Cons" in
  let* () = symbol "(" in
  let* head = p in
  let* () = symbol "," in
  let* tail = p in
  let* () = symbol ")" in
  return (head, tail)

type ('a, 'b) parse_state = {
  env : (string, E.Var.t, 'a) Base.Map.t;
  consts : (Label.t, T.t Label.Map.t, 'b) Base.Map.t;
  types : (string, T.t, 'a) Base.Map.t;
}

let var s =
  let parse =
    let* str = var_ident in
    let* { env; _ } = get_user_state in
    match Map.find env str with Some var -> return (E.var var) | None -> fail ("Unbound variable: " ^ str)
  in
  parse s

let with_bound_var str p =
  let var = E.Var.new_var str in
  let* state = get_user_state in
  let* () = set_user_state { state with env = Map.set state.env ~key:str ~data:var } in
  let* result = p in
  let* () = set_user_state state in
  return (var, result)

(* currently type variables cannot appear in type declarations *)
let typ_dec s =
  let parser =
    let* () = symbol "type" in
    let* t_name = typ_ident in
    let* () = symbol "=" in
    let* t = Typ.parse in
    let* ({ types; consts; _ } as state) = get_user_state in
    match Map.find types t_name with
    | Some _ -> fail ("Type " ^ t_name ^ " is already defined.")
    | None -> (
        let state = { state with types = Map.set types ~key:t_name ~data:t } in
        match T.out t with
        | T.Tsum lmap -> begin
            try
              let f ~key ~data:_ consts = Map.add_exn consts ~key ~data:lmap in
              set_user_state { state with consts = Map.fold lmap ~init:consts ~f }
            with _ -> fail "Constructor name used in multiple types."
          end
        | _ -> fail ("Type " ^ t_name ^ " is not a sum type."))
  in
  parser s

let rec exp s = app s

and app s =
  let parse =
    let* e = proj in
    let* es = many (attempt proj) in
    let f func arg = E.app ~func ~arg in
    return (List.fold ~init:e ~f es)
  in
  parse s

and proj s =
  let parse =
    let* e = atom in
    let proj = char '.' >> prod_comp >>= fun str -> return (Label.of_string str) in
    let* projs = many (attempt proj) in
    let f arg comp = E.proj ~comp ~arg in
    return (List.fold ~init:e ~f projs)
  in
  parse s

and atom s = (choice [ case; inj; prod; lrec; let'; nil; cons; lam; var; parens exp ]) s

and case s =
  let case =
    let* con = sum_const >>= fun str -> return (Label.of_string str) in
    let* var = var_ident in
    let* () = symbol "->" in
    let* var_exp = with_bound_var var exp in
    return (con, var_exp)
  in
  let parse =
    let* () = symbol "case" in
    let* typ = option (between (symbol "[") (symbol "]") Typ.parse) in
    let* arg = exp in
    let* cases = between (symbol "{") (symbol "}") (sep_by case (symbol "|")) in
    match Label.Map.of_alist cases with
    | `Ok cases -> return (E.case ~typ ~arg ~cases)
    | `Duplicate_key _ -> fail "Duplicated case."
  in
  parse s

and inj s =
  let parse =
    let* con = sum_const >>= fun str -> return (Label.of_string str) in
    let* { consts; _ } = get_user_state in
    match Map.find consts con with
    | None -> fail ("Contructor not defined: " ^ Label.to_string con)
    | Some lmap ->
        let* arg = exp in
        return (E.inj ~con ~typ:lmap ~arg)
  in
  parse s

and prod s =
  let comp s =
    let parse =
      let* name = prod_comp in
      let* _ = symbol "=" in
      let* e = exp in
      return (Label.of_string name, e)
    in
    parse s
  in
  let parse =
    let* () = symbol "<" in
    let* comps = sep_by comp (symbol ",") in
    let* () = symbol ">" in
    match Label.Map.of_alist comps with
    | `Ok lmap -> return (E.prod lmap)
    | `Duplicate_key _ -> fail "Component appears multiple times in product."
  in
  parse s

and lrec s =
  let parse =
    let* () = symbol "rec" in
    let* arg = exp in
    let* () = symbol "{" in
    let* () = sym_nil in
    let* () = symbol "->" in
    let* base = exp in
    let* () = symbol "|" in
    let* headv, recv = sym_cons var_ident in
    let* () = symbol "->" in
    let* headv, (recv, step) = with_bound_var headv (with_bound_var recv exp) in
    let* () = symbol "}" in
    return (E.lrec ~arg ~base ~headv ~recv ~step)
  in
  parse s

and let' s =
  let parse =
    let* () = symbol "let" in
    let* x = var_ident in
    let* () = symbol "=" in
    let* e1 = exp in
    let* () = symbol "in" in
    let* x, e2 = with_bound_var x exp in
    return (E.let' ~e1 ~x ~e2)
  in
  parse s

and nil s =
  let parse =
    let* () = sym_nil in
    let* () = symbol "[" in
    let* t = Typ.parse in
    let* () = symbol "]" in
    return (E.nil t)
  in
  parse s

and cons s =
  let parse =
    let* head, tail = sym_cons exp in
    return (E.cons ~head ~tail)
  in
  parse s

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

let parse s = (spaces >> many typ_dec >> exp) s
