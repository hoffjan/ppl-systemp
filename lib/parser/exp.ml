open Core
open MParser
open Utils
module E = Syntax.Exp
module T = Syntax.Typ
module Label = Syntax.Label

let var_ident s = (ident lowercase) s
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

let typ s = (get_user_state >>= fun { types; _ } -> Typ.parse (Map.find_exn types)) s

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

let typ_dec s =
  let parser =
    let* () = symbol "type" in
    let* t_name = typ_ident in
    let* () = symbol "=" in
    let* t = typ in
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

let string s =
  between (char '"') (char '"') (many_satisfy (fun c -> Char.(c <> '"')) |>> fun str -> E.const (E.Cstring str)) s

let num s =
  let digits = pipe2 digit (many digit) (fun d ds -> String.of_char_list (d :: ds)) in

  let parse =
    let* ds = digits in
    let* dot = option (char '.') in
    match dot with
    | None -> return (E.const (E.Cint (Int.of_string ds)))
    | Some _ ->
        let* ds' = digits in
        let f = ds ^ "." ^ ds' in
        return (E.const (E.Cfloat (Float.of_string f)))
  in
  parse s

let const s = (lexeme (choice [ string; num ])) s

(* Binary operations *)

let infix sym prim =
  let op e1 e2 = E.primapp ~prim ~args:[ e1; e2 ] in
  Infix (symbol sym >> return op, Assoc_left)

let prefix sym prim =
  let op e = E.primapp ~prim ~args:[ e ] in
  Prefix (symbol sym >> return op)

let operators () =
  let open Syntax.Prim in
  [
    [ prefix "-" Neg ];
    [ infix "*" Times; infix "/" Div; infix "%" Mod ];
    [ infix "+" Plus; infix "-" Minus ];
    [ infix "=" Eq; infix "/=" Neq; infix "<" Lt; infix "<=" Lte; infix ">" Gt; infix ">=" Gte ];
  ]

let rec exp s = (expression (operators ()) app) s

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
    let proj = char '.' >> prod_comp |>> fun str -> Label.of_string str in
    let* projs = many (attempt proj) in
    let f arg comp = E.proj ~comp ~arg in
    return (List.fold ~init:e ~f projs)
  in
  parse s

and atom s = (choice [ const; case; prod; lrec; let'; nil; cons; inj; lam; var; parens exp ]) s

and case s =
  let case =
    let* con = sum_const |>> fun str -> Label.of_string str in
    let* var = var_ident in
    let* () = symbol "->" in
    let* var_exp = with_bound_var var exp in
    return (con, var_exp)
  in
  let parse =
    let* () = symbol "case" in
    let* typ = option (between (symbol "[") (symbol "]") typ) in
    let* arg = exp in
    let* cases = between (symbol "{") (symbol "}") (sep_by case (symbol "|")) in
    match Label.Map.of_alist cases with
    | `Ok cases -> return (E.case ~typ ~arg ~cases)
    | `Duplicate_key _ -> fail "Duplicated case."
  in
  parse s

and inj s =
  let parse =
    let* con = sum_const |>> fun str -> Label.of_string str in
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
    let* () = symbol "{" in
    let* comps = sep_by comp (symbol ",") in
    let* () = symbol "}" in
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
    let* t = typ in
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
    let* argt = typ in
    let* _ = symbol ")" in
    let* argv, body = with_bound_var argv exp in
    return (E.lam ~argv ~argt ~body)
  in
  parse s

let parse s = (spaces >> many typ_dec >> exp) s
