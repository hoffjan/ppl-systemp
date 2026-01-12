open MParser
open Syntax
open Utils

let base s =
  let int = symbol "int" >> return Typ.Bint in
  let string = symbol "string" >> return Typ.Bstring in
  let float = symbol "float" >> return Typ.Bfloat in
  (choice [ int; float; string ] >>= fun btype -> return (Typ.base btype)) s

let rec typ t_of_str s =
  let parse =
    let* argt = list t_of_str in
    let* arrow = option (symbol "->") in
    match arrow with
    | None -> return argt
    | Some () ->
        let* rest = typ t_of_str in
        return (Typ.arr ~argt ~rest)
  in
  parse s

and var t_of_str = typ_ident >>= fun str -> return (t_of_str str)
and atom t_of_str = choice [ attempt (var t_of_str); base; prod t_of_str; sum t_of_str; parens (typ t_of_str) ]

and decs t_of_str l_sym r_sym label s =
  let dec s =
    let parse =
      let* name = label in
      let* _ = symbol ":" in
      let* t = typ t_of_str in
      return (Label.of_string name, t)
    in
    parse s
  in
  let parse =
    let* _ = symbol l_sym in
    let* decs = sep_by dec (symbol ",") in
    let* _ = symbol r_sym in
    match Label.Map.of_alist decs with `Ok lmap -> return lmap | `Duplicate_key _ -> fail "Identical labels in type."
  in
  parse s

and prod t_of_str s = (decs t_of_str "{" "}" prod_comp >>= fun lmap -> return (Typ.prod lmap)) s
and sum t_of_str s = (decs t_of_str "[" "]" sum_const >>= fun lmap -> return (Typ.sum lmap)) s

and list t_of_str =
  let* t = atom t_of_str in
  many_fold_left (fun t () -> Typ.list t) t (symbol "list")

let parse t_of_str s = (spaces >> typ t_of_str) s
