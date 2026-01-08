open MParser
open Syntax
open Utils

let base s =
  let int = symbol "int" >> return Typ.Bint in
  let string = symbol "string" >> return Typ.Bstring in
  let float = symbol "float" >> return Typ.Bfloat in
  (choice [ int; float; string ] >>= fun btype -> return (Typ.base btype)) s

let rec typ s =
  let parse =
    let* argt = list in
    let* arrow = option (symbol "->") in
    match arrow with
    | None -> return argt
    | Some () ->
        let* rest = typ in
        return (Typ.arr ~argt ~rest)
  in
  parse s

and atom s = (choice [ base; prod; sum; parens typ ]) s

and decs l_sym r_sym label s =
  let dec s =
    let parse =
      let* name = label in
      let* _ = symbol ":" in
      let* t = typ in
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

and prod s = (decs "<" ">" prod_comp >>= fun lmap -> return (Typ.prod lmap)) s
and sum s = (decs "[" "]" sum_const >>= fun lmap -> return (Typ.sum lmap)) s

and list s =
  let parse =
    let* t = atom in
    many_fold_left (fun t () -> Typ.list t) t (symbol "list")
  in
  parse s

let parse s = (spaces >> typ) s
