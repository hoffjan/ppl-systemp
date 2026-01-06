open MParser
open Syntax
open Utils

(* --- Types --- *)

let base s =
  let int = symbol "int" >> return Typ.Bint in
  let string = symbol "string" >> return Typ.Bstring in
  let float = symbol "float" >> return Typ.Bfloat in
  (choice [ int; float; string ] >>= fun btype -> return (Typ.base btype)) s

let rec typ s = (choice [ attempt arrow; list; atom ]) s
and atom s = (choice [ base; prod; sum; parens typ ]) s

and decs l_sym r_sym s =
  let dec s =
    let parse =
      let* name = ident in
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
    let lmap = Label.Map.of_alist_exn decs in
    return lmap
  in
  parse s

and prod s = (decs "<" ">" >>= fun lmap -> return (Typ.prod lmap)) s
and sum s = (decs "[" "]" >>= fun lmap -> return (Typ.sum lmap)) s

and arrow s =
  let parse =
    let* argt = atom in
    let* _ = symbol "->" in
    let* rest = typ in
    return (Typ.arr ~argt ~rest)
  in
  parse s

and list s =
  let parse =
    let* _ = symbol "list" in
    let* t_elem = parens typ in
    return (Typ.list t_elem)
  in
  parse s

let parse s = (spaces >> typ) s
