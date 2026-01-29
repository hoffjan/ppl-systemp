open Core
module Var = Var.Exp_var

type var = FVAR of Var.t | BVAR of int [@@deriving sexp]
type const = Cint of int | Cfloat of float | Cstring of string [@@deriving sexp]

type t =
  | VAR of var
  | CONST of const
  | LAM of { argv : string; argt : Typ.t; body : t }
  | APP of { func : t; arg : t }
  | PRIMAPP of { prim : Prim.t; args : t list }
  | INJ of { con : Label.t; typ : Typ.t Label.Map.t; arg : t }
  | CASE of { arg : t; cases : (string * t) Label.Map.t; typ : Typ.t option }
  | PROD of t Label.Map.t
  | PROJ of { comp : Label.t; arg : t }
  | LET of { e1 : t; x : string; e2 : t }
  | NIL of Typ.t
  | CONS of { head : t; tail : t }
  | LREC of { arg : t; base : t; headv : string; recv : string; step : t }
[@@deriving sexp]

type view =
  | Evar of Var.t
  | Econst of const
  | Elam of { argv : Var.t; argt : Typ.t; body : t }
  | Eapp of { func : t; arg : t }
  | Eprimapp of { prim : Prim.t; args : t list }
  | Einj of { con : Label.t; typ : Typ.t Label.Map.t; arg : t }
  | Ecase of { arg : t; cases : (Var.t * t) Label.Map.t; typ : Typ.t option }
  | Eprod of t Label.Map.t
  | Eproj of { comp : Label.t; arg : t }
  | Elet of { e1 : t; x : Var.t; e2 : t }
  | Enil of Typ.t
  | Econs of { head : t; tail : t }
  | Elrec of { arg : t; base : t; headv : Var.t; recv : Var.t; step : t }
[@@deriving variants]

let rec abs fx i z =
  let abs1 (v, e) = (v, abs fx (i + 1) z e) in
  function
  | VAR x -> VAR (fx i z x)
  | CONST b -> CONST b
  | LAM { argv; argt; body } -> LAM { argv; argt; body = abs fx (i + 1) z body }
  | APP { func; arg } -> APP { func = abs fx i z func; arg = abs fx i z arg }
  | PRIMAPP { prim; args } -> PRIMAPP { prim; args = List.map args ~f:(abs fx i z) }
  | INJ { con; typ; arg } -> INJ { con; typ; arg = abs fx i z arg }
  | CASE { arg; cases; typ } -> CASE { arg = abs fx i z arg; cases = Map.map cases ~f:abs1; typ }
  | PROD vs -> PROD (Map.map vs ~f:(abs fx i z))
  | PROJ { comp; arg } -> PROJ { comp; arg = abs fx i z arg }
  | LET { e1; x; e2 } -> LET { e1 = abs fx i z e1; x; e2 = abs fx (i + 1) z e2 }
  | NIL t -> NIL t
  | CONS { head; tail } -> CONS { head = abs fx i z head; tail = abs fx i z tail }
  | LREC { arg; base; headv; recv; step } ->
      LREC { arg = abs fx i z arg; base = abs fx i z base; headv; recv; step = abs fx (i + 2) z step }

let bind = abs (fun i z -> function FVAR x -> if Var.(x = z) then BVAR i else FVAR x | y -> y)
let unbind = abs (fun i z -> function BVAR j -> if i = j then FVAR z else BVAR j | y -> y)

let into =
  let into1 (v, e) = (Var.to_user_string v, bind 0 v e) in
  function
  | Evar x -> VAR (FVAR x)
  | Econst b -> CONST b
  | Elam { argv; argt; body } -> LAM { argv = Var.to_user_string argv; argt; body = bind 0 argv body }
  | Eapp { func; arg } -> APP { func; arg }
  | Eprimapp { prim; args } -> PRIMAPP { prim; args }
  | Einj { con; typ; arg } -> INJ { con; typ; arg }
  | Ecase { arg; cases; typ } -> CASE { arg; cases = Map.map cases ~f:into1; typ }
  | Eprod vs -> PROD vs
  | Eproj { comp; arg } -> PROJ { comp; arg }
  | Elet { e1; x; e2 } -> LET { e1; x = Var.to_user_string x; e2 = bind 0 x e2 }
  | Enil t -> NIL t
  | Econs { head; tail } -> CONS { head; tail }
  | Elrec { arg; base; headv; recv; step } ->
      let step = bind 1 headv (bind 0 recv step) in
      let headv = Var.to_user_string headv in
      let recv = Var.to_user_string recv in
      LREC { arg; base; headv; recv; step }

let out =
  let out1 (v, e) =
    let v = Var.new_var v in
    (v, unbind 0 v e)
  in
  function
  | VAR (FVAR x) -> Evar x
  | VAR (BVAR _) -> failwith "out_val: ABT invalid variable"
  | CONST b -> Econst b
  | LAM { argv; argt; body } ->
      let argv = Var.new_var argv in
      Elam { argv; argt; body = unbind 0 argv body }
  | APP { func; arg } -> Eapp { func; arg }
  | PRIMAPP { prim; args } -> Eprimapp { prim; args }
  | INJ { con; typ; arg } -> Einj { con; typ; arg }
  | CASE { arg; cases; typ } -> Ecase { arg; cases = Map.map cases ~f:out1; typ }
  | PROD vs -> Eprod vs
  | PROJ { comp; arg } -> Eproj { comp; arg }
  | LET { e1; x; e2 } ->
      let x = Var.new_var x in
      Elet { e1; x; e2 = unbind 0 x e2 }
  | NIL t -> Enil t
  | CONS { head; tail } -> Econs { head; tail }
  | LREC { arg; base; headv; recv; step } ->
      let headv = Var.new_var headv in
      let recv = Var.new_var recv in
      let step = unbind 1 headv @@ unbind 0 recv step in
      Elrec { arg; base; headv; recv; step }

let rec frees =
  let frees1 (_, e) = frees e in
  function
  | VAR (FVAR x) -> Var.Set.singleton x
  | VAR (BVAR _) | CONST _ -> Var.Set.empty
  | LAM { body; _ } -> frees body
  | APP { func; arg } -> Var.Set.union_list [ frees func; frees arg ]
  | PRIMAPP { args; _ } -> Var.Set.union_list (List.map args ~f:frees)
  | INJ { arg; _ } | PROJ { arg; _ } -> frees arg
  | CASE { arg; cases; _ } ->
      let _, cases_frees = List.unzip (Map.to_alist (Map.map cases ~f:frees1)) in
      Var.Set.union_list (frees arg :: cases_frees)
  | PROD vs -> Var.Set.union_list (List.map (Map.to_alist vs) ~f:frees1)
  | LET { e1; e2; _ } -> Var.Set.union_list [ frees e1; frees e2 ]
  | NIL _ -> Var.Set.empty
  | CONS { head; tail } -> Var.Set.union_list [ frees head; frees tail ]
  | LREC { arg; base; step; _ } -> Var.Set.union_list (List.map ~f:frees [ arg; base; step ])

let var x = into (evar x)
let const b = into (econst b)
let lam ~argv ~argt ~body = into (elam ~argv ~argt ~body)
let app ~func ~arg = into (eapp ~func ~arg)
let primapp ~prim ~args = into (eprimapp ~prim ~args)
let inj ~con ~typ ~arg = into (einj ~con ~typ ~arg)
let case ~arg ~cases ~typ = into (ecase ~arg ~cases ~typ)
let prod vs = into (eprod vs)
let proj ~comp ~arg = into (eproj ~comp ~arg)
let let' ~e1 ~x ~e2 = into (elet ~e1 ~x ~e2)
let nil t = into (enil t)
let cons ~head ~tail = into (econs ~head ~tail)
let lrec ~arg ~base ~headv ~recv ~step = into (elrec ~arg ~base ~headv ~recv ~step)
let to_string e = Sexp.to_string_hum (sexp_of_t e)
