open Core
module Var = Var.Exp_var

type var = FVAR of Var.t | BVAR of int
type const = Cint of int | Cfloat of float | Cstring of string

type t =
  | VAR of var
  | CONST of const
  | LAM of { argv : string; body : t }
  | APP of { func : t; arg : t }
  | PRIMAPP of { prim : Prim.t; args : t list }
  | INJ of { con : Label.t; arg : t }
  | CASE of { arg : t; cases : (string * t) Label.Map.t }
  | PROD of t Label.Map.t
  | PROJ of { comp : Label.t; arg : t }

type view =
  | Evar of Var.t
  | Econst of const
  | Elam of { argv : Var.t; body : t }
  | Eapp of { func : t; arg : t }
  | Eprimapp of { prim : Prim.t; args : t list }
  | Einj of { con : Label.t; arg : t }
  | Ecase of { arg : t; cases : (Var.t * t) Label.Map.t }
  | Eprod of t Label.Map.t
  | Eproj of { comp : Label.t; arg : t }
[@@deriving variants]

let rec abs fx i z =
  let abs1 (v, e) = (v, abs fx (i + 1) z e) in
  function
  | VAR x -> VAR (fx i z x)
  | CONST b -> CONST b
  | LAM { argv; body } -> LAM { argv; body = abs fx (i + 1) z body }
  | APP { func; arg } -> APP { func = abs fx i z func; arg = abs fx i z arg }
  | PRIMAPP { prim; args } -> PRIMAPP { prim; args = List.map args ~f:(abs fx i z) }
  | INJ { con; arg } -> INJ { con; arg = abs fx i z arg }
  | CASE { arg; cases } -> CASE { arg = abs fx i z arg; cases = Map.map cases ~f:abs1 }
  | PROD vs -> PROD (Map.map vs ~f:(abs fx i z))
  | PROJ { comp; arg } -> PROJ { comp; arg = abs fx i z arg }

let bind = abs (fun i z -> function FVAR x -> if Var.(x = z) then BVAR i else FVAR x | y -> y)
let unbind = abs (fun i z -> function BVAR j -> if i = j then FVAR z else BVAR j | y -> y)

let into =
  let into1 (v, e) = (Var.to_user_string v, bind 0 v e) in
  function
  | Evar x -> VAR (FVAR x)
  | Econst b -> CONST b
  | Elam { argv; body } -> LAM { argv = Var.to_user_string argv; body = bind 0 argv body }
  | Eapp { func; arg } -> APP { func; arg }
  | Eprimapp { prim; args } -> PRIMAPP { prim; args }
  | Einj { con; arg } -> INJ { con; arg }
  | Ecase { arg; cases } -> CASE { arg; cases = Map.map cases ~f:into1 }
  | Eprod vs -> PROD vs
  | Eproj { comp; arg } -> PROJ { comp; arg }

let out =
  let out1 (v, e) =
    let v = Var.new_var v in
    (v, unbind 0 v e)
  in
  function
  | VAR (FVAR x) -> Evar x
  | VAR (BVAR _) -> failwith "out_val: ABT invalid variable"
  | CONST b -> Econst b
  | LAM { argv; body } ->
      let argv = Var.new_var argv in
      Elam { argv; body = unbind 0 argv body }
  | APP { func; arg } -> Eapp { func; arg }
  | PRIMAPP { prim; args } -> Eprimapp { prim; args }
  | INJ { con; arg } -> Einj { con; arg }
  | CASE { arg; cases } -> Ecase { arg; cases = Map.map cases ~f:out1 }
  | PROD vs -> Eprod vs
  | PROJ { comp; arg } -> Eproj { comp; arg }

let rec frees =
  let frees1 (_, e) = frees e in
  function
  | VAR (FVAR x) -> Var.Set.singleton x
  | VAR (BVAR _) | CONST _ -> Var.Set.empty
  | LAM { body; _ } -> frees body
  | APP { func; arg } -> Var.Set.union_list [ frees func; frees arg ]
  | PRIMAPP { args; _ } -> Var.Set.union_list (List.map args ~f:frees)
  | INJ { arg; _ } | PROJ { arg; _ } -> frees arg
  | CASE { arg; cases } ->
      let _, cases_frees = List.unzip (Map.to_alist (Map.map cases ~f:frees1)) in
      Var.Set.union_list (frees arg :: cases_frees)
  | PROD vs -> Var.Set.union_list (List.map (Map.to_alist vs) ~f:frees1)

let var x = into (evar x)
let const b = into (econst b)
let lam ~argv ~body = into (elam ~argv ~body)
let app ~func ~arg = into (eapp ~func ~arg)
let primapp ~prim ~args = into (eprimapp ~prim ~args)
let inj ~con ~arg = into (einj ~con ~arg)
let case ~arg ~cases = into (ecase ~arg ~cases)
let prod vs = into (eprod vs)
let proj ~comp ~arg = into (eproj ~comp ~arg)
let to_string _ = ""
