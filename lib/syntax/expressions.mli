module Var = Var.Exp_var

type t
type const = Cint of int | Cfloat of float | Cstring of string

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

(* basics *)
val into : view -> t
val out : t -> view
val frees : t -> Var.Set.t

(* wrappers for (into (Vvar v)), etc *)
val var : Var.t -> t
val const : const -> t
val lam : argv:Var.t -> body:t -> t
val app : func:t -> arg:t -> t
val primapp : prim:Prim.t -> args:t list -> t
val inj : con:Label.t -> arg:t -> t
val case : arg:t -> cases:(Var.t * t) Label.Map.t -> t
val prod : t Label.Map.t -> t
val proj : comp:Label.t -> arg:t -> t

(* extras *)
val to_string : t -> string
