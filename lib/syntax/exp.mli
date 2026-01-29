module Var = Var.Exp_var

type t
type const = Cint of int | Cfloat of float | Cstring of string

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

(* basics *)
val into : view -> t
val out : t -> view
val frees : t -> Var.Set.t

(* wrappers for (into (Vvar v)), etc *)
val var : Var.t -> t
val const : const -> t
val lam : argv:Var.t -> argt:Typ.t -> body:t -> t
val app : func:t -> arg:t -> t
val primapp : prim:Prim.t -> args:t list -> t
val inj : con:Label.t -> typ:Typ.t Label.Map.t -> arg:t -> t
val case : arg:t -> cases:(Var.t * t) Label.Map.t -> typ:Typ.t option -> t
val prod : t Label.Map.t -> t
val proj : comp:Label.t -> arg:t -> t
val let' : e1:t -> x:Var.t -> e2:t -> t
val nil : Typ.t -> t
val cons : head:t -> tail:t -> t
val lrec : arg:t -> base:t -> headv:Var.t -> recv:Var.t -> step:t -> t

(* extras *)
val to_string : t -> string
