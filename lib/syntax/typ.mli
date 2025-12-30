open Core

type t_base = Bint | Bfloat | Bstring

val compare_t_base : t_base -> t_base -> int
val t_base_of_sexp : Sexp.t -> t_base
val sexp_of_t_base : t_base -> Sexp.t

module Var = Var.Typ_var

type t

include Sexpable.S with type t :=  t

type view =
  | Tbase of t_base
  | Tarr of t * t
  | Tsum of t Label.Map.t
  | Tprod of t Label.Map.t

val into : view -> t
val out : t -> view

val to_string :t -> string
(* I case we want to add inductive types at some point *)
val subst : t -> Var.t -> t -> t
val frees : t -> Var.Set.t

(* wrappers for into (Tvar v), etc. *)
val base : t_base -> t
val arr : t -> t -> t
val sum : t Label.Map.t -> t
val prod : t Label.Map.t -> t
val bool : t
val unit : t

include Comparable.S with type t := t
