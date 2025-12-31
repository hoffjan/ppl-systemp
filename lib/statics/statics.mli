(* open Core *)

(* module StaticsError : sig *)
(*   type t = *)
(*     | TypVar of Var.Typ_var.t *)
(*     | ExpVar of Var.Exp_var.t *)
(*     | Type of string * unit Typ.t *)
(*     | MissingLabel of string * Label.t *)
(*     | Elim of string * Var.Exp_var.t * unit Typ.t *)
(*     | Prim of Prim.t * unit Typ.t *)

(*   include Sexpable.S with type t := t *)

(*   val to_string : t -> string *)
(* end *)

(* exception TypeError of StaticsError.t *)

(* val validateType : unit Typ.t -> unit *)
(* val check : Raml.t -> unit *)
