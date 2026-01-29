open Core

module type S = sig
  type t

  include Comparable.S with type t := t
  include Sexpable.S with type t := t

  val new_var : string -> t
  val to_string : t -> string
  val to_user_string : t -> string
end

module Var_rep = struct
  type t = { name : string; id : int } [@@deriving sexp]

  let compare v1 v2 = Int.compare v1.id v2.id
  let to_string v = v.name ^ "_" ^ Int.to_string v.id
  let to_user_string v = v.name
end

module Make_var (Var_rep : module type of Var_rep) : S = struct
  include Var_rep
  include Comparable.Make (Var_rep)

  let counter = ref 0

  let new_var s =
    let _ = counter := !counter + 1 in
    { name = s; id = !counter }
end

module Exp_var = Make_var (Var_rep)
module Typ_var = Make_var (Var_rep)
