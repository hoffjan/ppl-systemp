(** Variables with unique ids for use in ABTs *)

open Core

module type S = sig
  type t

  include Comparable.S with type t := t
  include Sexpable.S with type t := t

  val new_var : string -> t
  val to_string : t -> string
  val to_user_string : t -> string
end

module Exp_var : S
module Typ_var : S
