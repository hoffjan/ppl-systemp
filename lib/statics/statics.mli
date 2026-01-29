open Core
open Syntax

module Statics_error : sig
  type t

  include Sexpable.S with type t := t

  exception E of t

  val to_string : t -> string
end

val type_exp : Exp.t -> Typ.t
