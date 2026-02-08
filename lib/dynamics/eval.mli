open Syntax

module Dynamics_error : sig
  type t

  exception E of t

  val to_string : t -> string
end

val eval : Exp.t -> Value.t
