open Syntax

type value =
  | Vconst of Exp.const
  | Vlam of (value -> value)
  | Vinj of Label.t * value
  | Vprod of value Label.Map.t
  | Vlist of value List.t

module Dynamics_error : sig
  type t

  exception E of t

  val to_string : t -> string
end

val eval : Exp.t -> value
