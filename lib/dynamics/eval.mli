open Syntax

module Dynamics_error : sig
  type t

  exception E of t

  val to_string : t -> string
end

type 'a result = 'a Value.result

val eval : Exp.t -> Value.t
val simulate : ?seed:int -> Exp.t -> Value.t option result
val assess : Value.t Trace.t -> Exp.t -> Value.t option result
val generate : ?seed:int -> trace:Value.t Trace.t -> Exp.t -> Value.t option result
