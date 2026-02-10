open Syntax

val init : int -> unit
val weigh : dist:Prim.Dist.t -> arg:Value.t -> Value.t -> float
val sample : dist:Prim.Dist.t -> arg:Value.t -> Value.t
