open Syntax

exception Dist_arg_mismatch of Value.t

val init : int -> unit
val weigh : dist:Prim.Dist.t -> arg:Value.t -> Value.t -> float
val sample : dist:Prim.Dist.t -> arg:Value.t -> Value.t
