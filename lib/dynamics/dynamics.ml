module Trace = Trace
module Value = Value
module Pprint = Pprint

let init = Stat.init
let simulate = Eval.simulate
let eval = Eval.eval
let assess = Eval.assess

type trace = Value.t Trace.t

open Syntax

(* The call rejection_sampling_until f n should return a list l of traces
   such that of the length of l is n and f t for all traces t in l *)
let rejection_sampling_random_runtime : model:Exp.t -> f:(trace -> bool) -> int -> trace list =
 fun ~model:_ -> failwith "ASSIGNMENT 2: IMPLEMENT ME"

(* The call rejection_sampling_until f n should return a list l of traces
   such that of the length of l is n and
   if (t,w) in l then w = 0.0 or w = 1.0 . *)
let rejection_sampling_fixed_runtime : model:Exp.t -> f:(trace -> bool) -> int -> (trace * float) list =
 fun ~model:_ -> failwith "ASSIGNMENT 2: IMPLEMENT ME"
