module Statics = Statics
module Parser = Parser
module Syntax = Syntax
module Dynamics = Dynamics

type trace = Dynamics.Inference.trace

val exp_of_str : string -> Syntax.Exp.t
val model_of_file : string -> Syntax.Exp.t
val trace_of_string : string -> Dynamics.Value.t Dynamics.Trace.t
val simulate : ?seed:int -> ?arg:Syntax.Exp.t -> Syntax.Exp.t -> Dynamics.Value.t option Dynamics.Value.result

val assess :
  ?arg:Syntax.Exp.t ->
  Dynamics.Value.t Dynamics.Trace.t ->
  Syntax.Exp.t ->
  Dynamics.Value.t option Dynamics.Value.result

val eval : ?arg:Syntax.Exp.t -> Syntax.Exp.t -> Dynamics.Value.t
val rejection_sampling_random_runtime : model:Syntax.Exp.t -> f:(trace -> bool) -> int -> trace list
val rejection_sampling_fixed_runtime : model:Syntax.Exp.t -> f:(trace -> bool) -> int -> (trace * float) list

val importance_resampling :
  model:Syntax.Exp.t -> observations:trace -> int -> Dynamics.Value.t option Dynamics.Value.result

val print_value : Dynamics.Value.t -> unit
val print_trace : trace -> unit
val print_result : ?weight:bool -> ?trace:bool -> Dynamics.Value.t option Dynamics.Value.result -> unit
