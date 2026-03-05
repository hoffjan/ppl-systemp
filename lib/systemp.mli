module Statics = Statics
module Parser = Parser
module Syntax = Syntax
module Dynamics = Dynamics

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
val print_value : Dynamics.Value.t -> unit
val print_trace : Dynamics.Value.t Dynamics.Trace.t -> unit
val print_result : ?weight:bool -> ?trace:bool -> Dynamics.Value.t option Dynamics.Value.result -> unit
