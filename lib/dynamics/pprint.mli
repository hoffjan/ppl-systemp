val print_value : Value.t -> unit
val print_trace : Value.t Trace.t -> unit
val print_result : ?weight:bool -> ?trace:bool -> Value.t Value.result -> unit
