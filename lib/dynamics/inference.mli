type trace = Value.t Trace.t

val rejection_sampling_random_runtime : model:Syntax.Exp.t -> f:(trace -> bool) -> int -> trace list
val rejection_sampling_fixed_runtime : model:Syntax.Exp.t -> f:(trace -> bool) -> int -> (trace * float) list
val importance_resampling : model:Syntax.Exp.t -> observations:trace -> int -> Value.t option Value.result
