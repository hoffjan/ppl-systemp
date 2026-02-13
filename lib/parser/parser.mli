val typ : ?typ_of_str:(string -> Syntax.Typ.t) -> string -> Syntax.Typ.t
val exp : string -> Syntax.Exp.t
val trace : string -> Syntax.Exp.t Dynamics.Trace.t
