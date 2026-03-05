exception Parse_error of string

val typ : ?typ_of_str:(string -> Syntax.Typ.t option) -> string -> Syntax.Typ.t
val exp : string -> Syntax.Exp.t
val trace : string -> Syntax.Exp.t Dynamics.Trace.t
