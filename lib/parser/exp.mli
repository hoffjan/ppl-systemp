open Syntax

type ('a, 'b) parse_state = {
  env : (string, Exp.Var.t, 'a) Base.Map.t;
  consts : (Label.t, Typ.t Label.Map.t, 'b) Base.Map.t;
  types : (string, Typ.t, 'a) Base.Map.t;
}

val parse : ('a, 'b) parse_state MParser.state -> (Exp.t, ('a, 'b) parse_state) MParser.reply
