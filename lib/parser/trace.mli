val parse :
  ('a, 'b) Exp.parse_state MParser.state -> (Syntax.Exp.t Dynamics.Trace.t, ('a, 'b) Exp.parse_state) MParser.reply
