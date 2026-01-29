val ( let* ) : ('a, 'b) MParser.t -> ('a -> ('c, 'b) MParser.t) -> ('c, 'b) MParser.t
val lexeme : ('a, 'b) MParser.t -> ('a, 'b) MParser.t
val symbol : string -> (unit, 'a) MParser.t
val ident : (char, 'a) MParser.t -> (string, 'a) MParser.t
val parens : ('a, 'b) MParser.t -> ('a, 'b) MParser.t
val prod_comp : 'a MParser.state -> (string, 'a) MParser.reply
val sum_const : 'a MParser.state -> (string, 'a) MParser.reply
val typ_ident : 'a MParser.state -> (string, 'a) MParser.reply
