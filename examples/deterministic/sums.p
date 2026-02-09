
type intd = [A : {} ,  B : {} ]
type test2 = [A2 : {} ,  B2 : [A : {} ,  B : {} ] ]   
let id	   = fn (x : [A : {}, B : {} ] ) x in
let xx	   =  case B2 (id (A {})) { A2 x -> id |  B2 x -> id } A {} in
fn (x : intd) x
