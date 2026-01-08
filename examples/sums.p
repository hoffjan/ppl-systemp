
type test = [A : <> ,  B : <> ]
type test2 = [A2 : <> ,  B2 : [A : <> ,  B : <> ] ]   
let id = fn (x : [A : <>, B : <> ] ) x in
case[[A:<>,B:<>] -> [A:<>,B:<>]] B2 (id (A <>)) { A2 x -> id |  B2 x -> id } A <>
