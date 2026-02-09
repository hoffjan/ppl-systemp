let nil = Nil[int->int] in
let nil2 = Nil[(int -> int) list] in
let l = let id = fn (x:int) x in Cons(id,Cons(id, nil)) in
let l2 = Cons(l,nil2) in
let id = fn (x : (int -> int) list) x in
let whoCares = Cons(id (id l), nil2) in
rec l {Nil -> nil | Cons (x,res) -> Cons(x,res) }
