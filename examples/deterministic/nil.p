(* nil without type annotations *)

let t = rec 1::Nil { Nil -> Nil | Cons(x,y) -> Cons(x,y) } in

let x = if true then fn (x:int) Nil else fn (x:int) 1::Nil in

{a = Nil, b = Cons(1,Nil), c = t, d = x}
