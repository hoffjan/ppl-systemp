
let append = fn (l1 : float list) fn (l2 : float list)
             rec l1 { Nil -> l2 | Cons(x,r) -> Cons(x,r) }
in
let l = Cons(1.1,Cons (1.2,Nil[float])) in
let l = append l l in
let l = append l l in
let t = Cons(1,Nil[int]) in
{ left = append l (append l l), right = l }
