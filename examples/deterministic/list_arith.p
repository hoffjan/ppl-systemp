
type arith = [li : float list, lili : float list list, fl : float ]

let add = fn (l : float list)
  rec l { Nil -> 0.0
        | Cons (x,r) -> x + r }
in

let l = Cons(1.1,
        Cons(2.2,
        Cons(3.0,
        Cons(199.199,	
	Nil[float]))))
in

let multadd = fn (l : float list list)
  rec l { Nil -> 1.0
        | Cons(x,r) -> (addall x) * r }
in

let l2 = (Cons (l, Cons (l, Nil[float list])))

let addall = fn (l : arith list)
  rec l
    { Nil -> 0.0
    | Cons

{ res1 = add l
, res2 = multadd l2
}
