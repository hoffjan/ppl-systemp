
let l = Cons(0.5,(Cons(0.8,Cons(0.2,Cons(0.3,Nil[float]))))) in

rec l { Nil -> 1
      | Cons(p,y) ->
        let label = "a" ^ (toString y) in
	let x = sample binomial {n=1, p=p} at label in
	y+1 }

