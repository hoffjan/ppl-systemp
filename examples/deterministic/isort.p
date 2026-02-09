let insert = fn (l: int list) fn (x:int) 
  let res =
    rec l
      { Nil -> { li = Nil[int], smallest = x }
      | Cons (y,r) ->
        let z = r.smallest in
        case y<z
          { True n -> { li = Cons(z,r.li), smallest = y }
            | False n -> { li = Cons(y,r.li), smallest = z } } }
  in
    Cons(res.smallest,res.li)
in

let insert = fn (l : int list)
  rec l
  { Nil -> fn (x : int) Cons(x,Nil[int])
  | Cons(y,f) ->
    fn (x : int)
      case x<y
      { True n -> Cons (x, f y)
      | False n -> Cons(y, f x) } }   
in

let isort = fn (l : int list)
  rec l
    { Nil -> Nil[int]
    | Cons (x,r) -> insert r x }
in

let append = fn (l1 : int list) fn (l2 : int list)
  rec l1 { Nil -> l2 | Cons(x,r) -> Cons(x,r) }
in    

let l = Cons(9,Cons(5,Cons(8,Cons(-1,Nil[int])))) in

{ res1 = isort l, res2 = isort (append l (append l l)) }
		 
