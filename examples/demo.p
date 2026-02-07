type onetwo = [ One : int list, Two : {x1:int list, x2:int list} ]

type myint = int

let append = fn (arg : {x1:int list, x2:int list} )
    let l1 = arg.x1 in
    let l2 = arg.x2 in
    rec l1 {
       Nil -> l2
     | Cons(x,y) -> Cons(x,y)}
in
let l1 = Cons(1,Cons(2,Nil[int])) in
let sumall = fn (l : int list)
  rec l {
    Nil -> 0
    | Cons(x,y) -> x+y }
in
let sumOrAppend = fn (arg : onetwo)
   case arg { One l -> sumall l
            | Two x -> sumall (append x) }
in
sumOrAppend (One l1)




