
type opt_int = [ None : {}, Some : int ]

(* find the first occurence of a string *)
let find = fn (l : {addr : string, val : int} list) fn (addr : string)
  rec l {
      Nil -> None {}
    | Cons(x,y) ->
        if x.addr = addr then
           Some x.val
        else
           y
  }
in

let custom_sample =
  fn (observations : {addr : string, val : int} list) fn (d : int dist) fn (addr : string) 
  case find observations addr
  { None z -> sample d at addr
  | Some i -> sample uniform_int {a = i, b = i} at addr
  }
in

(* Example probabilistic model.
  NOTE: We are using custom_sample instead of sample ... at *)
let model = fn (observations : {addr : string, val : int} list)
   let x = custom_sample observations (bernoulli 0.5) "x"  in
   let y = custom_sample observations (bernoulli (1.0/(toFloat x)+1.0) ) "y" in
   x + y
in

let l =  {addr = "a1", val = 1}
       ::{addr = "a2", val = 2}
       ::{addr = "a3", val = 3}
       ::{addr = "a1", val = 4}
       ::Nil[{addr : string, val : int}]
in


model l
