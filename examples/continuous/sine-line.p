
let enumerate = fn (xs : float list)
  let r =
    rec xs {
      Nil -> fn (n:int) Nil
    | Cons (x,y) -> fn (n:int) let tail = y (n+1) in {data = x, index = n}::tail
     }
  in
  r 0
in

let pi = 3.14159 in

let line_model_fancy = fn (xs : float list) "implement me"

let sine_model_fancy = fn (xs : float list) "implement me"

let combined_model = fn (xs : float list)
  let b = sample bernoulli 0.5 at "is_line" in
  "implement me"

let xs = [-5.0, -4.0, -3.0, -2.0, -1.0, 0.0, 1.0, 2.0, 3.0, 4.0, 5.0] in

combined_model xs



