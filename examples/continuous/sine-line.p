
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

let noisy_sample = fn (y : float) fn(index : int) fn (noise : float)
  sample normal {mu = y, sigma = noise} at "y" ^ toString(index)
in  

let line_model_fancy = fn (xs : float list)

  let slope =  sample normal {mu = 0.0, sigma = 1.0} at "slope" in
  let intercept = sample normal {mu = 0.0, sigma = 2.0} at "intercept" in
    
  let f = fn (x:float) slope * x + intercept in

  let noise = sample gamma {shape = 1.0, scale = 1.0} at "noise" in

  [ noisy_sample (f x.data) x.index noise | x <- enumerate xs  ]

in

let sine_model_fancy = fn (xs : float list) "implement me" in

let combined_model = fn (xs : float list)
  let b = sample bernoulli 0.5 at "is_line" in
  "implement me"
in

let xs = [-5.0, -4.0, -3.0, -2.0, -1.0, 0.0, 1.0, 2.0, 3.0, 4.0, 5.0] in

combined_model xs



