let a = sample bernoulli 0.5 at "a" in
let b =
  case a = 1 {
      True x -> sample binomial {n=1, p=0.9} at "b1"
    | False y -> sample binomial {n=1, p=0.1} at "b2" }
in
{ a = a, b =b }
