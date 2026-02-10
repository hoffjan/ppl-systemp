let a = sample binom {n=1, p=0.5} at "a" in
let b =
  case a = 1 {
      True x -> sample binom {n=1, p=0.9} at "b1"
    | False y -> sample binom {n=1, p=0.1} at "b2" }
in
{ a = a, b =b }
