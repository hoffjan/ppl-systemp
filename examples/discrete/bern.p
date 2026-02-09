
let a = sample binom {n=1, p=0.5} at "a" in
let param = {n = a, p=0.9} in
let addr = "b" in
sample binom param at addr
