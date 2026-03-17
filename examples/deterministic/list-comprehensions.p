
let l1 = [1,2,3,4] in
let l2 = [ [x*10] | x <- l1 ] in
let l3 = [ 1::y | y <- l2 ] in

{ a = l1
, b = l2
, c = l3
}
