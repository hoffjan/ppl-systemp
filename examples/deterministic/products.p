
let id = fn (x : {}) x in
let prod = { left = id , right = {one = {}} } in (prod.left prod.right.one)
