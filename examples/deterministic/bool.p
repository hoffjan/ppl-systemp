let a =
  let t = True {} in
  case 2>3 { True x -> True x | False x -> False x }
in
let b =
  let ff = false in
  if ff then true else false
in
{ a = a, b = b}
