let p = 0.7 in
let n = 1::2::3::4::5::6::7::8::9::10::Nil[int] in
let dist = bern p in
rec n
  { Nil -> 0
  | Cons(x,y) ->
      let label = "a" ^ (toString x) in
      let r = sample dist at label in
      y+r
  }
