let rain = sample binomial { n=1, p=0.8} at "rain" in
let traffic =
  let p = if rain = 1 then 0.8 else 0.4 in
  sample bernoulli p at "traffic"
in
let late =
  let p = if traffic = 1 then 0.9 else 0.2 in
  sample bernoulli p at "late"
in
late
