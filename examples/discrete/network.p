
let rain = sample bernoulli 0.4 at "rain" in

let traffic =
  let pTraffic = if rain = 1 then 0.8 else 0.2 in
  sample bernoulli pTraffic at "traffic"
in

let noBreakfast =
  sample bernoulli 0.5 at "no_breakfast"
in

let late =
  let pLate = if traffic = 1 then 0.9 else 0.2 in
  sample bernoulli pLate at "late"
in

{  meltdown = late + noBreakfast = 2 }

