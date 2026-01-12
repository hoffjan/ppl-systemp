let ten = 2 * -2 + 14 in
let fl = 1.2 - -1.1 * 1.5 * 55.55 + 1.1 in
let fl2 = -fl + 4.5 / 4.4 in
{ a1 = 2 * case (ten = 11) { False x -> 5 | True x -> 6 } < 6+6,
  a2 = ten ,
  a3 = fl2,
  a4 = fl2 < 3.4 }
