let ten = 2 * -2 + 14 in
{ a1 = 2 * case (ten = 11) { False x -> 5 | True x -> 6 } < 6+6, a2 = ten }
