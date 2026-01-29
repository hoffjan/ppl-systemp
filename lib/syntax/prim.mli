open Core

type t = Neg | Plus | Minus | Times | Div | Mod | Eq | Neq | Lt | Lte | Gt | Gte | Sqrt | Append

include Comparable.S with type t := t
include Sexpable.S with type t := t
