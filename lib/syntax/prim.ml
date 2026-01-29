open Core

module T = struct
  type t = Neg | Plus | Minus | Times | Div | Mod | Eq | Neq | Lt | Lte | Gt | Gte | Sqrt | Append
  [@@deriving compare, sexp]
end

include T
include Comparable.Make (T)
