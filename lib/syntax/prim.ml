open Core

module Op = struct
  module T = struct
    type t =
      | Neg
      | Plus
      | Minus
      | Times
      | Div
      | Mod
      | Eq
      | Neq
      | Lt
      | Lte
      | Gt
      | Gte
      | Sqrt
      | Concat
      | Tostring
      | Cons
      | Tofloat
    [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

module Dist = struct
  module T = struct
    type t = Dbinomial | Dbernoulli | Dcategorical | Duniform_int [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end
