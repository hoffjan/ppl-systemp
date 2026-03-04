open Core

module Op : sig
  type t = Neg | Plus | Minus | Times | Div | Mod | Eq | Neq | Lt | Lte | Gt | Gte | Sqrt | Concat | Tostring | Cons

  include Comparable.S with type t := t
  include Sexpable.S with type t := t
end

module Dist : sig
  type t = Dbinomial | Dbernoulli | Dcategorical | Duniform_int

  include Comparable.S with type t := t
  include Sexpable.S with type t := t
end
