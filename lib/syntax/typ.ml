open Core

type t_base = Bint | Bfloat | Bstring [@@deriving compare, sexp]

module Var = Var.Typ_var

type t =
  | BASE of t_base
  | ARR of { argt : t; rest : t }
  | SUM of t Label.Map.t
  | PROD of t Label.Map.t
  | LIST of t
  | DIST of t
[@@deriving sexp, compare]

type view =
  | Tbase of t_base
  | Tarr of { argt : t; rest : t }
  | Tsum of t Label.Map.t
  | Tprod of t Label.Map.t
  | Tlist of t
  | Tdist of t

let into = function
  | Tbase b -> BASE b
  | Tarr { argt; rest } -> ARR { argt; rest }
  | Tsum ts -> SUM ts
  | Tprod ts -> PROD ts
  | Tlist t -> LIST t
  | Tdist t -> DIST t

let out = function
  | BASE b -> Tbase b
  | ARR { argt; rest } -> Tarr { argt; rest }
  | SUM ts -> Tsum ts
  | PROD ts -> Tprod ts
  | LIST t -> Tlist t
  | DIST t -> Tdist t

let subst _ _ t2 = t2
let frees _ = Var.Set.empty
let base bt = into @@ Tbase bt
let arr ~argt ~rest = into @@ Tarr { argt; rest }
let sum ts = into @@ Tsum ts
let prod ts = into @@ Tprod ts
let list t = into @@ Tlist t
let dist t = into @@ Tdist t

let bool =
  sum
    (Label.Map.of_alist_exn
       [ (Label.of_string "True", prod Label.Map.empty); (Label.of_string "False", prod Label.Map.empty) ])

module Comp = Comparable.Make (struct
  type s = t [@@deriving compare, sexp]
  type t = s [@@deriving compare, sexp]
end)

let rec to_string : t -> string =
 fun t ->
  match out t with
  | Tbase Bint -> "int"
  | Tbase Bfloat -> "float"
  | Tbase Bstring -> "string"
  | Tarr { argt; rest } -> Printf.sprintf "(%s -> %s)" (to_string argt) (to_string rest)
  | Tsum ts ->
      if Comp.(t = bool) then "bool"
      else
        let summand (l, ty) = Printf.sprintf "`%s of %s" (Label.to_string l) (to_string ty) in
        Printf.sprintf "[%s]" (String.concat ~sep:" | " (List.map (Map.to_alist ts) ~f:summand))
  | Tprod lmaptyp ->
      let factor (label, ty) = Printf.sprintf "%s = %s" (Label.to_string label) (to_string ty) in
      Printf.sprintf "{%s}" (String.concat ~sep:", " (List.map (Map.to_alist lmaptyp) ~f:factor))
  | Tlist t -> to_string t ^ " list"
  | Tdist t -> to_string t ^ " dist"

let unit = prod Label.Map.empty

include Comp
