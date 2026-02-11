open Core
open Syntax
module O = Owl_stats

let init seed = Owl_stats_prng.init seed

exception Dist_arg_mismatch of Value.t

let wrong_arg v = raise (Dist_arg_mismatch v)

let prod_to_list v =
  match v with
  | Value.Vprod lmap ->
      let f ~key ~data l = (Label.to_string key, data) :: l in
      Map.fold lmap ~init:[] ~f
  | _ -> wrong_arg v

let sample ~dist ~arg =
  let open Prim.Dist in
  let open Value in
  match (dist, arg) with
  | Dbinomial, _ -> begin
      match prod_to_list arg with
      | [ ("p", Vconst (Cfloat p)); ("n", Vconst (Cint n)) ] ->
          let sample = O.binomial_rvs ~n ~p in
          Vconst (Cint sample)
      | _ -> wrong_arg arg
    end
  | Dbernoulli, Vconst (Cfloat p) ->
      let sample = O.binomial_rvs ~n:1 ~p in
      Vconst (Cint sample)
  | Dbernoulli, _ -> wrong_arg arg

let weigh ~dist ~arg v =
  let open Prim.Dist in
  let open Value in
  match (dist, arg, v) with
  | Dbinomial, _, Vconst (Cint x) -> begin
      match prod_to_list arg with
      | [ ("p", Vconst (Cfloat p)); ("n", Vconst (Cint n)) ] -> O.binomial_pdf ~n ~p x
      | _ -> wrong_arg arg
    end
  | Dbinomial, _, _ -> wrong_arg v
  | Dbernoulli, Vconst (Cfloat p), Vconst (Cint x) -> O.binomial_pdf ~n:1 ~p x
  | Dbernoulli, Vconst (Cfloat _), _ -> wrong_arg arg
  | Dbernoulli, param, _ -> wrong_arg param
