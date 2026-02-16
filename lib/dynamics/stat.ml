open Core
open Syntax
module Owl = Owl_stats

let init seed = Owl_stats_prng.init seed

exception Dist_arg_mismatch of Value.t

let wrong_arg v = raise (Dist_arg_mismatch v)

let prod_to_list v =
  match v with
  | Value.Vprod lmap ->
      let f ~key ~data l = (Label.to_string key, data) :: l in
      Map.fold lmap ~init:[] ~f
  | _ -> wrong_arg v

let list_to_array v =
  match v with
  | Value.Vlist fs ->
      let fs =
        let f x = match x with Value.Vconst (Cfloat f) -> f | _ -> wrong_arg x in
        List.map fs ~f
      in
      Array.of_list fs
  | _ -> wrong_arg v

let sample ~dist ~arg =
  let open Prim.Dist in
  let open Value in
  match (dist, arg) with
  | Dbinomial, _ -> begin
      match prod_to_list arg with
      | [ ("p", Vconst (Cfloat p)); ("n", Vconst (Cint n)) ] ->
          let sample = Owl.binomial_rvs ~n ~p in
          Vconst (Cint sample)
      | _ -> wrong_arg arg
    end
  | Dbernoulli, Vconst (Cfloat p) ->
      let sample = Owl.binomial_rvs ~n:1 ~p in
      Vconst (Cint sample)
  | Dbernoulli, _ -> wrong_arg arg
  | Dcategorical, v ->
      let p = list_to_array v in
      let sample = Owl.categorical_rvs p in
      Vconst (Cint sample)

let weigh ~dist ~arg v =
  let open Prim.Dist in
  let open Value in
  match (dist, arg, v) with
  | Dbinomial, _, Vconst (Cint x) -> begin
      match prod_to_list arg with
      | [ ("p", Vconst (Cfloat p)); ("n", Vconst (Cint n)) ] -> Owl.binomial_logpdf ~n ~p x
      | _ -> wrong_arg arg
    end
  | Dbinomial, _, _ -> wrong_arg v
  | Dbernoulli, Vconst (Cfloat p), Vconst (Cint x) -> Owl.binomial_logpdf ~n:1 ~p x
  | Dbernoulli, Vconst (Cfloat _), _ -> wrong_arg arg
  | Dbernoulli, param, _ -> wrong_arg param
  | Dcategorical, v_param, Vconst (Cint n) ->
      let p = list_to_array v_param in
      let weight = if Int.(0 <= n) && Int.(n < Array.length p) then Array.get p n else 0.0 in
      log weight
  | Dcategorical, _, v -> wrong_arg v
