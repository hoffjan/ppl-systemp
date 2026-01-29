open Core
open Syntax
module Var = Var.Exp_var

type value =
  | Vconst of Exp.const
  | Vlam of (value -> value)
  | Vinj of Label.t * value
  | Vprod of value Label.Map.t
  | Vlist of value List.t

module Dynamics_error = struct
  type t =
    | Xvar_not_found of Var.t
    | Xprim of Prim.t * value list
    | Xmalformed of Exp.t
    | Xmissing_label of Exp.t * Label.t

  exception E of t

  let to_string _ = failwith "unimplemented"
end

open Dynamics_error

let eval_prim (prim : Prim.t) arg_vals =
  let bool_value b = Vinj (Label.of_string (if b then "True" else "False"), Vprod Label.Map.empty) in
  match (prim, arg_vals) with
  | Neg, [ Vconst (Cint i) ] -> Vconst (Cint (-i))
  | Neg, [ Vconst (Cfloat f) ] -> Vconst (Cfloat (-.f))
  | Plus, [ Vconst (Cint i1); Vconst (Cint i2) ] -> Vconst (Cint (i1 + i2))
  | Plus, [ Vconst (Cfloat f1); Vconst (Cfloat f2) ] -> Vconst (Cfloat (f1 +. f2))
  | Minus, [ Vconst (Cint i1); Vconst (Cint i2) ] -> Vconst (Cint (i1 - i2))
  | Minus, [ Vconst (Cfloat f1); Vconst (Cfloat f2) ] -> Vconst (Cfloat (f1 -. f2))
  | Times, [ Vconst (Cint i1); Vconst (Cint i2) ] -> Vconst (Cint (i1 * i2))
  | Times, [ Vconst (Cfloat f1); Vconst (Cfloat f2) ] -> Vconst (Cfloat (f1 *. f2))
  | Div, [ Vconst (Cint i1); Vconst (Cint i2) ] -> Vconst (Cint (i1 / i2))
  | Div, [ Vconst (Cfloat f1); Vconst (Cfloat f2) ] -> Vconst (Cfloat (f1 /. f2))
  | Mod, [ Vconst (Cint i1); Vconst (Cint i2) ] -> Vconst (Cint (i1 % i2))
  | Mod, [ Vconst (Cfloat f1); Vconst (Cfloat f2) ] -> Vconst (Cfloat Float.(mod_float f1 f2))
  | Eq, [ Vconst (Cint i1); Vconst (Cint i2) ] -> bool_value (i1 = i2)
  | Eq, [ Vconst (Cfloat f1); Vconst (Cfloat f2) ] -> bool_value Float.(f1 = f2)
  | Neq, [ Vconst (Cint i1); Vconst (Cint i2) ] -> bool_value (i1 <> i2)
  | Neq, [ Vconst (Cfloat f1); Vconst (Cfloat f2) ] -> bool_value Float.(f1 <> f2)
  | Lt, [ Vconst (Cint i1); Vconst (Cint i2) ] -> bool_value (i1 < i2)
  | Lt, [ Vconst (Cfloat f1); Vconst (Cfloat f2) ] -> bool_value Float.(f1 < f2)
  | Lte, [ Vconst (Cint i1); Vconst (Cint i2) ] -> bool_value (i1 <= i2)
  | Lte, [ Vconst (Cfloat f1); Vconst (Cfloat f2) ] -> bool_value Float.(f1 <= f2)
  | Gt, [ Vconst (Cint i1); Vconst (Cint i2) ] -> bool_value (i1 > i2)
  | Gt, [ Vconst (Cfloat f1); Vconst (Cfloat f2) ] -> bool_value Float.(f1 > f2)
  | Gte, [ Vconst (Cint i1); Vconst (Cint i2) ] -> bool_value (i1 >= i2)
  | Gte, [ Vconst (Cfloat f1); Vconst (Cfloat f2) ] -> bool_value Float.(f1 >= f2)
  | _ -> raise (E (Xprim (prim, arg_vals)))

let rec eval ctx exp =
  let lookup v = match Map.find ctx v with None -> raise (E (Xvar_not_found v)) | Some value -> value in
  let bind l =
    let f ctx (x, t) = Map.add_exn ctx ~key:x ~data:t in
    List.fold ~f l ~init:ctx
  in
  let malformed e = raise (E (Xmalformed e)) in
  let open Exp in
  match out exp with
  | Evar x -> lookup x
  | Econst c -> Vconst c
  | Eprimapp { prim; args } ->
      let v_args = List.map ~f:(eval ctx) args in
      eval_prim prim v_args
  | Einj { con; arg; _ } -> Vinj (con, eval ctx arg)
  | Ecase { arg; cases; _ } -> (
      match eval ctx arg with
      | Vinj (con, v) -> begin
          match Map.find cases con with
          | Some (x, e) -> eval (bind [ (x, v) ]) e
          | None -> raise (E (Xmissing_label (exp, con)))
        end
      | _ -> malformed exp)
  | Eprod lmap -> Vprod (Map.map ~f:(eval ctx) lmap)
  | Eproj { comp; arg } -> begin
      match eval ctx arg with
      | Vprod lmap -> begin
          match Map.find lmap comp with Some v -> v | None -> raise (E (Xmissing_label (arg, comp)))
        end
      | _ -> malformed exp
    end
  | Elet { e1; x; e2 } ->
      let v1 = eval ctx e1 in
      eval (bind [ (x, v1) ]) e2
  | Elam _ -> failwith "ASSIGNMENT 1: Implement me"
  | Eapp _ -> failwith "ASSIGNMENT 1: Implement me"
  | Enil _ -> failwith "ASSIGNMENT 1: Implement me"
  | Econs _ -> failwith "ASSIGNMENT 1: Implement me"
  | Elrec _ -> failwith "ASSIGNMENT 1: Implement me"

let eval = eval Var.Map.empty
