open Core
open Syntax
module Var = Var.Exp_var
open Value

module Dynamics_error = struct
  type t =
    | Xvar_not_found of Var.t
    | Xprim of Prim.Op.t * Value.t list
    | Xmalformed of Exp.t
    | Xmissing_label of Exp.t * Label.t

  exception E of t

  let to_string _ = failwith "unimplemented"
end

open Dynamics_error

type 'a result = 'a Value.result

let eval_prim (prim : Prim.Op.t) arg_vals =
  let err () = raise (E (Xprim (prim, arg_vals))) in
  let bool_value b = Vinj (Label.of_string (if b then "True" else "False"), Vprod Label.Map.empty) in
  match (prim, arg_vals) with
  | Neg, [ Vconst (Cint i) ] -> Vconst (Cint (-i))
  | Neg, [ Vconst (Cfloat f) ] -> Vconst (Cfloat (-.f))
  | Neg, _ -> err ()
  | Plus, [ Vconst (Cint i1); Vconst (Cint i2) ] -> Vconst (Cint (i1 + i2))
  | Plus, [ Vconst (Cfloat f1); Vconst (Cfloat f2) ] -> Vconst (Cfloat (f1 +. f2))
  | Plus, _ -> err ()
  | Minus, [ Vconst (Cint i1); Vconst (Cint i2) ] -> Vconst (Cint (i1 - i2))
  | Minus, [ Vconst (Cfloat f1); Vconst (Cfloat f2) ] -> Vconst (Cfloat (f1 -. f2))
  | Minus, _ -> err ()
  | Times, [ Vconst (Cint i1); Vconst (Cint i2) ] -> Vconst (Cint (i1 * i2))
  | Times, [ Vconst (Cfloat f1); Vconst (Cfloat f2) ] -> Vconst (Cfloat (f1 *. f2))
  | Times, _ -> err ()
  | Div, [ Vconst (Cint i1); Vconst (Cint i2) ] -> Vconst (Cint (i1 / i2))
  | Div, [ Vconst (Cfloat f1); Vconst (Cfloat f2) ] -> Vconst (Cfloat (f1 /. f2))
  | Div, _ -> err ()
  | Mod, [ Vconst (Cint i1); Vconst (Cint i2) ] -> Vconst (Cint (i1 % i2))
  | Mod, [ Vconst (Cfloat f1); Vconst (Cfloat f2) ] -> Vconst (Cfloat Float.(mod_float f1 f2))
  | Mod, _ -> err ()
  | Sqrt, [ Vconst (Cfloat f) ] -> Vconst (Cfloat (Float.sqrt f))
  | Sqrt, _ -> err ()
  | Eq, [ Vconst (Cint i1); Vconst (Cint i2) ] -> bool_value (i1 = i2)
  | Eq, [ Vconst (Cfloat f1); Vconst (Cfloat f2) ] -> bool_value Float.(f1 = f2)
  | Eq, [ Vconst (Cstring s1); Vconst (Cstring s2) ] -> bool_value String.(s1 = s2)
  | Eq, _ -> err ()
  | Neq, [ Vconst (Cint i1); Vconst (Cint i2) ] -> bool_value (i1 <> i2)
  | Neq, [ Vconst (Cfloat f1); Vconst (Cfloat f2) ] -> bool_value Float.(f1 <> f2)
  | Neq, [ Vconst (Cstring s1); Vconst (Cstring s2) ] -> bool_value String.(s1 <> s2)
  | Neq, _ -> err ()
  | Lt, [ Vconst (Cint i1); Vconst (Cint i2) ] -> bool_value (i1 < i2)
  | Lt, [ Vconst (Cfloat f1); Vconst (Cfloat f2) ] -> bool_value Float.(f1 < f2)
  | Lt, [ Vconst (Cstring s1); Vconst (Cstring s2) ] -> bool_value String.(s1 < s2)
  | Lt, _ -> err ()
  | Lte, [ Vconst (Cint i1); Vconst (Cint i2) ] -> bool_value (i1 <= i2)
  | Lte, [ Vconst (Cfloat f1); Vconst (Cfloat f2) ] -> bool_value Float.(f1 <= f2)
  | Lte, [ Vconst (Cstring s1); Vconst (Cstring s2) ] -> bool_value String.(s1 <= s2)
  | Lte, _ -> err ()
  | Gt, [ Vconst (Cint i1); Vconst (Cint i2) ] -> bool_value (i1 > i2)
  | Gt, [ Vconst (Cfloat f1); Vconst (Cfloat f2) ] -> bool_value Float.(f1 > f2)
  | Gt, [ Vconst (Cstring s1); Vconst (Cstring s2) ] -> bool_value String.(s1 > s2)
  | Gt, _ -> err ()
  | Gte, [ Vconst (Cint i1); Vconst (Cint i2) ] -> bool_value (i1 >= i2)
  | Gte, [ Vconst (Cfloat f1); Vconst (Cfloat f2) ] -> bool_value Float.(f1 >= f2)
  | Gte, [ Vconst (Cstring s1); Vconst (Cstring s2) ] -> bool_value String.(s1 >= s2)
  | Gte, _ -> err ()
  | Concat, [ Vconst (Cstring s1); Vconst (Cstring s2) ] -> Vconst (Cstring (s1 ^ s2))
  | Concat, _ -> err ()
  | Tostring, [ Vconst (Cint i) ] -> Vconst (Cstring (Int.to_string i))
  | Tostring, [ Vconst (Cfloat i) ] -> Vconst (Cstring (Float.to_string i))
  | Tostring, _ -> err ()

(* generate : ~trace:Trace.t ~eval_dist:(Trace.t -> dist -> value) ?seed:int -> result
   where resutl = {trace:Trace.t; weight:float; value:Value.t }
   
   implement eval : ctx -> exp -> val as local function to generate

   for weigh: check if returned trace has the same size as input
*)

let bind x f =
  let { res; weight; trace } = f x.res in
  { res; weight = weight *. x.weight; trace = Trace.merge x.trace trace }

let ( let* ) = bind
let return res = { trace = Trace.empty; res; weight = 1.0 }

let generate ~trace:_ ~env ~eval_sample ?seed:_d exp =
  let rec eval ctx exp =
    let lookup v = match Map.find ctx v with None -> raise (E (Xvar_not_found v)) | Some value -> value in
    let bind_env l =
      let f ctx (x, t) = Map.add_exn ctx ~key:x ~data:t in
      List.fold ~f l ~init:ctx
    in
    let malformed e = raise (E (Xmalformed e)) in
    let open Exp in
    match out exp with
    | Evar x -> return @@ lookup x
    | Econst c -> return @@ Vconst c
    | Elam { argv; body; _ } -> return @@ Vlam (fun v -> eval (bind_env [ (argv, v) ]) body)
    | Eapp { func; arg } ->
        let* v1 = eval ctx func in
        begin match v1 with
        | Vlam f ->
            let* v2 = eval ctx arg in
            f v2
        | _ -> malformed exp
        end
    | Eprimapp { prim; args } ->
        let* v_args = eval_list ctx args in
        return (eval_prim prim v_args)
    | Einj { con; arg; _ } ->
        let* v = eval ctx arg in
        return @@ Vinj (con, v)
    | Ecase { arg; cases; _ } -> (
        let* v = eval ctx arg in
        match v with
        | Vinj (con, v) -> begin
            match Map.find cases con with
            | Some (x, e) -> eval (bind_env [ (x, v) ]) e
            | None -> raise (E (Xmissing_label (exp, con)))
          end
        | _ -> malformed exp)
    | Eprod emap ->
        let* vmap = eval_map ctx emap in
        return @@ Vprod vmap
    | Eproj { comp; arg } -> begin
        let* v = eval ctx arg in
        match v with
        | Vprod lmap -> begin
            match Map.find lmap comp with Some v' -> return v' | None -> raise (E (Xmissing_label (arg, comp)))
          end
        | _ -> malformed exp
      end
    | Elet { e1; x; e2 } ->
        let* v1 = eval ctx e1 in
        eval (bind_env [ (x, v1) ]) e2
    | Enil _ -> return @@ Vlist []
    | Econs { head; tail } ->
        let* v_hd = eval ctx head in
        let* v_tl =
          let* v = eval ctx tail in
          match v with Vlist l -> return l | _ -> malformed exp
        in
        return @@ Vlist (v_hd :: v_tl)
    | Elrec { arg; base; headv; recv; step } ->
        let* v = eval ctx arg in
        let vs = match v with Vlist vs -> vs | _ -> malformed arg in
        let rec eval_lrec = function
          | [] -> eval ctx base
          | v_head :: vs ->
              let* v_rec = eval_lrec vs in
              eval (bind_env [ (headv, v_head); (recv, v_rec) ]) step
        in
        eval_lrec vs
    | Edist dist -> return @@ Vlam (fun v -> return @@ Vdist { dist; arg = v })
    | Esamp { addr; dist } -> (
        let* v_addr = eval ctx addr in
        let* v_dist = eval ctx dist in
        match v_dist with
        | Vdist { dist; arg } -> (
            match v_addr with Vconst (Cstring s) -> eval_sample ~addr:s ~dist ~arg | _ -> malformed addr)
        | _ -> malformed dist)
  and eval_list ctx es =
    match es with
    | [] -> return []
    | e :: es ->
        let* v = eval ctx e in
        let* vs = eval_list ctx es in
        return (v :: vs)
  and eval_map ctx emap =
    let f ~key ~data:exp vmap =
      let* v = eval ctx exp in
      { vmap with res = Map.add_exn ~key ~data:v vmap.res }
    in
    Map.fold ~f ~init:(return Label.Map.empty) emap
  in

  eval env exp

let eval exp =
  let eval_sample ~addr:_ ~dist:_ ~arg:_ = failwith "Sample in deterministic execution" in
  let { res; _ } = generate ~trace:Trace.empty ~env:Var.Map.empty ~eval_sample exp in
  res

(* generate : ~trace:Trace.t ~eval_dist:(Trace.t -> dist -> value) ?seed:int -> result
   where resutl = {trace:Trace.t; weight:float; value:Value.t }
   
   implement eval : ctx -> exp -> val as local function to generate

   for weigh: check if returned trace has the same size as input
*)
