open Core
open Syntax

module Statics_error = struct
  type t =
    | Xvar_not_found of Exp.Var.t
    | Xnot_an_arrow of Typ.t
    | Xtype_mismatch of { expected : Typ.t; found : Typ.t }
    | Xprim of { prim : Prim.t; args : Typ.t list }
    | Xlabel_type of { label : Label.t; expected : Typ.t; found : Typ.t }
    | Xlabel_missing of { label : Label.t; typ : Typ.t }
    | Xnot_sum of Typ.t
    | Xnot_prod of Typ.t
    | Xcase_missing of { lable : Label.t; typ : Typ.t }
  [@@deriving sexp]

  exception E of t

  let to_string _ = failwith "implement me"
end

open Statics_error

let syn_prim_t prim arg_types =
  let open Prim in
  match (prim, List.map arg_types ~f:Typ.out) with
  | Neg, [ Tbase Bint ] -> Typ.base Bint
  | Neg, [ Tbase Bfloat ] -> Typ.base Bfloat
  | Sqrt, [ Tbase Bfloat ] -> Typ.base Bfloat
  | Plus, [ Tbase Bint; Tbase Bint ] -> Typ.base Bint
  | Plus, [ Tbase Bfloat; Tbase Bfloat ] -> Typ.base Bfloat
  | Minus, [ Tbase Bint; Tbase Bint ] -> Typ.base Bint
  | Minus, [ Tbase Bfloat; Tbase Bfloat ] -> Typ.base Bfloat
  | Times, [ Tbase Bint; Tbase Bint ] -> Typ.base Bint
  | Times, [ Tbase Bfloat; Tbase Bfloat ] -> Typ.base Bfloat
  | Div, [ Tbase Bint; Tbase Bint ] -> Typ.base Bint
  | Div, [ Tbase Bfloat; Tbase Bfloat ] -> Typ.base Bfloat
  | Mod, [ Tbase Bint; Tbase Bint ] -> Typ.base Bint
  | Mod, [ Tbase Bfloat; Tbase Bfloat ] -> Typ.base Bfloat
  | Lt, [ Tbase Bint; Tbase Bint ] -> Typ.bool
  | Lt, [ Tbase Bfloat; Tbase Bfloat ] -> Typ.bool
  | Lte, [ Tbase Bint; Tbase Bint ] -> Typ.bool
  | Lte, [ Tbase Bfloat; Tbase Bfloat ] -> Typ.bool
  | Gt, [ Tbase Bint; Tbase Bint ] -> Typ.bool
  | Gt, [ Tbase Bfloat; Tbase Bfloat ] -> Typ.bool
  | Gte, [ Tbase Bint; Tbase Bint ] -> Typ.bool
  | Gte, [ Tbase Bfloat; Tbase Bfloat ] -> Typ.bool
  | Eq, [ Tbase Bint; Tbase Bint ] -> Typ.bool
  | Eq, [ Tbase Bfloat; Tbase Bfloat ] -> Typ.bool
  | Eq, [ Tbase Bstring; Tbase Bstring ] -> Typ.bool
  | Neq, [ Tbase Bint; Tbase Bint ] -> Typ.bool
  | Neq, [ Tbase Bfloat; Tbase Bfloat ] -> Typ.bool
  | Neq, [ Tbase Bstring; Tbase Bstring ] -> Typ.bool
  | Append, [ Tbase Bstring; Tbase Bstring ] -> Typ.base Bstring
  | _ -> raise (E (Xprim { prim; args = arg_types }))

let rec syn_t gamma e =
  let lookup x = match Map.find gamma x with Some t -> t | None -> raise (E (Xvar_not_found x)) in
  let bind l =
    let f gamma (x, t) = Map.add_exn gamma ~key:x ~data:t in
    List.fold ~f l ~init:gamma
  in
  match Exp.out e with
  | Evar x -> lookup x
  | Econst c ->
      Typ.base
        begin
          match c with Cint _ -> Typ.Bint | Cfloat _ -> Typ.Bfloat | Cstring _ -> Typ.Bstring
        end
  | Elam { argv; argt; body } ->
      let t_res = syn_t (bind [ (argv, argt) ]) body in
      t_res
  | Eapp { func; arg } ->
      let argt, rest =
        match Typ.out @@ syn_t gamma func with
        | Typ.Tarr { argt; rest } -> (argt, rest)
        | t -> raise (E (Xnot_an_arrow (Typ.into t)))
      in
      let t_arg = syn_t gamma arg in
      if Typ.(t_arg = argt) then rest else raise (E (Xtype_mismatch { expected = argt; found = t_arg }))
  | Eprimapp { prim; args } ->
      let arg_types =
        let f = syn_t gamma in
        List.map args ~f
      in
      syn_prim_t prim arg_types
  | Einj { con; typ; arg } ->
      let arg_typ = syn_t gamma arg in
      let () =
        match Map.find typ con with
        | Some t when Typ.(t <> arg_typ) -> raise (E (Xlabel_type { label = con; expected = t; found = arg_typ }))
        | Some _ -> ()
        | None -> raise (E (Xlabel_missing { label = con; typ = Typ.sum typ }))
      in
      Typ.sum typ
  | Ecase { arg; cases } ->
      let arg_typ = syn_t gamma arg in
      begin
        match Typ.out arg_typ with
        | Typ.Tprod lmap ->
            let () =
              let f ~key ~data =
                match Map.find cases key with
                | Some (x, e) ->
                    if Typ.(failwith "fixme" <> syn_t (bind [ (x, data) ]) e) then
                      failwith "(E (Xtype_mismatch {expected})"
                    else ()
                | None -> raise (E (Xcase_missing { lable = key; typ = arg_typ }))
              in
              Map.iteri lmap ~f
            in
            failwith "fixme"
        | _ -> raise (E (Xnot_sum arg_typ))
      end
  | Eprod comps -> Typ.prod @@ Map.map comps ~f:(syn_t gamma)
  | Eproj { comp; arg } -> (
      let arg_typ = syn_t gamma arg in
      match Typ.out arg_typ with
      | Typ.Tprod lmap -> begin
          match Map.find lmap comp with
          | Some t -> t
          | None -> raise (E (Xlabel_missing { label = comp; typ = arg_typ }))
        end
      | _ -> raise (E (Xnot_prod arg_typ)))

let type_exp e = syn_t Exp.Var.Map.empty e
