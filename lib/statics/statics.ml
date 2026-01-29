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
    | Xnot_list of Typ.t
    | Xcase_type_missing
    | Xcase_missing of { lable : Label.t; typ : Typ.t }
  [@@deriving sexp]

  exception E of t

  let to_string err = Sexp.to_string_hum (sexp_of_t err)
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
        begin match c with Cint _ -> Typ.Bint | Cfloat _ -> Typ.Bfloat | Cstring _ -> Typ.Bstring
        end
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
  | Ecase { arg; cases; typ } ->
      let arg_typ = syn_t gamma arg in
      begin match Typ.out arg_typ with
      | Typ.Tsum lmap ->
          let types =
            let f ~key ~data types =
              match Map.find cases key with
              | Some (x, e) ->
                  let t = syn_t (bind [ (x, data) ]) e in
                  t :: types
              | None -> raise (E (Xcase_missing { lable = key; typ = arg_typ }))
            in
            Map.fold lmap ~f ~init:(match typ with None -> [] | Some t -> [ t ])
          in
          begin match types with
          | [] -> raise (E Xcase_type_missing)
          | typ :: types ->
              let () =
                let f t = if Typ.(typ <> t) then raise (E (Xtype_mismatch { expected = typ; found = t })) else () in
                List.iter types ~f
              in
              typ
          end
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
  | Elet { e1; x; e2 } ->
      let t1 = syn_t gamma e1 in
      syn_t (bind [ (x, t1) ]) e2
  | Elam _ -> failwith "ASSIGNMENT 1: Implement me"
  | Eapp _ -> failwith "ASSIGNMENT 1: Implement me"
  | Enil _ -> failwith "ASSIGNMENT 1: Implement me"
  | Econs _ -> failwith "ASSIGNMENT 1: Implement me"
  | Elrec _ -> failwith "ASSIGNMENT 1: Implement me"

let type_exp e =
  try syn_t Exp.Var.Map.empty e
  with E err ->
    let _ = Printf.printf "Type error: %s" (to_string err) in
    exit 1
