open Core
open Syntax

module Statics_error = struct
  type t =
    | Xvar_not_found of Exp.Var.t
    | Xnot_an_arrow of Typ.t
    | Xtype_mismatch of { expected : Typ.t; found : Typ.t }
    | Xprim of { prim : Prim.Op.t; args : Typ.t list }
    | Xlabel_type of { label : Label.t; expected : Typ.t; found : Typ.t }
    | Xlabel_missing of { label : Label.t; typ : Typ.t }
    | Xnot_sum of Typ.t
    | Xnot_prod of Typ.t
    | Xnot_list of Typ.t
    | Xcase_type_missing
    | Xcase_missing of { lable : Label.t; typ : Typ.t }
    | Xnot_string of Typ.t
    | Xnot_dist of Typ.t
  [@@deriving sexp]

  exception E of t

  let to_string err = Sexp.to_string_hum (sexp_of_t err)
end

open Statics_error

let syn_prim_t prim arg_types =
  let open Prim.Op in
  let err () = raise (E (Xprim { prim; args = arg_types })) in
  match (prim, List.map arg_types ~f:Typ.out) with
  | Neg, [ Tbase Bint ] -> Typ.base Bint
  | Neg, [ Tbase Bfloat ] -> Typ.base Bfloat
  | Neg, _ -> err ()
  | Sqrt, [ Tbase Bfloat ] -> Typ.base Bfloat
  | Sqrt, _ -> err ()
  | Plus, [ Tbase Bint; Tbase Bint ] -> Typ.base Bint
  | Plus, [ Tbase Bfloat; Tbase Bfloat ] -> Typ.base Bfloat
  | Plus, _ -> err ()
  | Minus, [ Tbase Bint; Tbase Bint ] -> Typ.base Bint
  | Minus, [ Tbase Bfloat; Tbase Bfloat ] -> Typ.base Bfloat
  | Minus, _ -> err ()
  | Times, [ Tbase Bint; Tbase Bint ] -> Typ.base Bint
  | Times, [ Tbase Bfloat; Tbase Bfloat ] -> Typ.base Bfloat
  | Times, _ -> err ()
  | Div, [ Tbase Bint; Tbase Bint ] -> Typ.base Bint
  | Div, [ Tbase Bfloat; Tbase Bfloat ] -> Typ.base Bfloat
  | Div, _ -> err ()
  | Mod, [ Tbase Bint; Tbase Bint ] -> Typ.base Bint
  | Mod, [ Tbase Bfloat; Tbase Bfloat ] -> Typ.base Bfloat
  | Mod, _ -> err ()
  | Lt, [ Tbase Bint; Tbase Bint ] -> Typ.bool
  | Lt, [ Tbase Bfloat; Tbase Bfloat ] -> Typ.bool
  | Lt, [ Tbase Bstring; Tbase Bstring ] -> Typ.bool
  | Lt, _ -> err ()
  | Lte, [ Tbase Bint; Tbase Bint ] -> Typ.bool
  | Lte, [ Tbase Bfloat; Tbase Bfloat ] -> Typ.bool
  | Lte, [ Tbase Bstring; Tbase Bstring ] -> Typ.bool
  | Lte, _ -> err ()
  | Gt, [ Tbase Bint; Tbase Bint ] -> Typ.bool
  | Gt, [ Tbase Bfloat; Tbase Bfloat ] -> Typ.bool
  | Gt, [ Tbase Bstring; Tbase Bstring ] -> Typ.bool
  | Gt, _ -> err ()
  | Gte, [ Tbase Bint; Tbase Bint ] -> Typ.bool
  | Gte, [ Tbase Bfloat; Tbase Bfloat ] -> Typ.bool
  | Gte, [ Tbase Bstring; Tbase Bstring ] -> Typ.bool
  | Gte, _ -> err ()
  | Eq, [ Tbase Bint; Tbase Bint ] -> Typ.bool
  | Eq, [ Tbase Bfloat; Tbase Bfloat ] -> Typ.bool
  | Eq, [ Tbase Bstring; Tbase Bstring ] -> Typ.bool
  | Eq, _ -> err ()
  | Neq, [ Tbase Bint; Tbase Bint ] -> Typ.bool
  | Neq, [ Tbase Bfloat; Tbase Bfloat ] -> Typ.bool
  | Neq, [ Tbase Bstring; Tbase Bstring ] -> Typ.bool
  | Neq, _ -> err ()
  | Concat, [ Tbase Bstring; Tbase Bstring ] -> Typ.base Bstring
  | Concat, _ -> err ()
  | Tostring, [ Tbase Bint ] -> Typ.base Bstring
  | Tostring, [ Tbase Bfloat ] -> Typ.base Bstring
  | Tostring, _ -> err ()
  | Cons, [ t1; Tlist t2 ] when Typ.(Typ.into t1 = t2) -> Typ.list t2
  | Cons, _ -> err ()

let dist_type dist =
  let open Prim.Dist in
  let t_int = Typ.base Bint in
  let t_float = Typ.base Bfloat in
  let t_prod l =
    let f (s, t) = (Label.of_string s, t) in
    Typ.prod (Label.Map.of_alist_exn (List.map ~f l))
  in
  match dist with
  | Dbinomial -> (t_prod [ ("p", t_float); ("n", t_int) ], t_int)
  | Dbernoulli -> (t_float, t_int)
  | Dcategorical -> (Typ.list t_float, t_int)
  | Duniform_int -> (t_prod [ ("a", t_int); ("b", t_int) ], t_int)

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
  | Elam { argv; argt; body } ->
      let rest = syn_t (bind [ (argv, argt) ]) body in
      Typ.arr ~argt ~rest
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
  | Enil t -> Typ.list t
  | Econs { head; tail } ->
      let t_head = syn_t gamma head in
      let t_tail = syn_t gamma tail in
      begin match Typ.out t_tail with
      | Typ.Tlist t_elm when Typ.(t_elm = t_head) -> t_tail
      | _ -> raise (E (Xtype_mismatch { expected = Typ.list t_head; found = t_tail }))
      end
  | Elrec { arg; base; headv; recv; step } ->
      let t_arg = syn_t gamma arg in
      begin match Typ.out t_arg with
      | Tlist t_elem ->
          let t_base = syn_t gamma base in
          let t_step = syn_t (bind [ (headv, t_elem); (recv, t_base) ]) step in
          if Typ.(t_base = t_step) then t_base else raise (E (Xtype_mismatch { expected = t_base; found = t_step }))
      | _ -> raise (E (Xnot_list t_arg))
      end
  | Esamp { addr; dist } -> (
      let () =
        let addr_type = syn_t gamma addr in
        match Typ.out addr_type with Typ.Tbase Bstring -> () | _ -> raise (E (Xnot_string addr_type))
      in
      let t_dist = syn_t gamma dist in
      match Typ.out t_dist with Typ.Tdist t -> t | _ -> raise (E (Xnot_dist t_dist)))
  | Edist dist ->
      let argt, rest = dist_type dist in
      Typ.arr ~argt ~rest:(Typ.dist rest)

let type_exp e =
  try syn_t Exp.Var.Map.empty e
  with E err ->
    let _ = Printf.printf "Type error: %s" (to_string err) in
    exit 1
