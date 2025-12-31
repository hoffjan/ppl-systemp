open Core
open Syntax

module Statics_error = struct
  type t =
    | Xvar_not_found of Exp.Var.t
    | Xnot_an_arrow of Typ.t
    | Xtype_mismatch of { expected : Typ.t; found : Typ.t }
  [@@deriving sexp]

  exception E of t

  let to_string _ = failwith "implement me"
end

open Statics_error

type context = Typ.t Exp.Var.Map.t

let rec syn_t (gamma : context) e =
  let lookup x = match Map.find gamma x with Some t -> t | None -> raise (E (Xvar_not_found x)) in
  let add l =
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
      let t_res = syn_t (add [ (argv, argt) ]) body in
      t_res
  | Eapp { func; arg } ->
      let argt, rest =
        match Typ.out @@ syn_t gamma func with
        | Typ.Tarr { argt; rest } -> (argt, rest)
        | t -> raise (E (Xnot_an_arrow (Typ.into t)))
      in
      let t_arg = syn_t gamma arg in
      if Typ.(t_arg = argt) then rest else raise (E (Xtype_mismatch { expected = argt; found = t_arg }))
(* | Eprimapp of { prim : Prim.t; args : t list } *)
(* | Einj of { con : Label.t; typ : Typ.t Label.Map.t; arg : t } *)
(* | Ecase of { arg : t; cases : (Var.t * t) Label.Map.t } *)
(* | Eprod of t Label.Map.t *)
(* | Eproj of { comp : Label.t; arg : t } *)

let type_exp e = syn_t Exp.Var.Map.empty e
