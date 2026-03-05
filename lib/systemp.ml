open Core
module Statics = Statics
module Parser = Parser
module Syntax = Syntax
module Dynamics = Dynamics

let exp_of_str str =
  let exp = Parser.exp str in
  let _ = Statics.type_exp exp in
  exp

let model_of_file file_name =
  let str = In_channel.read_all file_name in
  exp_of_str str

let trace_of_string str =
  let exp_trace = Parser.trace str in
  Dynamics.Trace.map exp_trace ~f:(fun exp ->
      let _ = Statics.type_exp exp in
      Dynamics.eval exp)

let with_arg arg func model =
  match arg with
  | None ->
      let _ = Statics.type_exp model in
      func model
  | Some arg ->
      let exp = Syntax.Exp.app ~func:model ~arg in
      let _ = Statics.type_exp exp in
      func exp

let simulate ?seed ?arg model =
  let () = match seed with Some s -> Dynamics.init s | None -> () in
  with_arg arg Dynamics.simulate model

let assess ?arg trace exp = with_arg arg (Dynamics.assess trace) exp
let eval ?arg exp = with_arg arg Dynamics.eval exp

include Dynamics.Pprint
