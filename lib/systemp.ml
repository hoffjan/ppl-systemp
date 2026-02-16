open Core
module Statics = Statics
module Parser = Parser
module Syntax = Syntax
module Dynamics = Dynamics

let model_of_file file_name =
  let str = In_channel.read_all file_name in
  let exp = Parser.exp str in
  exp

let exp_of_str = Parser.exp

let model_of_string str =
  let exp_trace = Parser.trace str in
  Dynamics.Trace.map exp_trace ~f:(fun exp -> Dynamics.eval exp)

let simulate ?seed exp =
  let _ = Statics.type_exp exp in
  let () = match seed with Some s -> Dynamics.init s | None -> () in
  Dynamics.simulate exp

let assess trace exp =
  let _ = Statics.type_exp exp in
  Dynamics.assess trace exp

let eval exp =
  let _ = Statics.type_exp exp in
  Dynamics.eval exp

include Dynamics.Pprint
