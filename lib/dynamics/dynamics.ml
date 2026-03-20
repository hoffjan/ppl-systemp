module Trace = Trace
module Value = Value
module Pprint = Pprint
module Inference = Inference

type trace = Value.t Trace.t

let init = Stat.init
let simulate = Eval.simulate
let eval = Eval.eval
let assess = Eval.assess
let generate = Eval.generate
