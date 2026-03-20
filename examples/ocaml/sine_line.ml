open Core
open Systemp
open Dynamics

let xs = [ -5.0; -4.0; -3.0; -2.0; -1.0; 0.0; 1.0; 2.0; 3.0; 4.0; 5.0 ]

let sine_line_trace amp =
  let f i x =
    let y = (((0.25 *. x) +. 0.3) *. (1.2 -. amp)) +. (amp *. Float.sin (x +. 3.)) in
    ("y" ^ Int.to_string i, Value.Vconst (Syntax.Exp.Cfloat y))
  in
  let l = List.mapi ~f xs in
  Dynamics.Trace.of_list l

let model = model_of_file "examples/continuous/sine-line.p"
let traces = List.map ~f:(fun amp -> (amp, sine_line_trace amp)) [ 0.0; 0.4; 0.8; 1.2 ]
