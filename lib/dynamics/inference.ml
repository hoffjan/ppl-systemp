open Syntax

type trace = Value.t Trace.t

(* The call rejection_sampling_random_runtime f n should return a list l of traces
   such that the length of l is n and f t for all traces t in l *)
let rejection_sampling_random_runtime : model:Exp.t -> f:(trace -> bool) -> int -> trace list =
 fun ~model ~f n ->
  let _ = Statics.type_exp model in
  let rec sample n acc =
    if n < 1 then acc
    else
      let trace = (Eval.simulate model).trace in
      let n, acc = if f trace then (n - 1, trace :: acc) else (n, acc) in
      sample n acc
  in
  sample n []

(* The call rejection_sampling_fixed_runtime f n should return a list l of traces
   such that of the length of l is n and if (t,w) in l then w = 0.0 or w = 1.0 . *)
let rejection_sampling_fixed_runtime : model:Exp.t -> f:(trace -> bool) -> int -> (trace * float) list =
 fun ~model ~f n ->
  let _ = Statics.type_exp model in
  let rec sample n acc =
    if n < 0 then acc
    else
      let trace = (Eval.simulate model).trace in
      let weight = if f trace then 1.0 else 0.0 in
      sample (n - 1) ((trace, weight) :: acc)
  in
  sample n []

let importance_resampling =
 fun ~model ~observations num_samples ->
  let open Value in
  let rec sample num_samples =
    if num_samples < 2 then Eval.generate ~trace:observations model
    else
      let rec_result = sample (num_samples - 1) in
      let cand_result = Eval.generate ~trace:observations model in
      let log_weight = cand_result.weight in
      let log_total_weight = Float.log (Float.exp rec_result.weight +. Float.exp log_weight) in
      if Owl_stats.binomial_rvs ~n:1 ~p:(exp (log_weight -. log_total_weight)) = 1 then
        { cand_result with weight = log_total_weight }
      else { rec_result with weight = log_total_weight }
  in
  let result = sample num_samples in
  { result with weight = result.weight -. Float.log (Float.of_int num_samples) }
