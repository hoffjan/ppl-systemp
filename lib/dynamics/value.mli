open Syntax

type t =
  | Vconst of Exp.const
  | Vlam of (t -> t result)
  | Vinj of Label.t * t
  | Vprod of t Label.Map.t
  | Vlist of t List.t
  | Vdist of { dist : Prim.Dist.t; arg : t }

and 'a result = { trace : t Trace.t; res : 'a; weight : float }
