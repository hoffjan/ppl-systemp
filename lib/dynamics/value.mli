open Syntax

type t = Vconst of Exp.const | Vlam of (t -> t) | Vinj of Label.t * t | Vprod of t Label.Map.t | Vlist of t List.t
