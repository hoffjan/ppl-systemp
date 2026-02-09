type t

exception Duplicate_key of string

val to_list : t -> (string * Value.t) list
val of_list : (string * Value.t) list -> t
val to_map : t -> Value.t Core.String.Map.t
val of_map : Value.t Core.String.Map.t -> t
