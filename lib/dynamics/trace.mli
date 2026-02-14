type 'a t

exception Duplicate_key of string

val to_list : 'a t -> (string * 'a) list
val of_list : (string * 'a) list -> 'a t
val to_map : 'a t -> 'a Core.String.Map.t
val of_map : 'a Core.String.Map.t -> 'a t
val merge : 'a t -> 'a t -> 'a t
val empty : 'a t
val lookup : 'a t -> string -> 'a option
val map : 'a t -> f:('a -> 'b) -> 'b t
