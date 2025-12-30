open Core
(** Labels and label maps for labeled sums and products *)

type t

include Comparable.S with type t := t
include Sexpable.S with type t := t

val to_string : t -> string
val of_string : string -> t
val of_int : int -> t

module Map : Core.Map.S_binable with type Key.t = t with type Key.comparator_witness = comparator_witness

val map_of_lists : t list -> 'a list -> err:(unit -> 'a Map.t) -> 'a Map.t
