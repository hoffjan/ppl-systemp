open Core
include String

let map_of_lists ls xs ~err =
  match List.zip ls xs with
  | Ok alist -> begin match Map.of_alist alist with `Ok lmap -> lmap | `Duplicate_key _ -> err () end
  | Unequal_lengths -> err ()

let of_int = Int.to_string
