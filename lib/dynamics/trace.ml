open Core

type t = Value.t String.Map.t

exception Duplicate_key of string

let to_list trace = Map.to_alist trace
let of_list l = match String.Map.of_alist l with `Ok x -> x | `Duplicate_key key -> raise (Duplicate_key key)
let of_map x = x
let to_map x = x

(* every address should be mapped to a list of values
   when printing, converting the list is flattened and addresses are made unique by adding `1 `2 etc.
   if this flattening results in a name conflict then raise an exception
*)
