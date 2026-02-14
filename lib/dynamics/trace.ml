open Core

type 'a t = 'a String.Map.t

exception Duplicate_key of string

let to_list trace = Map.to_alist trace
let of_list l = match String.Map.of_alist l with `Ok x -> x | `Duplicate_key key -> raise (Duplicate_key key)
let of_map x = x
let to_map x = x
let empty = String.Map.empty
let lookup = Map.find
let map = Map.map
let length = Map.length

let diff t1 t2 =
  let f ~key ~data acc = match data with `Left _ | `Right _ -> key :: acc | `Both _ -> acc in
  Map.fold2 t1 t2 ~init:[] ~f

let merge t1 t2 =
  let f ~key = function `Left v | `Right v -> Some v | `Both _ -> raise (Duplicate_key key) in
  Map.merge t1 t2 ~f

(* every address should be mapped to a list of values
   when printing, converting the list is flattened and addresses are made unique by adding `1 `2 etc.
   if this flattening results in a name conflict then raise an exceptionM
*)
