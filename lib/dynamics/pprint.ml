open Core
open Format
open Syntax

let print_constant e =
  match e with Exp.Cint n -> print_int n | Exp.Cfloat q -> print_float q | Exp.Cstring s -> print_string s

let print_label l =
  let str = Label.to_string l in
  let str =
    match String.lsplit2 ~on:'_' str with
    | Some (prefix, suffix) -> begin match Int.of_string_opt suffix with Some _ -> prefix | None -> str end
    | None -> str
  in
  print_string str

let rec print_value v =
  let open Eval in
  match v with
  | Vconst c -> print_constant c
  | Vlam _ -> print_string "<fun>"
  | Vinj (l, v) ->
      open_box 1;
      print_label l;
      print_space ();
      print_value v;
      close_box ()
  | Vprod vmap ->
      let alist = Map.to_alist vmap in
      let pp_sep _ () =
        print_string ",";
        print_space ()
      in
      let f _ (key, data) =
        open_hbox ();
        print_label key;
        print_space ();
        print_string "=";
        print_space ();
        print_value data;
        close_box ()
      in
      open_hvbox 1;
      print_string "{";
      pp_print_list ~pp_sep f std_formatter alist;
      print_space ();
      print_string "}";
      close_box ()
  | Vlist vals ->
      let pp_sep _ () =
        print_string ",";
        print_space ()
      in
      open_box 1;
      print_string "[";
      pp_print_list ~pp_sep (fun _ v -> print_value v) std_formatter vals;
      print_string "]";
      close_box ()

let print_value v =
  let () = print_value v in
  print_string "\n"
