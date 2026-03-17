open Core
open Format
open Syntax

let print_constant e =
  match e with
  | Exp.Cint n -> print_int n
  | Exp.Cfloat q -> print_float q
  | Exp.Cstring s -> print_string ("\"" ^ s ^ "\"")

let print_label l =
  let str = Label.to_string l in
  let str =
    match String.lsplit2 ~on:'_' str with
    | Some (prefix, suffix) -> begin match Int.of_string_opt suffix with Some _ -> prefix | None -> str end
    | None -> str
  in
  print_string str

let print_dist d =
  let open Syntax.Prim.Dist in
  match d with
  | Dbinomial -> print_string "binomial"
  | Dbernoulli -> print_string "bernoulli"
  | Dcategorical -> print_string "categorical"
  | Duniform_int -> print_string "uniform_int"
  | Dnormal -> print_string "normal"
  | Duniform -> print_string "uniform"
  | Dexponential -> print_string "exponential"
  | Dbeta -> print_string "beta"
  | Dgamma -> print_string "gamma"

let rec print_value v =
  let open Value in
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
  | Vdist { dist; arg } ->
      open_box 1;
      print_dist dist;
      print_space ();
      print_value arg;
      close_box ()

let print_trace trace =
  let trace = Trace.to_list trace in
  let pp_sep _ () =
    print_string ",";
    print_space ()
  in
  let f _ (addr, value) =
    open_hbox ();
    print_string addr;
    print_space ();
    print_string "=";
    print_space ();
    print_value value;
    close_box ()
  in
  open_box 1;
  print_string "{";
  pp_print_list ~pp_sep f std_formatter trace;
  print_string "}";
  close_box ()

let print_result ?(weight = true) ?(trace = true) arg =
  let weight_arg = weight in
  let trace_arg = trace in
  let print_field field print_arg =
    open_hbox ();
    print_string field;
    print_space ();
    print_arg ();
    close_box ()
  in
  let print_sep () = print_space () in
  let open Value in
  let { res; trace; weight } = arg in
  open_vbox 1;
  print_string "Result";
  print_sep ();
  (match res with
  | None -> print_string "Malformed trace"
  | Some res -> print_field "Value: " (fun () -> print_value res));
  if weight_arg then (
    print_sep ();
    print_field "Weight:" (fun () -> print_float weight));
  if trace_arg then (
    print_sep ();
    print_field "Trace: " (fun () -> print_trace trace));
  print_sep ();
  print_string "\n";
  close_box ()

let print_value v =
  let () = print_value v in
  print_string "\n"
